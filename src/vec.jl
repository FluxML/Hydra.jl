# Unwrap vecs into tuples
const BoolTypes = Union{Bool}
const IntTypes = Union{Int8, Int16, Int32, Int64, Int128}
const UIntTypes = Union{UInt8, UInt16, UInt32, UInt64, UInt128}
const IntegerTypes = Union{BoolTypes, IntTypes, UIntTypes}
const FloatingTypes = Union{Float16, Float32, Float64}
const ScalarTypes = Union{IntegerTypes, FloatingTypes}

data(x) = x
abstract type AbstractVec{T,N} end

data(x::AbstractVec) = error("`data` not implemented for $(typeof(x))")

Base.length(xs::AbstractVec{T,N}) where {T,N} = N
Base.getindex(xs::AbstractVec, i::Integer) = data(xs)[i]

function Base.show(io::IO, v::AbstractVec)
  print(io, summary(v), "{")
  tostring(x) = if x isa Nothing "nothing" else x end
  join(io, tostring.(v), ", ")
  print(io, "}")
end

Base.iterate(v::AbstractVec, i = 1) =
  i > length(v) ? nothing : (v[i], i+1)


struct Vec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,T}
  Vec{T,N}(data::NTuple{N,T}) where {T,N} = new(data)
end

data(vec::Vec) = vec.data

Vec(xs::Tuple) = Vec{Union{map(typeof,xs)...},length(xs)}(xs)

vect(xs...) = Vec(xs)

Base.summary(::Vec{T}) where T = "Vec{$T}"

struct SVec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,VecElement{T}}
  SVec{T,N}(data::NTuple{N,VecElement{T}}) where {T,N} = new(data)
end

SVec(xs::NTuple{N,VecElement{T}}) where {T,N} = SVec{T,N}(xs)
SVec(xs::NTuple{N,T}) where {T,N} = SVec{T,N}(map(VecElement, xs))

SVec{T,N}(x::T) where {T,N} = SVec(ntuple(_ -> VecElement(x), N))
SVec{T,N}(x) where {T,N} = SVec{T,N}(convert(T, x))

vect(xs::T...) where {T} = SVec(xs)

data(xs::SVec) = getfield.(xs.data, :value)

Base.summary(::SVec) = "SVec"

struct HoleyVec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,Union{Nothing,VecElement{T}}}
  HoleyVec{T,N}(data::NTuple{N,Union{Nothing, VecElement{T}}}) where {T,N} = new(data)
end

HoleyVec(xs::NTuple{N,Union{Nothing,VecElement{T}}}) where {T,N} = HoleyVec{T,N}(xs)

vect(xs::Union{Nothing, T}...) where {T} = HoleyVec(map(x -> if x isa Nothing x else VecElement(x) end, xs))

data(xs::HoleyVec) = map(x -> if x isa Nothing x else x.data.value end, xs)

struct BitVec{N,T<:Unsigned} <: AbstractVec{Bool,N}
  data::T
end

BitVec{N}(data::T) where {T<:Unsigned,N} = BitVec{N,T}(data)

summary(::BitVec) = "BitVec"

Base.getindex(v::BitVec, i) = Bool(v.data >> (i-1) & 0x01)

@generated function bitpack(::Type{T}, xs::NTuple{N,Any}) where {T<:Unsigned,N}
  y = zero(T)
  for i = 1:N
    y = :($y | (xs[$i] << $(i-1)))
  end
  return y
end

# TODO use smaller words where possible
# vect(xs::Bool...) = BitVec{length(xs)}(bitpack(UInt64, xs))

import Base.convert, Base.promote_rule
convert(::Type{SVec{T,N}}, x) where {T,N} = SVec{T,N}(x)
convert(::Type{SVec{T,N}}, xs::SVec{S,N}) where {T,S,N} = vect(T.(data(xs))...)

convert(::Type{HoleyVec{T,N}}, xs::SVec{S,N}) where {T,S,N} = HoleyVec(map(x -> VecElement(T(x.value)), xs.data))
convert(::Type{HoleyVec{T,N}}, xs::HoleyVec{S,N}) where {T,S,N} = HoleyVec(map(x->if x isa Nothing x else VecElement(T(x.value)) end, xs.data))

promote_rule(::Type{SVec{S,N}}, ::Type{T}) where {S,T<:ScalarTypes,N} = SVec{promote_type(T,S),N}
promote_rule(::Type{SVec{S,N}}, ::Type{SVec{T,N}}) where {S,T,N} = SVec{promote_type(T,S),N}
promote_rule(::Type{HoleyVec{S,N}}, ::Type{SVec{T,N}}) where {S,T,N} = HoleyVec{promote_type(T,S),N}
promote_rule(::Type{HoleyVec{S,N}}, ::Type{HoleyVec{T,N}}) where {S,T,N} = HoleyVec{promote_type(T,S),N}

import Base.(:)
# function (:)(start::SVec{T,N}, step::SVec{T,N}, stop::SVec{T,N}) where {T <: ScalarTypes,N}
#     vect(map(x -> x[1]:x[2]:x[3], zip(start,step,stop)))
# end
(:)(start::SVec{T,N}, step::SVec{T,N}, stop::SVec{T,N}) where {T,N} = vect(map(x -> x[1]:x[2]:x[3], zip(start,step,stop))...)
(:)(start::SVec{T,N}, stop::SVec{T,N}) where {T,N} = (:)(start, SVec{T,N}(1), stop)

(:)(start::T, stop::SVec{S,N}) where {T,S,N} = (:)(promote(start, stop)...)
(:)(start::SVec{T,N}, stop::S) where {T,S,N} = (:)(promote(start, stop)...)

spmd(mask, ::typeof(:), start, step, stop) = start:step:stop
spmd(mask, ::typeof(:), start, stop) = start:stop

#HACK
spmd(mask, ::typeof(===), x, y) = vect((x .=== y)...)
