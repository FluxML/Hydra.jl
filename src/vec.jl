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

function Base.show(io::IO, v::AbstractVec)
  print(io, summary(v), "{")
  tostring(x) = if x isa Nothing "nothing" else x end
  join(io, tostring.(v), ", ")
  print(io, "}")
end

Base.iterate(v::AbstractVec, i = 1) =
  i > length(v) ? nothing : (v[i], i+1)

struct Vec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,VecElement{T}}
  Vec{T,N}(data::NTuple{N,VecElement{T}}) where {T,N} = new(data)
end

struct HoleyVec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,Union{Nothing,VecElement{T}}}
  HoleyVec{T,N}(data::NTuple{N,Union{Nothing, VecElement{T}}}) where {T,N} = new(data)
end

Vec(xs::NTuple{N,VecElement{T}}) where {T,N} = Vec{T,N}(xs)
HoleyVec(xs::NTuple{N,Union{Nothing,VecElement{T}}}) where {T,N} = HoleyVec{T,N}(xs)

# summary(::Vec) = "Vec"

Base.getindex(v::Vec, i) = v.data[i].value
Base.getindex(v::HoleyVec, i) = if v.data[i] isa Nothing v.data[i] else v.data[i].value end

# vect(xs::AbstractRange...) =

vect(xs::Union{Nothing, T}...) where {T} = HoleyVec(map(x -> if x isa Nothing x else VecElement(x) end, xs))
vect(xs::T...) where {T} = Vec(VecElement.(xs))
vect(xs...) = vect(promote(xs...)...)

Vec{T,N}(x::T) where {T,N} = Vec(ntuple(_ -> VecElement(x), N))
Vec{T,N}(x) where {T,N} = Vec{T,N}(convert(T, x))

data(xs::Vec) = getfield.(xs.data, :value)
data(xs::HoleyVec) = map(x -> if x isa Nothing x else x.data.value end, xs)

struct BitVec{N,T<:Unsigned} <: AbstractVec{Bool,N}
  data::T
end

BitVec{N}(data::T) where {T<:Unsigned,N} = BitVec{N,T}(data)

# summary(::BitVec) = "BitVec"

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
convert(::Type{Vec{T,N}}, x) where {T,N} = Vec{T,N}(x)
convert(::Type{Vec{T,N}}, xs::Vec{S,N}) where {T,S,N} = vect(T.(data(xs))...)

convert(::Type{HoleyVec{T,N}}, xs::Vec{S,N}) where {T,S,N} = HoleyVec(map(x -> VecElement(T(x.value)), xs.data))
convert(::Type{HoleyVec{T,N}}, xs::HoleyVec{S,N}) where {T,S,N} = HoleyVec(map(x->if x isa Nothing x else VecElement(T(x.value)) end, xs.data))

promote_rule(::Type{Vec{S,N}}, ::Type{T}) where {S,T<:ScalarTypes,N} = Vec{promote_type(T,S),N}
promote_rule(::Type{Vec{S,N}}, ::Type{Vec{T,N}}) where {S,T,N} = Vec{promote_type(T,S),N}
promote_rule(::Type{HoleyVec{S,N}}, ::Type{Vec{T,N}}) where {S,T,N} = HoleyVec{promote_type(T,S),N}
promote_rule(::Type{HoleyVec{S,N}}, ::Type{HoleyVec{T,N}}) where {S,T,N} = HoleyVec{promote_type(T,S),N}

import Base.(:)
# function (:)(start::Vec{T,N}, step::Vec{T,N}, stop::Vec{T,N}) where {T <: ScalarTypes,N}
#     vect(map(x -> x[1]:x[2]:x[3], zip(start,step,stop)))
# end
(:)(start::Vec{T,N}, step::Vec{T,N}, stop::Vec{T,N}) where {T,N} = vect(map(x -> x[1]:x[2]:x[3], zip(start,step,stop))...)
(:)(start::Vec{T,N}, stop::Vec{T,N}) where {T,N} = (:)(start, Vec{T,N}(1), stop)

(:)(start::T, stop::Vec{S,N}) where {T,S,N} = (:)(promote(start, stop)...)
(:)(start::Vec{T,N}, stop::S) where {T,S,N} = (:)(promote(start, stop)...)

spmd(mask, ::typeof(:), start, step, stop) = start:step:stop
spmd(mask, ::typeof(:), start, stop) = start:stop

#HACK
spmd(mask, ::typeof(===), x, y) = vect((x .=== y)...)
