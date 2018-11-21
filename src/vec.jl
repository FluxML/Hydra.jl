# Unwrap vecs into tuples
data(x) = x
abstract type AbstractVec{T,N} end

data(x::AbstractVec) = error("`data` not implemented for $(typeof(x))")

Base.length(xs::AbstractVec{T,N}) where {T,N} = N
Base.getindex(xs::AbstractVec, i::Integer) = data(xs)[i]

function Base.show(io::IO, v::AbstractVec)
  print(io, summary(v), "{")
  join(io, sprint.(show, v), ", ")
  print(io, "}")
end

Base.iterate(v::AbstractVec, i = 1) =
  i > length(v) ? nothing : (v[i], i+1)

const Mask{N} = AbstractVec{Bool,N}

struct Vec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,T}
  Vec{T,N}(data::NTuple{N,T}) where {T,N} = new(data)
end

data(vec::Vec) = vec.data

Vec(xs::Tuple) = Vec{Union{map(typeof,xs)...},length(xs)}(xs)

vect(xs...) = Vec(xs)

Base.summary(::Vec{T}) where T = "Vec{$T}"

struct HoleyVec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,Union{Nothing,VecElement{T}}}
  HoleyVec{T,N}(data::NTuple{N,Union{Nothing, VecElement{T}}}) where {T,N} = new(data)
end

HoleyVec(xs::NTuple{N,Union{Nothing,VecElement{T}}}) where {T,N} = HoleyVec{T,N}(xs)

vect(xs::Union{Nothing, T}...) where {T} = HoleyVec(map(x -> if x isa Nothing x else VecElement(x) end, xs))

data(xs::HoleyVec) = map(x -> if x isa Nothing x else x.value end, xs.data)

struct BitVec{N,T<:Unsigned} <: AbstractVec{Bool,N}
  data::T
end

BitVec{N}(data::T) where {T<:Unsigned,N} = BitVec{N,T}(data)

Base.summary(::BitVec) = "BitVec"

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

import Base.(:)
# function (:)(start::SVec{T,N}, step::SVec{T,N}, stop::SVec{T,N}) where {T <: ScalarTypes,N}
#     vect(map(x -> x[1]:x[2]:x[3], zip(start,step,stop)))
# end
(:)(start::AbstractVec{T,N}, step::AbstractVec{T,N}, stop::AbstractVec{T,N}) where {T,N} = vect(map(x -> x[1]:x[2]:x[3], zip(start,step,stop))...)
(:)(start::AbstractVec{T,N}, stop::AbstractVec{T,N}) where {T,N} = (:)(start, AbstractVec{T,N}(1), stop)

(:)(start::T, stop::AbstractVec{S,N}) where {T,S,N} = (:)(promote(start, stop)...)
(:)(start::AbstractVec{T,N}, stop::S) where {T,S,N} = (:)(promote(start, stop)...)

spmd(mask, ::typeof(:), start, step, stop) = start:step:stop
spmd(mask, ::typeof(:), start, stop) = start:stop

#HACK
spmd(mask, ::typeof(===), x, y) = vect((x .=== y)...)
