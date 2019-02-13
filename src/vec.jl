# Unwrap vecs into tuples
data(x) = x
abstract type AbstractVec{T,N} end

datatype(x) = typeof(x)
datatype(::AbstractVec{T}) where T = T

data(x::AbstractVec) = error("`data` not implemented for $(typeof(x))")

Base.length(xs::AbstractVec{T,N}) where {T,N} = N
Base.getindex(xs::AbstractVec, i::Integer) = data(xs)[i]

Base.getindex(xs::AbstractVec, is::AbstractVec) = vect(map(i -> xs[i], is)...)

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
vect(xs::Nothing...) = Vec(xs)

Base.summary(::Vec{T}) where T = "Vec{$T}"

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

# Define some mask operations directly

import Base: &, |, ~

(a::Mask{N} & b::Mask{N}) where N = spmd(mask(N), &, a, b)
(a::Mask{N} | b::Mask{N}) where N = spmd(mask(N), |, a, b)
(~a::Mask{N}) where N = spmd(mask(N), ~, a)

Base.sum(x::AbstractVec) = sum(data(x))

# TODO use smaller words where possible
# vect(xs::Bool...) = BitVec{length(xs)}(bitpack(UInt64, xs))

import Base.convert, Base.promote_rule

convert(::Type{Vec{T,N}}, xs::Vec{S,N}) where {T,S,N} = Vec(convert.(T, xs.data))

promote_rule(::Type{Vec{S,N}}, ::Type{Vec{T,N}}) where {S,T,N} = Vec{promote_type(T,S),N}

#HACK
spmd(mask, ::typeof(===), x, y) = vect((x .=== y)...)
