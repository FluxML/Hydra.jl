abstract type AbstractVec{T,N} end

struct Vec{T,N}
  data::NTuple{N,VecElement{T}}
end

vec(xs::T...) where T = Vec(VecElement.(xs))
vec(xs...) = vec(promote(xs...)...)

struct BitVec{N,T<:Unsigned} <: AbstractVec{Bool,N}
  data::T
end

BitVec{N}(data::T) where {T<:Unsigned,N} = BitVec{N,T}(data)

@generated function bitpack(::Type{T}, xs::NTuple{N,Any}) where {T<:Unsigned,N}
  y = zero(T)
  for i = 1:N
    y = :($y | (xs[$i] << $(i-1)))
  end
  return y
end

# TODO use smaller words where possible
vec(xs::Bool...) = BitVec{length(xs)}(bitpack(UInt64, xs))

# vec(1,2,3,4)
# vec(true, false, true, true)

# spmd(::typeof(+), mask, a::Vec, b::Vec) = ...
