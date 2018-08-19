abstract type AbstractVec{T,N} end

Base.length(xs::AbstractVec{T,N}) where {T,N} = N

function Base.show(io::IO, v::AbstractVec)
  print(io, summary(v), "{")
  join(io, v, ", ")
  print(io, "}")
end

Base.iterate(v::AbstractVec, i = 1) =
  i > length(v) ? nothing : (v[i], i+1)

struct Vec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,VecElement{T}}
end

summary(::Vec) = "Vec"

Base.getindex(v::Vec, i) = v.data[i].value

vec(xs::T...) where T = Vec(VecElement.(xs))
vec(xs...) = vec(promote(xs...)...)

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
vec(xs::Bool...) = BitVec{length(xs)}(bitpack(UInt64, xs))

# vec(1,2,3,4)
# vec(true, false, true, true)

# spmd(::typeof(+), mask, a::Vec, b::Vec) = ...
