abstract type AbstractVec{T,N} end

struct Vec{T,N}
  data::NTuple{N,VecElement{T}}
end


vect(xs::T...) where T <: Number = Vec(VecElement.(xs))
vect(xs...) = vect(promote(xs...)...)

tovect(n, x) = vect(repeat([x], n)...)
isvect(x::Vec{T,N}) where {T,N} = true
isvect(x) = false



function Base.:+(x::Vec{T,N}, y::Vec{T,N}) where {T,N}
  result = []
  for i in range(1,length=N)
    append!(result, [x.data[i].value + y.data[i].value])
  end
  vect(result...)
end
vect(1,2,3) + vect(2,3,4)

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
vect(xs::Bool...) = BitVec{length(xs)}(bitpack(UInt64, xs))

import Base.convert, Base.promote_rule
convert(::Type{Vec{T, N}}, x::T) where {N, T} = tovect(N,x)
promote_rule(::Type{Vec{S,N}}, ::Type{T}) where {S,T,N} = Vec{promote_type(T,S),N}

# vect(1,2,3,4)
# vect(true, false, true, true)

# spmd(::typeof(+), mask, a::Vec, b::Vec) = ...
