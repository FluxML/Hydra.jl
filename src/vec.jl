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

# summary(::Vec) = "Vec"

Base.getindex(v::Vec, i) = v.data[i].value

vect(xs::T...) where { T } = Vec(VecElement.(xs))
vect(xs...) = vect(promote(xs...)...)

unvect(x::Vec{T,N}) where {T,N} = getfield.(x.data, :value)
unvect(x) = x
scalartovect(x, n) = vect(repeat([x], n)...)
isvect(x::Vec{T,N}) where {T,N} = true
isvect(x) = false

datatype(::Type{Vec{T,N}}) where {T,N} = T
datatype(x) = x

similar(::Type{Vec{T,N}}) where {T,N} = vect(zeros(T, N)...)

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
convert(::Type{Vec{T, N}}, x::T) where {N, T} = scalartovect(x,N)
convert(::Type{Vec{T, N}}, x::S) where {N, T, S} = scalartovect(T(x),N)
convert(::Type{Vec{T, N}}, x::Vec{S, N}) where {S,T,N} = Vec(VecElement.(T.(unvect(x))))
promote_rule(::Type{Vec{S,N}}, ::Type{T}) where {S,T,N} = Vec{promote_type(T,S),N}
