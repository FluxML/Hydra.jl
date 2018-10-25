# Unwrap vecs into tuples
data(x) = x

abstract type AbstractVec{T,N} end

data(x::AbstractVec) = error("`data` not implemented for $(typeof(x))")

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
  Vec{T,N}(data::NTuple{N,VecElement{T}}) where {T,N} = new(data)
end

# summary(::Vec) = "Vec"

Base.getindex(v::Vec, i) = v.data[i].value

vect(xs::T...) where { T } = Vec(VecElement.(xs))
vect(xs...) = vect(promote(xs...)...)

Vec{T,N}(x::T) where {T,N} = Vec(ntuple(_ -> VecElement(x), N))
Vec{T,N}(x) where {T,N} = Vec{T,N}(convert(T, x))

data(x::Vec) = getfield.(x.data, :value)

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
convert(::Type{Vec{T, N}}, x) where {N, T} = Vec{T,N}(x)
convert(::Type{Vec{T, N}}, x::Vec{S, N}) where {S,T,N} = Vec(VecElement.(T.(data(x))))
promote_rule(::Type{Vec{S,N}}, ::Type{T}) where {S,T,N} = Vec{promote_type(T,S),N}
