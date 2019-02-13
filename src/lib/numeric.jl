import SIMD
using SIMD: ScalarTypes

(V::Type{<:AbstractVec{T,N}})(x::T) where {T <: ScalarTypes, N} = V(ntuple(_ -> x, N))
datatype(x::SIMD.Vec{N,T}) where {N,T} = T

# Scalar vector type
# Essentially equivalent to what SIMD.jl provides; we only reproduce it
# so that it fits into our `AbstractVec` framework.

struct SVec{T,N} <: AbstractVec{T,N}
  data::NTuple{N,VecElement{T}}
  SVec{T,N}(data::NTuple{N,VecElement{T}}) where {T,N} = new(data)
end

SVec(xs::NTuple{N,VecElement{T}}) where {T,N} = SVec{T,N}(xs)
SVec(xs::NTuple{N,T}) where {T,N} = SVec{T,N}(map(VecElement, xs))

SVec{T,N}(xs::NTuple{N,T}) where {T,N} = SVec{T,N}(map(VecElement, xs))

# SIMD vectors are buggy – https://github.com/JuliaLang/julia/issues/30056
# Flip this to test programs without using them.
vect(xs::T...) where {T <: ScalarTypes} = SVec(xs)
vect(xs::VecElement{T}...) where {T} = SVec(xs)
# vect(xs::T...) where {T} = Vec(xs)

data(xs::SVec) = getfield.(xs.data, :value)

Base.summary(::SVec) = "SVec"

Base.convert(::Type{SIMD.Vec}, x::SVec{T,N}) where {T,N} =
  SIMD.Vec{N,T}(x.data)

Base.convert(::Type{SVec}, x::SIMD.Vec{N,T}) where {T,N} =
  SVec{T,N}(x.elts)

simd_vec(x::SVec) = convert(SIMD.Vec, x)
simd_vec(x) = x
svec(x::SIMD.Vec) = convert(SVec, x)
svec(x) = x

Base.sum(x::SVec) = sum(convert(SIMD.Vec, x))

SVecOrVal{T} = Union{SVec{T},T}
VecOrVal{T} = Union{Vec{T},T}
vecconvert(T, x::SIMD.Vec) = convert(SIMD.Vec{length(x),T}, x)
vecconvert(T, x) = convert(T, x)

vecpromote(xs...) = map(x -> vecconvert(promote_type(map(datatype, xs)...), x), xs)

# Arithmetic operations (forward to SIMD.jl)

for op in :[+, -, *, /, div, rem, &, |, !,
            ==, !=, >, >=, <, <=].args
    @eval begin
        @spmd $op(xs::SVecOrVal{T}...) where T <: ScalarTypes =
          svec($op(map(simd_vec, xs)...))
        # TODO: remove this when we can compile the usual promotion mechanisms
        @spmd $op(xs::SVecOrVal{<:ScalarTypes}...) =
          svec($op(vecpromote(map(simd_vec, xs)...)...))
    end
end

# TODO figure out what we can remove

promote_rule(::Type{SVec{S,N}}, ::Type{T}) where {S,T<:ScalarTypes,N} = SVec{promote_type(T,S),N}
promote_rule(::Type{SVec{S,N}}, ::Type{SVec{T,N}}) where {S,T,N} = SVec{promote_type(T,S),N}
promote_rule(::Type{Vec{S,N}}, ::Type{SVec{T,N}}) where {S,T,N} = Vec{promote_type(T,S),N}

convert(::Type{SVec{T,N}}, x) where {T,N} = SVec{T,N}(x)
convert(::Type{SVec{T,N}}, xs::SVec{S,N}) where {T,S,N} = vect(T.(data(xs))...)

convert(::Type{Vec{T,N}}, xs::SVec{S,N}) where {T,S,N} = Vec(map(x -> VecElement(T(x.value)), xs.data))

import Base.(:)
(:)(start::SVec{T,N}, step::SVec{T,N}, stop::SVec{T,N}) where {T,N} = Vec(tuple(map(x -> x[1]:x[2]:x[3], zip(start,step,stop))...))
(:)(start::SVec{T,N}, stop::SVec{T,N}) where {T,N} = (:)(start, SVec{T,N}(1), stop)

(:)(start::T, stop::AbstractVec{S,N}) where {T,S,N} = (:)(promote(start, stop)...)
(:)(start::AbstractVec{T,N}, stop::S) where {T,S,N} = (:)(promote(start, stop)...)

@spmd (:)(start, step, stop) = start:step:stop
@spmd (:)(start, stop) = start:stop

using Base: not_int
@spmd not_int(xs) = spmd(__mask__, ~, xs)
