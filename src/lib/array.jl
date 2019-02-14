struct VecArray{C,A,N} <: AbstractVec{C,N}
  batch::A
  sizes
end

Base.show(io::IO, x::VecArray) = println(io, "VecArray")

data(xs::VecArray) = xs.batch

VecArray(batch::C...) where {C <: AbstractArray} = let
  batched, sizes = tobatch(batch...)
  VecArray{C, typeof(batched), length(batch)}(batched, sizes)
end

function tobatch(xs::A...) where {T <: ScalarTypes,N,A <: AbstractArray{T,N}}
  if all(y->y==size(xs[1]), size.(xs))
    res = cat(xs..., dims=N+1)
    return (res, size.(xs))
  end
  dims = max.(size.(xs)...)
  batch = zeros(T, dims..., length(xs)) #TODO: this feels wrooooong
  for (i,x) in enumerate(xs)
    setindex!(batch, x, axes(x)..., i)
  end
  (batch, size.(xs))
end

function select(conds::Mask{N}, first_vals::VecArray{C,T,N}, second_vals::VecArray{C,T,N}) where {C,T,N}
  result = zero(first_vals.batch)
  for (i,cond) in enumerate(conds)
    result[axes(result)[1:end-1]..., i] = cond ?
      first_vals.batch[axes(result)[1:end-1]..., i] :
      second_vals.batch[axes(result)[1:end-1]..., i]
  end
  return VecArray{C,T,N}(result, first_vals.sizes)
end

function select(conds::Mask{N}, first_vals::VecArray{C,T,N}, second_vals::C) where {C <: AbstractArray,T,N}
  result = zero(first_vals.batch)
  for (i,cond) in enumerate(conds)
    result[axes(result)[1:end-1]..., i] = cond ?
      first_vals.batch[axes(result)[1:end-1]..., i] :
      second_vals
  end
  return VecArray{C,T,N}(result, first_vals.sizes)
end

function select(conds::Mask{N}, first_vals::C, second_vals::VecArray{C,T,N}) where {C <: AbstractArray,T,N}
  result = zero(second_vals.batch)
  for (i,cond) in enumerate(conds)
    result[axes(result)[1:end-1]..., i] = cond ?
      first_vals :
      second_vals.batch[axes(result)[1:end-1]..., i]
  end
  return VecArray{C,T,N}(result, first_vals.sizes)
end

vect(xs::AbstractArray{T}...) where {T <: ScalarTypes} = VecArray(xs...)
VecArrayOrArrayOrVal{C,A,N} = Union{VecArray{C,A,N}, C, ScalarTypes}
sizes(xs) = xs[findfirst(x->x isa VecArray, xs)].sizes
# element wise ops
for op in :[+, -, &, |, !,
            ==, !=, >, >=, <, <=].args
    @eval begin
        @spmd $op(xs::VecArrayOrArrayOrVal{C,A,N}...) where {C,A,N} =
          VecArray{C,A,N}($op.(data.(xs)...), sizes(xs))
    end
end

@spmd length(x::VecArray{C,A,N}) where {C,A,N} = vect(map(sum, S)...)
@spmd size(x::VecArray{C,A,N}) where {C,A,N} = vect(S...)
#
# VecVectorOrVector{C <: AbstractArray{T,1}, A <: AbstractArray{T,2}, N, S} = Union{VecArray{C,A,N,S}, C}
# @spmd (*)(xs::VecVectorOrVector{C,A,N,S}...) where {C,A,N,S} =
#   VecArray{C,A,N,map(x->,S)}((*)(data.(xs)...))

@spmd (*)(xs::VecArray{C1,A,N}, ys::C2) where {T,C1<:AbstractVector,A,N,C2<:AbstractMatrix{T}} = let
  create_new_sizes(s1,s2) = (s1[1], length(s2) == 1 ? () : s2[2])
  result = (*)(data.((xs,ys))...)
  sizes = map(s->create_new_sizes(s,size(ys)),xs.sizes)
  VecArray{C2,typeof(result),N}(result, sizes)
end
@spmd (*)(xs::C1, ys::VecArray{C2,A,N}) where {T,C1<:AbstractMatrix{T},A,N,C2<:AbstractVector} = let
  create_new_sizes(s1,s2) = (s1[1], length(s2) == 1 ? () : s2[2])
  result = (*)(data.((xs,ys))...)
  sizes = map(s->create_new_sizes(size(xs),s),ys.sizes)
  VecArray{C2,typeof(result),N}(result, sizes)
end

@spmd Base.Broadcast.broadcasted(f,xs::VecArrayOrArrayOrVal...) = Base.Broadcast.broadcasted(f,data.(xs)...)
@spmd Base.Broadcast.materialize(x) = Base.Broadcast.materialize(x)

type_minus_one(xs::C) where {T,N, C<:AbstractArray{T,N}} = AbstractArray{T,N-1}

@spmd reshape(xs::VecArray{C,A,N}, dims) where {C,A,N} = begin
  res = reshape(data(xs), dims..., N)
  VecArray{type_minus_one(res),typeof(res),N}(res, repeat([dims...], N))
end

@spmd reshape(xs::VecArray{C,A,N}, dims...) where {C,A,N} = begin
  res = reshape(data(xs), dims..., N)
  VecArray{type_minus_one(res),typeof(res),N}(res, repeat([dims...], N))
end
