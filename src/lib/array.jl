struct VecArray{C,A,N} <: AbstractVec{C,N}
  batch::A
  sizes
end

data(xs::VecArray) = xs.batch

VecArray(batch::C...) where {C <: AbstractArray} = let
  batch, sizes = tobatch(batch...)
  VecArray{C, typeof(batch), length(batch)}(batch, sizes)
end

function tobatch(xs::A...) where {T <: ScalarTypes,A <: AbstractArray{T}}
  dims = max.(size.(xs)...)
  batch = similar(xs[1], T, dims..., length(xs)) #TODO: this feels wrooooong
  for (i,x) in enumerate(xs)
    setindex!(batch, x, axes(x)..., i)
  end
  (batch, size.(xs))
end

# function tobatch(xs::A...) where {T <: AbstractArray, A <: AbstractArray{T}}
#   dims = max.(size.(xs)...)
#   batch = similar(xs[1], VecArray, dims...)
#   for (i) in 1:length(batch)
#     getindex_or_nothing(x, i) = try x[i] catch e nothing end
#     xs_or_nothing = getindex_or_nothing.(xs, i)
#     first_x_not_nothing = something(xs_or_nothing...)
#     padded_xs = map(x->x == nothing ? zeros(size(first_x_not_nothing)) : x, xs_or_nothing)
#     batch[i] = vect(padded_xs...)
#   end
#   (batch, size.(xs))
# end

vect(xs::AbstractArray{T}...) where {T <: ScalarTypes} = VecArray(xs...)

# element wise ops
for op in :[+, -, &, |, !,
            ==, !=, >, >=, <, <=].args
    @eval begin
        @spmd $op(xs::VecArray{C,A,N}, ys::C) where {C,A,N} =
          VecArray{C,A,N}($op.(data.((xs,ys))...), xs.sizes)
        @spmd $op(xs::C, ys::VecArray{C,A,N}) where {C,A,N} =
          VecArray{C,A,N}($op.(data.((xs,ys))...), xs.sizes)
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

VecArrayOrArrayOrVal = Union{VecArray, AbstractArray, ScalarTypes}

@spmd Base.Broadcast.broadcasted(f,xs::VecArrayOrArrayOrVal...) = Base.Broadcast.broadcasted(f,data.(xs)...)
@spmd Base.Broadcast.materialize(x) = Base.Broadcast.materialize(x)
