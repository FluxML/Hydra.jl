asvec(mask, x::AbstractVec) = x
asvec(mask, x) = vect(ntuple(_ -> x, length(mask))...)

function spmdmap(mask, f, args...)
  vargs = map(x -> asvec(mask, x), args)
  vect(map((m, a...) -> m ? f(a...) : nothing, mask, vargs...)...)
end

@spmd println(xs...) = spmdmap(__mask__, println, xs...)
@spmd print(xs...) = spmdmap(__mask__, print, xs...)
@spmd repr(xs...) = spmdmap(__mask__, repr, xs...)

@spmd function iterate(iter::AbstractVec{T,N}) where {T, N}
  splat_and_vect(xs) = vect(xs...)
  res = map(zip(__mask__, iter)) do x
    if x[1]
      iterate(x[2])
    else
      nothing
    end
  end |> splat_and_vect

  # println(res)
  res
end

@spmd function iterate(iter::AbstractVec{T,N}, state::AbstractVec{S,N}) where {T, S, N}
  splat_and_vect(xs) = vect(xs...)
  res = map(zip(__mask__, iter, state)) do x
    if x[1]
      iterate(x[2], x[3])
    else
      nothing
    end
  end |> splat_and_vect

  # println(res)
  res
end

function put_zero_value(mask, xs::Union{Nothing, T}...) where {T}
  not_nothing_indeces = findall(x->x!=nothing, xs)
  if length(not_nothing_indeces) == 0
    return xs
  end
  map(x->x==nothing ? zero(xs[not_nothing_indeces[1]]) : x,xs)
end

function masked_getfield(mask, xs::Vec{T,N}, names) where {T, N}
  splat_and_vect(xs) = vect(put_zero_value(mask, xs...)...)
  map(zip(mask, xs, names)) do (m, x, name)
    if m
      getfield(x, name)
    else
      nothing
    end
  end |> splat_and_vect
end

function masked_getindex(mask, xs::Vec{T,N}, indices) where {T, N}
  splat_and_vect(xs) = vect(put_zero_value(mask, xs...)...)
  map(zip(mask, xs, indices)) do (m, x, index)
    if m
      getindex(x, index)
    else
      nothing
    end
  end |> splat_and_vect
end

@spmd function getfield(x::AbstractVec{T,N}, name::S) where {S,T,N}
  vect(getfield.(x,name)...)
end
@spmd getfield(x::AbstractVec{T,N}, name::AbstractVec{S,N}) where {S,T,N} = vect(getfield.(x, name)...)

@spmd getfield(x::Vec{T,N}, name::S) where {S,T,N} = begin println(name); masked_getfield(__mask__, x, Vec{S,N}(name)) end
@spmd getfield(x::Vec{T,N}, name::Vec{S,N}) where {S,T,N} = masked_getfield(__mask__, x, name)
@spmd length(x::Vec{T,N}) where {T <: AbstractArray, N} = vect(length.(data(x))...)
@spmd getindex(x::Vec{T,N}, index::S) where {T <: AbstractArray, N, S} = masked_getindex(__mask__, x, Vec{S,N}(index))
