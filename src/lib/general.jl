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
  map(zip(__mask__, iter)) do x
    if x[1]
      iterate(x[2])
    else
      nothing
    end
  end |> splat_and_vect
end

@spmd function iterate(iter::AbstractVec{T,N}, state::AbstractVec{S,N}) where {T, S, N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(__mask__, iter, state)) do x
    if x[1]
      iterate(x[2], x[3])
    else
      nothing
    end
  end |> splat_and_vect
end

function masked_getfield(mask, xs::Vec{T,N}, names) where {T,N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(mask, xs, names)) do (m, x, name)
    if m
      getfield(x, name)
    else
      nothing
    end
  end |> splat_and_vect
end

@spmd function getfield(x::AbstractVec{T,N}, name::S) where {S,T,N}
  vect(getfield.(x,name)...)
end
@spmd getfield(x::AbstractVec{T,N}, name::AbstractVec{S,N}) where {S,T,N} = vect(getfield.(x, name)...)

@spmd getfield(x::Vec{T,N}, name::S) where {S,T,N} = masked_getfield(__mask__, x, Vec{S,N}(name))
@spmd getfield(x::Vec{T,N}, name::Vec{S,N}) where {S,T,N} = masked_getfield(__mask__, x, name)
