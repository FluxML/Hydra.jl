function spmd(mask, ::typeof(iterate), iter::AbstractVec{T,N}) where {T, N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(mask, iter)) do x
    if x[1]
      iterate(x[2])
    else
      nothing
    end
  end |> splat_and_vect
end

function spmd(mask, ::typeof(iterate), iter::AbstractVec{T,N}, state::SVec{S,N}) where {T, S, N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(mask, iter, state)) do x
    if x[1]
      iterate(x[2], x[3])
    else
      nothing
    end
  end |> splat_and_vect
end

function masked_getfield(mask, xs::HoleyVec{T,N}, names) where {T,N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(mask, xs, names)) do (m, x, name)
    if m
      getfield(x, name)
    else
      nothing
    end
  end |> splat_and_vect
end

function spmd(mask, ::typeof(getfield), x::SVec{T,N}, name::S) where {S,T,N}
  vect(getfield.(x,name)...)
end
spmd(mask, ::typeof(getfield), x::SVec{T,N}, name::SVec{S,N}) where {S,T,N} = vect(getfield.(x, name)...)

spmd(mask, ::typeof(getfield), x::HoleyVec{T,N}, name::S) where {S,T,N} = masked_getfield(mask, x, SVec{S,N}(name))
spmd(mask, ::typeof(getfield), x::HoleyVec{T,N}, name::HoleyVec{S,N}) where {S,T,N} = masked_getfield(mask, x, name)

spmd(mask, ::typeof(println), xs...) where {T,N} = println(data.(xs)...)
spmd(mask, ::typeof(print), xs...) where {T,N} = print(data.(xs)...)
