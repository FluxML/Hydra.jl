mask(n) = vect(ntuple(_ -> true, n)...)

lane1(xs::AbstractVec) = xs[1]
lane1(x) = x

macro spmd(n, ex)
  :(let
      mask = vect(ntuple(_ -> true, $(esc(n)))...)
      lane1(spmd(mask, () -> $(esc(ex))))
  end)
end

lanewidth() = 1
lane() = 1

spmd(mask, ::typeof(lanewidth)) = length(mask)
spmd(mask, ::typeof(lane)) = vect(ntuple(identity, length(mask))...)

lanesum(x) = x
spmd(mask, ::typeof(lanesum), x) = x
spmd(mask, ::typeof(lanesum), x::AbstractVec) = sum(x)
