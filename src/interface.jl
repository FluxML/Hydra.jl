macro spmd(n, ex)
  :(let
      mask = vect(ntuple(_ -> true, $(esc(n)))...)
      spmd(mask, () -> $(esc(ex)))
      nothing
  end)
end

lanewidth() = 1
lane() = 1

spmd(mask, ::typeof(lanewidth)) = length(mask)
spmd(mask, ::typeof(lane)) = vect(ntuple(identity, length(mask))...)
