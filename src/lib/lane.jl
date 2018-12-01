lane() = 1

@spmd lane() = vect(ntuple(identity, length(__mask__))...)

lanewidth() = 1

@spmd lanewidth() = length(__mask__)

lanesum(x) = lanewidth() * x

@spmd lanesum(x::AbstractVec) = sum(x)
@spmd lanesum(x) = length(__mask__) * x # TODO remove this

function shuffle(x, i)
  i == 1 || error("Tried to shuffle from lane $i of 1")
  return x
end

@spmd shuffle(x, i) = x
@spmd shuffle(x::AbstractVec, i) = x[i]
