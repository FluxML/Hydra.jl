using Hydra, Test
using Hydra: shuffle

@test lane() == lanewidth() == 1

@test @spmd(4, lane()) == 1

@test @spmd(4, lanewidth()) == 4

@test @spmd(4, lanesum(5)) == 20

@test @spmd(4, shuffle(lane(), 4+1-lane())) == 4

@test @spmd(4, shuffle(2lane(), 3)) == 6

function if_statement(x)
  a = x + 1
  if a > 10
    a += 1000
  elseif a > 5
    a += 100
  else
    a += 10
  end

  if x > 0
    a += 10
  else
    a -= 10
  end
  return a
end

function while_loop(x)
  a = x + 1
  i = 0
  while x > i
    if x >= 10
      a += 3
    elseif x >= 5
      a += 2
    else
      a += 1
    end
    i += 1
  end
  a
end

function for_loop(x)
  a = x + 1
  for i=1:x
    if x > 10
      a += x
    else
      a -= x
    end
  end
  a
end

W = 1:60000 |> collect |> x->reshape(x,(200,300))
b = 1:200 |> collect

function rnn(sequence, ht)
  i = 1
  while i <= length(sequence)
    word = sequence[i]
    ht = W * word + b + ht
    i += 1
  end
  ht
end

mask = Hydra.vect(true,true,true,true)

input = Vector{Int32}([1,6,11,-1])
@test Hydra.spmd(mask, if_statement, Hydra.vect(input...)) == Hydra.vect(map(if_statement, input)...)

input = Vector{Int32}([5,5,5,5])
@test Hydra.spmd(mask, while_loop, Hydra.vect(input...)) == Hydra.vect(map(while_loop, input)...)

input = Vector{Int32}([1,5,12,-3])
@test Hydra.spmd(mask, while_loop, Hydra.vect(input...)) == Hydra.vect(map(while_loop, input)...)

input = Vector{Int32}([2,32,-1,21])
@test Hydra.spmd(mask, for_loop, Hydra.vect(input...)) == Hydra.vect(map(for_loop, input)...)

mask = Hydra.vect(true,true)

input = (
  [collect(1:300),collect(301:600),collect(601:900)],
  [collect(901:1200), collect(1201:1500)])
@test Hydra.spmd(mask, rnn, Hydra.vect(input...), zeros(Int64, 200)).batch == Hydra.vect(map(x->rnn(x,zeros(Int64, 200)), input)...).batch
