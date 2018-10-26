using SPMD, Test
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

input = Vector{Int32}([1,6,11,-1])
@test SPMD.tospmd(if_statement, SPMD.vect(input...)) == SPMD.vect(map(if_statement, input)...)

input = Vector{Int32}([5,5,5,5])
@test SPMD.tospmd(while_loop, SPMD.vect(input...)) == SPMD.vect(map(while_loop, input)...)

input = Vector{Int32}([1,5,12,-3])
@test SPMD.tospmd(while_loop, SPMD.vect(input...)) == SPMD.vect(map(while_loop, input)...)
