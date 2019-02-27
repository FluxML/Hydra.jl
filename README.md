# Hydra.jl
SPMD + Neural Nets

# DISCLAIMER: THIS IS NOT READY TO BE USED BY MOST PEOPLE

# What is Hydra.jl?

Hydra.jl main component is a compiler pass which transforms the Julia IR to allow people to write their functions on an individual example, but run them on multiple examples with no extra developer work. It follows the SPMD paradigm and takes inspiration mainly from ISPC, but also matchbox.

It has been created with a particular focus on Deep Learning applications, but it can also be used more generally.

# Example

Let's say we write a function that works on a scalar, like: 

```julia
function f(x)
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
```

We can then call this function with more than 1 input by doing: 

```julia
using Hydra

mask = Hydra.vect(true,true,true,true)
input = Vector{Int32}([1,5,12,-3])
Hydra.spmd(mask, f, Hydra.vect(input...))
```

This will make use (where possible) of vector registers on different CPU architectures, instead of naively looping through each example one at a time. Similarly, when operating with matrixes and array on GPU, we will batch operations. 

