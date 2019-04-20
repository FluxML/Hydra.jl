# Hydra

Hydra provides a SPMD programming model for Julia, intended for auto batching of
machine learning models. Hydra is an early proof-of-concept; it ready for simple
alpha testing and testers should open issues liberally.

```julia
julia> @spmd 4 println("Hello, World!")
Hello, World!
Hello, World!
Hello, World!
Hello, World!
```

`@spmd N` is analagous to `for i = 1:N ...`, with an important difference;
each "lane" (iteration) runs *in lockstep*, one instruction from each lane at
a time.

```
julia> @spmd 4 begin
         println("Hello from lane ", lane())
         println("Goodbye from lane ", lane())
       end
Hello from lane 1
Hello from lane 2
Hello from lane 3
Hello from lane 4
Goodbye from lane 1
Goodbye from lane 2
Goodbye from lane 3
Goodbye from lane 4
```

This allows computations across lanes to be run *batched*. For example, the set
of return values from `lane()` can be stored as a tuple and use [SIMD
operations](https://github.com/eschnett/SIMD.jl); thus 4 lanes of SPMD code can
potentially be just as fast as a single lane of normal scalar code. Hydra also
handles batches of more complex objects such as arrays, allowing it to express
the kinds of batching used in machine learning.

```julia
julia> @spmd 4 lanesum(lane()*2)
20
```

Crucially, Hydra does this while taking care of control flow, so we hope to
bring great performance to almost any Julia program.

```julia
julia> @spmd 4 begin
         iseven(lane()) && println("Hello from lane ", lane())
       end
Hello from lane 2
Hello from lane 4
```
