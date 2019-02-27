using Hydra: @spmd, spmd, vect, VecArray, @code_ir
using Flux
using Flux.Tracker: TrackedArray, back!
using NNlib: conv
using CuArrays

@spmd conv(x::VecArray{C,A,N}, w::F) where {C,A,N,F<:AbstractArray} = begin
    res = conv(x.batch, w)
    res = VecArray{C, typeof(res), N}(res, x.sizes)
    res
end

import Flux: gpu
gpu(x::VecArray{C,A,N}) where {C,A,N} = begin
    res = gpu(x.batch)
    VecArray{C,typeof(res),N}(res, x.sizes)
end

@spmd gpu(x) = gpu(x)

sigm(x) = sigmoid.(x)
@spmd sigm(x::VecArray{C,A,N}) where {C,A,N} = VecArray{C,A,N}(sigmoid.(x.batch), x.sizes)

conv_w1 = Flux.glorot_uniform(3,3,1,8) |> TrackedArray |> gpu
conv_w2 = Flux.glorot_uniform(3,3,8,8) |> TrackedArray |> gpu
lin_w = Flux.glorot_uniform(10,4608) |> TrackedArray |> gpu

function model(x)
    x = conv(x, conv_w1)
    x = sigm(x)
    x = conv(x, conv_w2)
    x = sigm(x)
    x = reshape(x, 4608)
    x = lin_w * x
    sigm(x)
end

function b_model(x)
    x = conv(x, conv_w1)
    x = sigm(x)
    x = conv(x, conv_w2)
    x = sigm(x)
    x = reshape(x, 4608,160)
    x = lin_w * x
    sigm(x)
end

using BenchmarkTools

inp = repeat([rand(Float32, 28,28,1),rand(Float32, 28,28,1)], 80)

create_vect(xs::T...) where {C,N,T <: AbstractArray{C,N}} = begin
    res = cat(xs..., dims=N+1)
    VecArray{T,typeof(res),length(xs)}(res, size.(xs))
end

@btime spmd(vect(true,true), model, vect(inp...) |> gpu).batch

@btime b_model(cat(inp..., dims=4) |> gpu)
