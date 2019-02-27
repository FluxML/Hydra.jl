struct Uniform{T}
    data::T
end

Uniform(x::T) where {T <:ScalarTypes} = Uniform{T}(x)
Uniform(x::T) where {T <:AbstractArray} = Uniform{T}(x)
Uniform(x) = x

@spmd Uniform(x) = Uniform(x)
