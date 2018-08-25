const BoolTypes = Union{Bool}
const IntTypes = Union{Int8, Int16, Int32, Int64, Int128}
const UIntTypes = Union{UInt8, UInt16, UInt32, UInt64, UInt128}
const IntegerTypes = Union{BoolTypes, IntTypes, UIntTypes}
const FloatingTypes = Union{Float16, Float32, Float64}
const ScalarTypes = Union{IntegerTypes, FloatingTypes}

llvmins(::Type{typeof(+)}, N, ::Type{T}) where {T <: IntegerTypes} = "add"
llvmins(::Type{typeof(-)}, N, ::Type{T}) where {T <: IntegerTypes} = "sub"
llvmins(::Type{typeof(*)}, N, ::Type{T}) where {T <: IntegerTypes} = "mul"
llvmins(::Type{typeof(div)}, N, ::Type{T}) where {T <: IntTypes} = "sdiv"
llvmins(::Type{typeof(rem)}, N, ::Type{T}) where {T <: IntTypes} = "srem"
llvmins(::Type{typeof(div)}, N, ::Type{T}) where {T <: UIntTypes} = "udiv"
llvmins(::Type{typeof(rem)}, N, ::Type{T}) where {T <: UIntTypes} = "urem"

llvmins(::Type{typeof(~)}, N, ::Type{T}) where {T <: IntegerTypes} = "xor"
llvmins(::Type{typeof(&)}, N, ::Type{T}) where {T <: IntegerTypes} = "and"
llvmins(::Type{typeof(|)}, N, ::Type{T}) where {T <: IntegerTypes} = "or"
llvmins(::Type{typeof(⊻)}, N, ::Type{T}) where {T <: IntegerTypes} = "xor"

llvmins(::Type{typeof(<<)}, N, ::Type{T}) where {T <: IntegerTypes} = "shl"
llvmins(::Type{typeof(>>>)}, N, ::Type{T}) where {T <: IntegerTypes} = "lshr"
llvmins(::Type{typeof(>>)}, N, ::Type{T}) where {T <: UIntTypes} = "lshr"
llvmins(::Type{typeof(>>)}, N, ::Type{T}) where {T <: IntTypes} = "ashr"

llvmins(::Type{typeof(==)}, N, ::Type{T}) where {T <: IntegerTypes} = "icmp eq"
llvmins(::Type{typeof(!=)}, N, ::Type{T}) where {T <: IntegerTypes} = "icmp ne"
llvmins(::Type{typeof(>)}, N, ::Type{T}) where {T <: IntTypes} = "icmp sgt"
llvmins(::Type{typeof(>=)}, N, ::Type{T}) where {T <: IntTypes} = "icmp sge"
llvmins(::Type{typeof(<)}, N, ::Type{T}) where {T <: IntTypes} = "icmp slt"
llvmins(::Type{typeof(<=)}, N, ::Type{T}) where {T <: IntTypes} = "icmp sle"
llvmins(::Type{typeof(>)}, N, ::Type{T}) where {T <: UIntTypes} = "icmp ugt"
llvmins(::Type{typeof(>=)}, N, ::Type{T}) where {T <: UIntTypes} = "icmp uge"
llvmins(::Type{typeof(<)}, N, ::Type{T}) where {T <: UIntTypes} = "icmp ult"
llvmins(::Type{typeof(<=)}, N, ::Type{T}) where {T <: UIntTypes} = "icmp ule"

llvmins(::Type{typeof(+)}, N, ::Type{T}) where {T <: FloatingTypes} = "fadd"
llvmins(::Type{typeof(-)}, N, ::Type{T}) where {T <: FloatingTypes} = "fsub"
llvmins(::Type{typeof(*)}, N, ::Type{T}) where {T <: FloatingTypes} = "fmul"
llvmins(::Type{typeof(/)}, N, ::Type{T}) where {T <: FloatingTypes} = "fdiv"
llvmins(::Type{typeof(inv)}, N, ::Type{T}) where {T <: FloatingTypes} = "fdiv"
llvmins(::Type{typeof(rem)}, N, ::Type{T}) where {T <: FloatingTypes} = "frem"

llvmins(::Type{typeof((==))}, N, ::Type{T}) where {T <: FloatingTypes} = "fcmp oeq"
llvmins(::Type{typeof((!=))}, N, ::Type{T}) where {T <: FloatingTypes} = "fcmp une"
llvmins(::Type{typeof((>))}, N, ::Type{T}) where {T <: FloatingTypes} = "fcmp ogt"
llvmins(::Type{typeof((>=))}, N, ::Type{T}) where {T <: FloatingTypes} = "fcmp oge"
llvmins(::Type{typeof((<))}, N, ::Type{T}) where {T <: FloatingTypes} = "fcmp olt"
llvmins(::Type{typeof((<=))}, N, ::Type{T}) where {T <: FloatingTypes} = "fcmp ole"

llvmins(::typeof(^), N, ::Type{T}) where {T <: FloatingTypes} =
    "@llvm.pow.$(suffix(N,T))"
llvmins(::typeof(abs), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.fabs.$(suffix(N,T))"
llvmins(::typeof(ceil), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.ceil.$(suffix(N,T))"
llvmins(::typeof(copysign), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.copysign.$(suffix(N,T))"
llvmins(::typeof(cos), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.cos.$(suffix(N,T))"
llvmins(::typeof(exp), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.exp.$(suffix(N,T))"
llvmins(::typeof(exp2), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.exp2.$(suffix(N,T))"
llvmins(::typeof(floor), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.floor.$(suffix(N,T))"
llvmins(::typeof(fma), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.fma.$(suffix(N,T))"
llvmins(::typeof(log), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.log.$(suffix(N,T))"
llvmins(::typeof(log10), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.log10.$(suffix(N,T))"
llvmins(::typeof(log2), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.log2.$(suffix(N,T))"
llvmins(::typeof(max), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.maxnum.$(suffix(N,T))"
llvmins(::typeof(min), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.minnum.$(suffix(N,T))"
llvmins(::typeof(muladd), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.fmuladd.$(suffix(N,T))"
llvmins(::typeof(round), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.rint.$(suffix(N,T))"
llvmins(::typeof(sin), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.sin.$(suffix(N,T))"
llvmins(::typeof(sqrt), N, ::Type{T}) where {T<:FloatingTypes} =
    "@llvm.sqrt.$(suffix(N,T))"
llvmins(::typeof(trunc), N, ::Type{T}) where {T<:FloatingTypes} =
"@llvm.trunc.$(suffix(N,T))"

llvmtype(::Type{Bool}) = "i8"   # Julia represents Tuple{Bool} as [1 x i8]

# llvmtype(::Type{Bool8}) = "i8"
# llvmtype(::Type{Bool16}) = "i16"
# llvmtype(::Type{Bool32}) = "i32"
# llvmtype(::Type{Bool64}) = "i64"
# llvmtype(::Type{Bool128}) = "i128"

llvmtype(::Type{Int8}) = "i8"
llvmtype(::Type{Int16}) = "i16"
llvmtype(::Type{Int32}) = "i32"
llvmtype(::Type{Int64}) = "i64"
llvmtype(::Type{Int128}) = "i128"

llvmtype(::Type{UInt8}) = "i8"
llvmtype(::Type{UInt16}) = "i16"
llvmtype(::Type{UInt32}) = "i32"
llvmtype(::Type{UInt64}) = "i64"
llvmtype(::Type{UInt128}) = "i128"

llvmtype(::Type{Float16}) = "half"
llvmtype(::Type{Float32}) = "float"
llvmtype(::Type{Float64}) = "double"

@generated function vectorise(f, x::Vec{T, N}, y::Vec{T, N}) where {T, N}
    llvmT = llvmtype(T)
    func = llvmins(f, N, T)
    exp = """
    %3 = $(func) <$(N) x $(llvmT)> %0, %1
    ret <$(N) x $(llvmT)> %3
    """
    return quote
        Base.@_inline_meta
        Vec(Core.getfield(Base, :llvmcall)($exp, NTuple{N,VecElement{T}}, Tuple{NTuple{N,VecElement{T}}, NTuple{N,VecElement{T}}}, x.data, y.data))
    end
end

@generated function vectorisePredicate(f, x::Vec{T, N}, y::Vec{T, N}) where {T, N}
    llvmT = llvmtype(T)
    func = llvmins(f, N, T)
    exp = """
    %3 = $(func) <$(N) x $(llvmT)> %0, %1
    %4 = zext <$(N) x i1> %3 to <$(N) x i8>
    ret <$(N) x i8> %4
    """
    return quote
        Base.@_inline_meta
        Vec(Core.getfield(Base, :llvmcall)($exp, NTuple{N,VecElement{Bool}}, Tuple{NTuple{N,VecElement{T}}, NTuple{N,VecElement{T}}}, x.data, y.data))
    end
end

for op in (:+, :-, :*, :/, :div, :rem)
    @eval begin
        spmd(::typeof($op), xs::Vec{T, N}, x::S) where {S <: Number, T <: Number, N} = vectorise($op, promote(xs,x)...)
    end
end

for op in (:(==),:(!=), :(>), :(>=), :(<), :(<=))
    @eval begin
        spmd(::typeof($op), xs::Vec{T, N}, x::S) where {S <: Number, T <: Number, N} = vectorisePredicate($op, promote(xs,x)...)
    end
end
