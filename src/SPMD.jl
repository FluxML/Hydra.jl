__precompile__(false)

module SPMD

using MacroTools
using MacroTools: @forward

include("tools/ir.jl")
include("tools/slots.jl")
include("tools/reflection.jl")

end # module
