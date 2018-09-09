__precompile__(false)

module SPMD

  using MacroTools
  using MacroTools: @forward
  using IRTools: meta, varargs!, argnames!, spliceargs!, update!, IR, @code_ir
  using IRTools: blocks, map, xcall
  import Core.Compiler: userefs

  include("vec.jl")

  include("llvm_spmd.jl")
  include("pass.jl")
end
