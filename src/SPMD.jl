__precompile__(false)

module SPMD
  using MacroTools
  using MacroTools: @forward
  using IRTools: meta, varargs!, argnames!, spliceargs!, update!, IR, @code_ir, iscontrol
  using IRTools: blocks, map, xcall, isgoto, block, blockidx, blocks, successors, label, Block
  using IRTools: block!
  using IRTools.Wrap: CFG
  import Core.Compiler: userefs

  include("vec.jl")

  include("interface.jl")
  include("llvm_spmd.jl")
  include("pass.jl")
end
