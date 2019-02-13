__precompile__(false)

module SPMD
  using MacroTools
  using MacroTools: @forward
  using IRTools: meta, varargs!, argnames!, spliceargs!, update!, IR, @code_ir, iscontrol
  using IRTools: blocks, map, xcall, isgoto, block, blockidx, blocks, successors, label, Block
  using IRTools: block!
  using IRTools.Wrap: CFG
  import Core.Compiler: userefs

  export @spmd, lanewidth, lane, lanesum

  include("vec.jl")

  include("interface.jl")
  include("lib/general.jl")
  include("lib/numeric.jl")
  include("lib/array.jl")
  include("lib/lane.jl")
  include("pass.jl")
end
