struct Context{N}
  mask::Vec{Bool,N}
end

Context{N}() where N = Context(vect(ntuple(_->true,N)...))

lanewidth(::Context{N}) where N = N

lanewidth() = 1
lane() = 1

spmd(cx::Context, ::typeof(lanewidth)) = lanewidth(cx)
spmd(cx::Context, ::typeof(lane)) = vect(ntuple(identity, lanewidth(cx))...)

default_lanewidth() = 4 # TODO use CpuId
# how do we choose something sensible for a data type?

# function spmd(f, x...)
#   spmd(Context{default_lanewidth()}, f, x...)
#   return
# end
