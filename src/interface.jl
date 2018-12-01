# SPMD primitives

named(arg) = isexpr(arg, :(::)) && length(arg.args) == 1 ? :($(gensym())::$(arg.args[1])) : arg

typeless(x) = MacroTools.prewalk(x -> isexpr(x, :(::)) ? x.args[1] : x, x)

macro spmd(ex)
  @capture(shortdef(ex), (name_(args__) = body_) |
                         (name_(args__) where {Ts__} = body_)) || error("Need a function definition")
  f, T = isexpr(name, :(::)) ?
    (length(name.args) == 1 ? (esc(gensym()), esc(name.args[1])) : esc.(name.args)) :
    (esc(gensym()), :(typeof($(esc(name)))))
  Ts = esc.(Ts == nothing ? [] : Ts)
  args = esc.(named.(args))
  argnames = typeless.(args)
  cx = :($(esc(:__mask__))::Mask)
  fargs = [cx, :($f::$T), args...]
  MacroTools.@q begin
    @inline SPMD.spmd($(fargs...)) where $(Ts...) = $(esc(body))
  end
end

# SPMD execution

mask(n) = vect(ntuple(_ -> true, n)...)

lane1(xs::AbstractVec) = xs[1]
lane1(x) = x

macro spmd(n, ex)
  :(lane1(spmd(mask($(esc(n))), () -> $(esc(ex)))))
end

# Lane-level primitives

lane() = 1

@spmd lane() = vect(ntuple(identity, length(__mask__))...)

lanewidth() = 1

@spmd lanewidth() = length(__mask__)

lanesum(x) = lanewidth() * x

@spmd lanesum(x::AbstractVec) = sum(x)
@spmd lanesum(x) = length(__mask__) * x # TODO remove this

function shuffle(x, i)
  i == 1 || error("Tried to shuffle from lane $i of 1")
  return x
end

@spmd shuffle(x, i) = x
@spmd shuffle(x::AbstractVec, i) = x[i]
