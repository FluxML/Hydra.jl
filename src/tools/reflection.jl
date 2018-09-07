using Core: CodeInfo

worldcounter() = ccall(:jl_get_world_counter, UInt, ())

struct MetaInfo
  method::Method
  code::CodeInfo
  sparams
end

function meta(T; world = worldcounter())
  F = T.parameters[1]
  F isa DataType && (F.name.module === Core.Compiler ||
                     F <: Core.Builtin ||
                     F <: Core.Builtin) && return nothing
  _methods = Base._methods_by_ftype(T, -1, world)
  length(_methods) == 1 || return nothing
  type_signature, sps, method = first(_methods)
  mi = Core.Compiler.code_for_method(method, type_signature, sps, world, false)
  ci = Base.isgenerated(mi) ? Base.get_staged(mi) : Base.uncompressed_ast(mi)
  MetaInfo(method, ci, sps)
end

function inline_sparams!(ir::IRCode, sps)
  ir = IncrementalCompact(ir)
  for (i, x) in ir
    for x in userefs(x)
      isexpr(x[], :static_parameter) && (x[] = sps[x[].args[1]])
    end
  end
  return finish(ir)
end

function IRCode(meta::MetaInfo)
  ir = just_construct_ssa(meta.code, deepcopy(meta.code.code),
                          Int(meta.method.nargs)-1, meta.sparams)
  return inline_sparams!(ir, meta.sparams)
end

function code_ir(f, T)
  m = meta(Tuple{Typeof(f),T.parameters...})
  return IRCode(m)
end

function code_irm(ex)
  isexpr(ex, :call) || error("@code_ir f(args...)")
  f, args = ex.args[1], ex.args[2:end]
  :(code_ir($(esc(f)), typesof($(esc.(args)...))))
end

macro code_ir(ex)
  code_irm(ex)
end

function argnames!(meta, names...)
  meta.code.slotnames = [names...]
end

function spliceargs!(meta, ir::IRCode, args...)
  for i = 1:length(ir.stmts)
    ir[SSAValue(i)] = argmap(x -> Argument(x.n+length(args)), ir[SSAValue(i)])
  end
  for (name, T) in reverse(args)
    pushfirst!(ir.argtypes, T)
    pushfirst!(meta.code.slotnames, name)
  end
  return ir
end

# Behave as if the function signature is f(args...)
function varargs!(meta, ir::IRCode, n = 1)
  isva = meta.method.isva
  Ts = widenconst.(ir.argtypes[n+1:end])
  argtypes = !isva ?
    Any[ir.argtypes[1:n]..., Tuple{Ts...}] :
    Any[ir.argtypes[1:n]..., Tuple{Ts[1:end-1]...,Ts[end].parameters...}]
  empty!(ir.argtypes); append!(ir.argtypes, argtypes)
  ir = IncrementalCompact(ir)
  map = Dict{Argument,Any}()
  for i = 1:(length(Ts)-isva)
    map[Argument(i+n)] = insert_node_here!(ir, xcall(Base, :getfield, Argument(n+1), i), Ts[i], Int32(0))
  end
  if isva
    i = length(Ts)
    xs, T = Argument(n+1), argtypes[end]
    for _ = 1:i-1
      T = Tuple{T.parameters[2:end]...}
      xs = insert_node_here!(ir, xcall(Base, :tail, xs), T, Int32(0))
    end
    map[Argument(i+n)] = xs
  end
  for (i, x) in ir
    ir[i] = argmap(a -> get(map, a, a), x)
  end
  return finish_dc(ir)
end

function update!(meta, ir::IRCode)
  ir = slots!(ir)
  Core.Compiler.replace_code_newstyle!(meta.code, ir, length(ir.argtypes)-1)
  meta.code.ssavaluetypes = length(meta.code.code)
  slots!(meta.code)
end

function inlineable!(ir)
  insert_node!(ir, 1, Any, Expr(:meta, :inline))
  compact!(ir)
end

spmd(::typeof(println), xs...) where {T,N} = println(unvect.(xs)...)
spmd(::typeof(print), xs...) where {T,N} = print(unvect.(xs)...)

function spmd(f, args...)
  if any(isvect, args)
    tospmd(f, args...)
  else
    f(args...)
  end
end


function pass(x::IRCode)
  new_stmts = []
  for stmt in x.stmts
    if isexpr(stmt, :call)
      new_stmt = xcall(SPMD, :spmd, stmt.args...)
      append!(new_stmts, [new_stmt])
    else
      append!(new_stmts, [stmt])
    end

    # println(fieldnames(stmt))
  end
  # append!(new_stmts, [:(1)])
  return IRCode(x, new_stmts, x.types, x.lines, x.flags, x.cfg, x.new_nodes)
end

unvec(::Type{Vec{T,N}}) where {T,N} = T
unvec(x) = x

@generated function roundtrip(f, args...)
  m = meta(Tuple{f,args...})
  ir = IRCode(m)
  ir = varargs!(m, ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(roundtrip)))
  update!(m, ir)
  return m.code
end

@generated function tospmd(f, args...)
  m = meta(Tuple{f,map(unvec, args)...})
  ir = IRCode(m)
  ir = varargs!(m, ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(roundtrip)))
  ir = pass(ir)
  update!(m, ir)
  return m.code
end

# x = @code_ir f(1)
# pass(x)
g(x) = x * 2
function f(x)
  println(g(x) < 10)
end
tospmd(f, vect(1,2,3,4,5))
# pass(y)
