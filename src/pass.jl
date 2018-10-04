using Core.Compiler: PhiNode, GotoNode, GotoIfNot, ReturnNode, SSAValue, BasicBlock

spmd(::typeof(println), xs...) where {T,N} = println(unvect.(xs)...)
spmd(::typeof(print), xs...) where {T,N} = print(unvect.(xs)...)

function spmd(f, args...)
  if any(isvect, args)
    tospmd(f, args...)
  else
    f(args...)
  end
end

function add_cond_for_block(block_to_conds::Dict{Int64, Vector{Tuple{SSAValue, Bool}}}, block_id, cond)
  if haskey(block_to_conds, block_id)
    if !(cond in block_to_conds[block_id])
      push!(block_to_conds[block_id], cond)
    end
  else
    block_to_conds[block_id] = [cond]
  end
end

function phi(conditions::Array{Vec{Bool, N}}, values::Array{Vec{T, N}}) where {T, N}
  res = ones(T, N)
  for (condition, value) in zip(conditions, values)
    cond_value_tuples = collect(zip(unvect(condition), unvect(value)))
    for i in range(1, length=N)
      c,v = cond_value_tuples[i]
      if c
        res[i] = v
      end
    end
  end
  return vect(res...)
end

spmd(::typeof(phi), xs...) = phi(xs...)
spmd(::typeof(Base.vect), xs...) = Base.vect(xs...)

function get_conditions_for_blocks(ir)
  cfg = CFG(ir)
  blcks = blocks(ir)
  block_to_conds = Dict{Int64, Vector{Tuple{SSAValue, Bool}}}()
  for (block, block_cfg, block_id) in zip(blcks, cfg.blocks, range(1, length=length(blcks)))
    block_to_conds[block_id] = Vector{Tuple{SSAValue, Bool}}(undef, 0)
    if length(block_cfg.preds) == 1
      parent = blcks[block_cfg.preds[1]]
      parent_conds = block_to_conds[block_cfg.preds[1]]
      cond = parent.bb.gotos[1].expr.cond
      else_dest = parent.bb.gotos[1].expr.dest
      if_dest = parent.id + 1
      for parent_cond in parent_conds
        add_cond_for_block(block_to_conds, block_id, parent_cond)
      end
      if block_id == if_dest
        add_cond_for_block(block_to_conds, block_id, (cond, true))
      elseif block_id == else_dest
        add_cond_for_block(block_to_conds, block_id, (cond, false))
      end
    elseif length(block_cfg.preds) > 1
      parents_conds = map(id -> block_to_conds[id], block_cfg.preds)
      shared_conds = intersect(parents_conds...)
      for shared_cond in shared_conds
        add_cond_for_block(block_to_conds, block_id, shared_cond)
      end
    end
  end
  block_to_conds
end

struct NegatedConditions
  conds::Dict{SSAValue, SSAValue}
  NegatedConditions(conds) = new(conds)
  NegatedConditions() = NegatedConditions(Dict{SSAValue, SSAValue}())
end

function get_negated(nc, cond, ir)
  if haskey(nc.conds, cond)
    return nc.conds[cond]
  end
  push!(ir, xcall(:~, cond))
  nc.conds[cond] = SSAValue(length(ir.defs))
  return SSAValue(length(ir.defs))
end

function get_condition(ssavalue, is_true, old_to_new, nc, ir)
  if is_true
    old_to_new[ssavalue]
  else
    get_negated(nc, old_to_new[ssavalue], ir)
  end
end

function pass_if(ir)
  negated_conditions = NegatedConditions()
  block_to_conds = get_conditions_for_blocks(ir)
  println(block_to_conds)
  block_to_cond = Dict{Int64, SSAValue}()
  new_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  blks = blocks(ir)
  for b in blks
    if haskey(block_to_conds, b.id)
      conds = block_to_conds[b.id]
      if length(conds) >= 1
        current_cond = get_condition(conds[1]..., old_to_new_ssavalue, negated_conditions, new_ir)
        for i in range(2,length=length(conds)-1)
          cond = get_condition(conds[i]..., old_to_new_ssavalue, negated_conditions, new_ir)
          push!(new_ir, xcall(:&, current_cond, cond))
          current_cond = SSAValue(length(new_ir.defs))
        end
        block_to_cond[b.id] = current_cond
      end
    end
    for (ssavalue, stmt) in b
      if stmt.expr isa PhiNode
        conds = map(x -> block_to_cond[x], stmt.expr.edges)
        values = map(x -> old_to_new_ssavalue[x], stmt.expr.values)
        push!(new_ir, xcall(:vect, conds...))
        conds_ssa = SSAValue(length(new_ir.defs))
        push!(new_ir, xcall(:vect, values...))
        values_ssa = SSAValue(length(new_ir.defs))
        push!(new_ir, xcall(SPMD, :phi, conds_ssa, values_ssa))
        old_to_new_ssavalue[ssavalue] = SSAValue(length(new_ir.defs))
      elseif stmt.expr isa ReturnNode
        push!(new_ir, ReturnNode(old_to_new_ssavalue[stmt.expr.val]))
      elseif !isgoto(stmt.expr)
        push!(new_ir, stmt)
        old_to_new_ssavalue[ssavalue] = SSAValue(length(new_ir.defs))
      end
    end
  end
  new_ir
end

function pass_call(ir)
  function transform_call(stmt)
    if isexpr(stmt, :call)
      stmt = xcall(SPMD, :spmd, stmt.args...)
    end
    stmt
  end
  map(transform_call, ir)
end

@generated function tospmd(f, args...)
  m = meta(Tuple{f,map(datatype, args)...})
  ir = IR(m)
  ir = pass_if(ir)
  ir = pass_call(ir)
  ir = varargs!(m, ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(tospmd)))
  update!(m, ir)
  return m.code
end
# 
# using IRTools: @code_ir
#
# function f(x)
#   a = 1 + x
#   if x > 0
#     if x > 5
#       a += 1000
#     elseif x > 2
#       a += 100
#     else
#       a += 10
#     end
#   else
#     if x < -10
#       a -= 1000
#     else
#       if x > -5
#         a -= 10
#       else
#         a -= 100
#       end
#     end
#   end
#   return a
# end
#
# function g(x)
#   a = x + 1
#   for i in range(1, length=x)
#     a *= i
#   end
#   a
# end
#
# code = @code_ir g(5)
# println(code)
# # println(pass_call(pass_if(code)))
#
# println(tospmd(f, vect(-6,-11,4,6)))
# println(tospmd(f, vect(-11,0,10,19)))
