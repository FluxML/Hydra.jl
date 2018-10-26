using Core.Compiler: PhiNode, GotoNode, GotoIfNot, ReturnNode, SSAValue, BasicBlock, Argument

spmd(::typeof(println), xs...) where {T,N} = println(data.(xs)...)
spmd(::typeof(print), xs...) where {T,N} = print(data.(xs)...)

is_latch(block_cfg, block_id) = any(map(succ -> succ < block_id, block_cfg.succs))
is_header(block_cfg, block_id) = any(map(pred -> pred > block_id, block_cfg.preds))

function spmd(f, args...)
  # println(f, args)
  if any(x -> x isa AbstractVec, args)
    tospmd(f, args...)
  else
    f(args...)
  end
end

function get_block_for_statement(ir::IR)
  block_for_statement = Dict{SSAValue, Int64}()
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if ssavalue != nothing
        block_for_statement[ssavalue] = b.id
      end
    end
  end
  block_for_statement
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

function select(conds::Vec{Bool, N}, first_vals::Vec{T,N}, second_vals::Vec{S,N}) where {T,S,N}
  result = zeros(promote_type(T,S), N)
  for i in range(1, length=N)
    if conds[i]
      result[i] = first_vals[i]
    else
      result[i] = second_vals[i]
    end
  end
  return vect(result...)
end

function select(conds::Vec{Bool, N}, first_val::T, second_val::S) where {T <: ScalarTypes, S <: ScalarTypes, N}
  if any(conds)
    first_val
  else
    second_val
  end
end

conds_any_true(conds) = any(conds)

spmd(::typeof(select), conds, val1, val2) = select(conds, val1, val2)
spmd(::typeof(conds_any_true), conds) = conds_any_true(conds)

function get_conditions_for_blocks(ir)
  cfg = CFG(ir)
  blcks = blocks(ir)
  block_to_conds = Dict{Int64, Vector{Tuple{SSAValue, Bool}}}()
  for (block, block_cfg, block_id) in zip(blcks, cfg.blocks, range(1, length=length(blcks)))
    block_to_conds[block_id] = Vector{Tuple{SSAValue, Bool}}(undef, 0)
    preds_no_backward = filter(x -> !is_latch(cfg.blocks[x], x), block_cfg.preds)
    if length(preds_no_backward) == 1
      parent = blcks[preds_no_backward[1]]
      parent_conds = block_to_conds[preds_no_backward[1]]
      for parent_cond in parent_conds
        add_cond_for_block(block_to_conds, block_id, parent_cond)
      end
      if length(parent.bb.gotos) >= 1
        parent_cfg = cfg.blocks[preds_no_backward[1]]
        cond = parent.bb.gotos[1].expr.cond
        else_dest = parent.bb.gotos[1].expr.dest
        if_dest = parent.id + 1
        if block_id == if_dest
          add_cond_for_block(block_to_conds, block_id, (cond, true))
        elseif block_id == else_dest && !is_header(parent_cfg, preds_no_backward[1])
          add_cond_for_block(block_to_conds, block_id, (cond, false))
        end
      end
    elseif length(preds_no_backward) > 1
      parents_conds = map(id -> block_to_conds[id], preds_no_backward)
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

function get_vector_values(ir::IR, prev_vector_values::Dict{SSAValue, Bool})
  is_ssavalue_vec = copy(prev_vector_values)
  blks = blocks(ir)
  for b in blks
    for (ssavalue, stmt) in b
      if stmt.expr isa PhiNode
        is_ssavalue_vec[ssavalue] =
          any(map(arg -> isa(arg, Argument), stmt.expr.values)) |
          any(map(arg -> isa(arg, SSAValue)
            && haskey(is_ssavalue_vec, arg)
            && is_ssavalue_vec[arg], stmt.expr.values))
      elseif stmt.expr isa Expr
        is_ssavalue_vec[ssavalue] =
          any(map(arg -> isa(arg, Argument), stmt.expr.args)) |
          any(map(arg -> isa(arg, SSAValue)
            && haskey(is_ssavalue_vec, arg)
            && is_ssavalue_vec[arg], stmt.expr.args))
      end
    end
  end
  if prev_vector_values == is_ssavalue_vec
    return is_ssavalue_vec
  else
    return get_vector_values(ir, is_ssavalue_vec)
  end
end

function fix_phinode(expr, old_to_new_ssavalue, old_to_new_block=Dict())
  if expr isa PhiNode
    new_values = map(x->if x isa SSAValue old_to_new_ssavalue[x] else x end,expr.values)
    new_edges = map(x->if haskey(old_to_new_block, x) old_to_new_block[x] else x end, expr.edges)
    new_expr = PhiNode(new_edges, new_values)
    return new_expr
  end
  return expr
end

function pass_for_loops_gotos(ir, old_to_new_block)
  new_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if stmt.expr isa GotoIfNot
        push!(new_ir, xcall(SPMD, :conds_any_true, stmt.expr.cond))
        new_ssavalue = SSAValue(length(new_ir.defs))
        new_stmt = GotoIfNot(new_ssavalue, old_to_new_block[stmt.expr.dest])
        push!(new_ir, new_stmt)
      elseif stmt.expr isa GotoNode
        new_stmt = GotoNode(old_to_new_block[stmt.expr.label])
        push!(new_ir, new_stmt)
      elseif stmt.expr isa ReturnNode
        push!(new_ir, ReturnNode(old_to_new_ssavalue[stmt.expr.val]))
      elseif stmt.expr isa PhiNode
        push!(new_ir, stmt)
        new_ssavalue = SSAValue(length(new_ir.defs))
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      else
        new_args = map(x->if x isa SSAValue old_to_new_ssavalue[x] else x end,stmt.expr.args)
        new_stmt = Expr(:call, new_args[1], new_args[2:length(new_args)]...)
        push!(new_ir, new_stmt)
        new_ssavalue = SSAValue(length(new_ir.defs))
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      end
    end
    if (b.id != length(blocks(ir)))
      block!(new_ir)
    end
  end
  new_ir = map(x->fix_phinode(x, old_to_new_ssavalue), new_ir)
  new_ir
end

function get_phinodes_values(ir)
  phinodes_values = Dict{SSAValue, SSAValue}()
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if stmt.expr isa PhiNode
        in_loop_values = map(x -> x[1] > b.id && x[2] isa SSAValue && x[2], zip(stmt.expr.edges, stmt.expr.values))
        in_loop_values = filter(x -> x != false, in_loop_values)
        for value in in_loop_values
          phinodes_values[value] = ssavalue
        end
      end
    end
  end
  phinodes_values
end

function pass_for_loops_phinodes(ir, block_to_cond)
  block_for_statement = get_block_for_statement(ir)
  phinodes_values = get_phinodes_values(ir)
  new_ir = new_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if stmt.expr isa GotoIfNot
        new_stmt = GotoIfNot(old_to_new_ssavalue[stmt.expr.cond], stmt.expr.dest)
        push!(new_ir, new_stmt)
      elseif stmt.expr isa GotoNode
        push!(new_ir, stmt)
      elseif stmt.expr isa ReturnNode
        push!(new_ir, ReturnNode(old_to_new_ssavalue[stmt.expr.val]))
      elseif stmt.expr isa PhiNode
        push!(new_ir, stmt)
        new_ssavalue = SSAValue(length(new_ir.defs))
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      else
        new_args = map(x->if x isa SSAValue old_to_new_ssavalue[x] else x end,stmt.expr.args)
        new_stmt = Expr(:call, new_args[1], new_args[2:length(new_args)]...)
        push!(new_ir, new_stmt)
        new_ssavalue = SSAValue(length(new_ir.defs))
        if haskey(phinodes_values, ssavalue)
          cond = old_to_new_ssavalue[block_to_cond[b.id]]
          header_value = old_to_new_ssavalue[phinodes_values[ssavalue]]
          push!(new_ir, xcall(SPMD, :select, cond, new_ssavalue, header_value))
          new_ssavalue = SSAValue(length(new_ir.defs))
        end
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      end
    end
    if (b.id != length(blocks(ir)))
      block!(new_ir)
    end
  end
  new_ir = map(x->fix_phinode(x, old_to_new_ssavalue), new_ir)
  new_ir
end

function pass_if(ir)
  negated_conditions = NegatedConditions()
  block_to_conds = get_conditions_for_blocks(ir)
  block_to_cond = Dict{Int64, SSAValue}()
  new_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  old_to_new_block = Dict{Int64, Int64}()
  blks = blocks(ir)
  cfg = CFG(ir)
  for (b, b_cfg) in zip(blks, cfg.blocks)
    if is_header(b_cfg, b.id)
      block!(new_ir)
    end
    old_to_new_block[b.id] = length(blocks(new_ir))
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
        if is_header(b_cfg, b.id)
          push!(new_ir, stmt)
          new_ssavalue = SSAValue(length(new_ir.defs))
          old_to_new_ssavalue[ssavalue] = new_ssavalue
          continue
        end
        conds = map(x -> block_to_cond[x], stmt.expr.edges)
        values = map(x -> old_to_new_ssavalue[x], stmt.expr.values)
        first_cond, second_cond = conds[1], conds[2]
        first_val, second_val = values[1], values[2]
        push!(new_ir, xcall(SPMD, :select, first_cond, first_val, second_val))
        new_ssavalue = SSAValue(length(new_ir.defs))
        for (condition, value) in zip(conds[3:length(conds)], values[3:length(values)])
          push!(new_ir, xcall(SPMD, :select, condition, value, new_ssavalue))
          new_ssavalue = SSAValue(length(new_ir.defs))
        end
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      elseif stmt.expr isa ReturnNode
        push!(new_ir, ReturnNode(old_to_new_ssavalue[stmt.expr.val]))
      elseif isgoto(stmt.expr)
        if is_latch(b_cfg, b.id) | is_header(b_cfg, b.id)
          push!(new_ir, stmt)
          block!(new_ir)
        end
      elseif stmt.expr isa Expr
        new_args = map(x->if x isa SSAValue old_to_new_ssavalue[x] else x end,stmt.expr.args)
        new_stmt = Expr(:call, new_args[1], new_args[2:length(new_args)]...)
        push!(new_ir, new_stmt)
        new_ssavalue = SSAValue(length(new_ir.defs))
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      elseif stmt.expr isa PiNode
        #TODO
      end
    end
  end
  new_ir = map(x->fix_phinode(x, old_to_new_ssavalue, old_to_new_block), new_ir)
  (new_ir, old_to_new_block, block_to_cond)
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

unwraptype(::Type{Vec{T,N}}) where {T,N} = T
unwraptype(x) = x

function pass(ir)
  ir, old_to_new_block, block_to_cond = pass_if(ir)
  ir = pass_for_loops_phinodes(ir, block_to_cond)
  ir = pass_call(ir)
  ir = pass_for_loops_gotos(ir, old_to_new_block)
end

@generated function tospmd(f, args...)
  m = meta(Tuple{f,unwraptype.(args)...})
  ir = IR(m)
  ir = pass(ir)
  ir = varargs!(m, ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(tospmd)))
  update!(m, ir)
  return m.code
end

using IRTools: @code_ir

function f(x)
  a = x + 1
  if a > 10
    a += 1000
  elseif a > 5
    a += 100
  else
    a += 10
  end

  if x > 0
    a += 10
  else
    a -= 10
  end
  return a
end

function g(x)
  a = x + 1
  i = 0
  while x > i
    if x >= 10
      a += 3
    elseif x >= 5
      a += 2
    else
      a += 1
    end
    i += 1
  end
  a
end


# function naive_spmd(f, args)
#   map(f, args)
# end
code = @code_ir g(5)
# println(code)
# println(pass(code))
# println(get_vector_values(code, Dict{SSAValue, Bool}()))
# println(pass_if(code)...)
# println(pass_for_loops_gotos(pass_if(code)...))


# println(pass_call(pass_for_loops_gotos(pass_if(code)...)))
# println(tospmd(g, vect(3,4,5,6)))
# println(tospmd(f, vect(-6,-11,4.4,6)))
# using BenchmarkTools
#
# input = Vector{Int16}(repeat([256], 8))
#
# println(tospmd(f, vect(1,6,11,-1)))
#
# @btime tospmd(g, Vec{Int16, 8}(256))
# @btime naive_spmd(g, input)
# println(tospmd(g, vect(3,4,5,10)))
# println(naive_spmd(g, input))
