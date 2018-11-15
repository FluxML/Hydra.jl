using Core.Compiler: PhiNode, GotoNode, GotoIfNot, ReturnNode, SSAValue, BasicBlock, Argument, PiNode
using IRTools

function spmd(mask, ::typeof(iterate), iter::AbstractVec{T,N}) where {T, N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(mask, iter)) do x
    if x[1]
      iterate(x[2])
    else
      nothing
    end
  end |> splat_and_vect
end

function spmd(mask, ::typeof(iterate), iter::AbstractVec{T,N}, state::Vec{S,N}) where {T, S, N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(mask, iter, state)) do x
    if x[1]
      iterate(x[2], x[3])
    else
      nothing
    end
  end |> splat_and_vect
end

function masked_getfield(mask, xs::HoleyVec{T,N}, names) where {T,N}
  splat_and_vect(xs) = vect(xs...)
  map(zip(mask, xs, names)) do (m, x, name)
    if m
      getfield(x, name)
    else
      nothing
    end
  end |> splat_and_vect
end

function spmd(mask, ::typeof(getfield), x::Vec{T,N}, name::S) where {S,T,N}
  vect(getfield.(x,name)...)
end
spmd(mask, ::typeof(getfield), x::Vec{T,N}, name::Vec{S,N}) where {S,T,N} = vect(getfield.(x, name)...)

spmd(mask, ::typeof(getfield), x::HoleyVec{T,N}, name::S) where {S,T,N} = masked_getfield(mask, x, Vec{S,N}(name))
spmd(mask, ::typeof(getfield), x::HoleyVec{T,N}, name::HoleyVec{S,N}) where {S,T,N} = masked_getfield(mask, x, name)

spmd(mask, ::typeof(println), xs...) where {T,N} = println(data.(xs)...)
spmd(mask, ::typeof(print), xs...) where {T,N} = print(data.(xs)...)

is_latch(block_cfg, block_id) = any(map(succ -> succ < block_id, block_cfg.succs))
is_header(block_cfg, block_id) = any(map(pred -> pred > block_id, block_cfg.preds))

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

function add_cond_for_block(block_to_conds::Dict{Int64, Vector{Tuple{Union{SSAValue, Argument}, Bool}}}, block_id, cond)
  if haskey(block_to_conds, block_id)
    if !(cond in block_to_conds[block_id])
      push!(block_to_conds[block_id], cond)
    end
  else
    block_to_conds[block_id] = [cond]
  end
end

function select(conds::Vec{Bool, N}, first_vals::AbstractVec{T,N}, second_vals::AbstractVec{T,N}) where {T,S,N}
  result = zeros(T,N)
  for i in range(1, length=N)
    if !conds[i]
      result[i] = second_vals[i]
    else
      result[i] = first_vals[i]
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

struct NegatedConditions
  conds::Dict{SSAValue, SSAValue}
  NegatedConditions(conds) = new(conds)
  NegatedConditions() = NegatedConditions(Dict{SSAValue, SSAValue}())
end

function get_negated(nc, cond, ir)
  if haskey(nc.conds, cond)
    return nc.conds[cond]
  end
  push!(ir, xcall(SPMD, :not_mask, cond))
  nc.conds[cond] = SSAValue(length(ir.defs))
  return SSAValue(length(ir.defs))
end

function get_condition(value, is_true, old_to_new, nc, ir)
  if is_true
    if value isa SSAValue
      old_to_new[value]
    elseif value isa Argument
      value
    end
  else
    get_negated(nc, old_to_new[value], ir)
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
        push!(new_ir, xcall(:any, stmt.expr.cond))
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

function get_phinodes_values(ir, block_to_cond)
  conds = block_to_cond.vals
  phinodes_values = Dict{SSAValue, SSAValue}()
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if stmt.expr isa PhiNode && !(ssavalue in conds)
        in_loop_values = map(x -> x[1] >= b.id && x[2] isa SSAValue && x[2], zip(stmt.expr.edges, stmt.expr.values))
        in_loop_values = filter(x -> x != false, in_loop_values)
        for value in in_loop_values
          phinodes_values[value] = ssavalue
        end
      end
    end
  end
  phinodes_values
end

function pass_loops_result_value(ir, block_to_cond)
  block_for_statement = get_block_for_statement(ir)
  phinodes_values = get_phinodes_values(ir, block_to_cond)
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
      elseif stmt.expr isa Expr
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

and_mask(masks...) = reduce((acc,val)->vectorise(&, promote(acc,val)...), masks)
or_mask(masks...) = reduce((acc,val)->vectorise(|, promote(acc,val)...), masks)
not_mask(mask) = vect(map(~, mask)...)

function get_conditions_for_blocks(ir)
  cfg = CFG(ir)
  blcks = blocks(ir)
  block_to_conds = Dict{Int64, Vector{Tuple{Union{SSAValue, Argument}, Bool}}}()
  block_to_conds[1] = [(Argument(0), true)]
  for (block, block_cfg, block_id) in zip(blcks, cfg.blocks, range(1, length=length(blcks)))
    # block_to_conds[block_id] = Vector{Tuple{SSAValue, Bool}}(undef, 0)
    preds_no_backward = filter(x -> !is_latch(cfg.blocks[x], x), block_cfg.preds)
    # preds_no_backward = block_cfg.preds
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

function fix_stmt(ssavalue, stmt, ir, old_to_new_ssavalue)
  if stmt.expr isa GotoIfNot
    new_stmt = GotoIfNot(old_to_new_ssavalue[stmt.expr.cond], stmt.expr.dest)
    push!(ir, new_stmt)
  elseif stmt.expr isa GotoNode
    push!(ir, stmt)
  elseif stmt.expr isa ReturnNode
    push!(ir, ReturnNode(old_to_new_ssavalue[stmt.expr.val]))
  elseif stmt.expr isa PhiNode
    push!(ir, stmt)
    new_ssavalue = SSAValue(length(ir.defs))
    old_to_new_ssavalue[ssavalue] = new_ssavalue
  elseif stmt.expr isa PiNode
    push!(ir, stmt)
    new_ssavalue = SSAValue(length(ir.defs))
    old_to_new_ssavalue[ssavalue] = new_ssavalue
  elseif stmt.expr isa Expr
    new_args = map(x->if x isa SSAValue old_to_new_ssavalue[x] else x end,stmt.expr.args)
    new_stmt = Expr(:call, new_args[1], new_args[2:length(new_args)]...)
    push!(ir, new_stmt)
    new_ssavalue = SSAValue(length(ir.defs))
    old_to_new_ssavalue[ssavalue] = new_ssavalue
  end
end

function pass_create_masks(ir)
  block_to_conds = get_conditions_for_blocks(ir)
  negated_conditions = NegatedConditions()
  block_to_cond = Dict{Int64, Union{SSAValue, Argument}}()
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  forward_new_ir = IR(ir.lines, ir.args)
  for b in blocks(ir)
    if haskey(block_to_conds, b.id)
      conds = block_to_conds[b.id]
      current_cond = get_condition(conds[1]..., old_to_new_ssavalue, negated_conditions, forward_new_ir)
      for i in range(2,length=length(conds)-1)
        cond = get_condition(conds[i]..., old_to_new_ssavalue, negated_conditions, forward_new_ir)
        push!(forward_new_ir, xcall(SPMD, :and_mask, current_cond, cond))
        current_cond = SSAValue(length(forward_new_ir.defs))
      end
      block_to_cond[b.id] = current_cond
    end
    for (ssavalue, stmt) in b
      fix_stmt(ssavalue, stmt, forward_new_ir, old_to_new_ssavalue)
    end
    if (b.id != length(blocks(ir)))
      block!(forward_new_ir)
    end
  end
  forward_new_ir = map(x->fix_phinode(x, old_to_new_ssavalue), forward_new_ir)
  # old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  # backward_new_ir = IR(ir.lines, ir.args)
  # for (b, b_cfg) in zip(blocks(forward_new_ir), CFG(forward_new_ir).blocks)
  #   if is_header(b_cfg, b.id)
  #     done = false
  #     for (ssavalue, stmt) in b
  #       if done
  #         fix_stmt(ssavalue, stmt, backward_new_ir, old_to_new_ssavalue)
  #       else
  #         is_arg = block_to_cond[b.id] isa Argument
  #         is_ssa_and_after_mask = block_to_cond[b.id] isa SSAValue && (block_to_cond[b.id].id < ssavalue.id)
  #         if !(stmt.expr isa PhiNode) && is_arg || is_ssa_and_after_mask
  #           done = true
  #           latch_block = filter(x->is_latch(CFG(forward_new_ir).blocks[x], x), b_cfg.preds)[1]
  #           loop_mask = PhiNode(b_cfg.preds, [block_to_cond[b.id], block_to_cond[latch_block]])
  #           push!(backward_new_ir, loop_mask)
  #           new_ssavalue = SSAValue(length(backward_new_ir.defs))
  #           block_to_cond[b.id] = new_ssavalue
  #         end
  #         fix_stmt(ssavalue, stmt, backward_new_ir, old_to_new_ssavalue)
  #       end
  #     end
  #   else
  #     for (ssavalue, stmt) in b
  #       fix_stmt(ssavalue, stmt, backward_new_ir, old_to_new_ssavalue)
  #     end
  #   end
  #   if (b.id != length(blocks(ir)))
  #     block!(backward_new_ir)
  #   end
  # end
  # new_ir = map(x->fix_phinode(x, old_to_new_ssavalue), backward_new_ir)
  # (new_ir, block_to_cond)
  (forward_new_ir, block_to_cond)
end

# function pass_create_masks(ir)
#   cfg = CFG(ir)
#   new_ir = IR(ir.lines, ir.args)
#   old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
#   block_to_block_cond = Dict{Tuple{Int64, Int64}, SSAValue}
#   for (b, b_cfg) in zip(blocks(ir), cfg.blocks)
#     preds = b_cfg.preds
#     if length(preds) > 1
#     succs = b_cfg.succs

function pass_if(ir, block_to_cond)
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
    for (ssavalue, stmt) in b
      if stmt.expr isa PhiNode
        if is_header(b_cfg, b.id)
          push!(new_ir, stmt)
          new_ssavalue = SSAValue(length(new_ir.defs))
          old_to_new_ssavalue[ssavalue] = new_ssavalue
          continue
        end
        edges = reverse(stmt.expr.edges)[2:length(stmt.expr.edges)]
        conds = map(x -> old_to_new_ssavalue[block_to_cond[x]], edges)
        values = reverse(map(x -> old_to_new_ssavalue[x], stmt.expr.values))
        second_cond = conds[1]
        first_val, second_val = values[1], values[2]
        push!(new_ir, xcall(SPMD, :select, second_cond, second_val, first_val))
        new_ssavalue = SSAValue(length(new_ir.defs))
        for (condition, value) in zip(conds[2:length(conds)], values[3:length(values)])
          push!(new_ir, xcall(SPMD, :select, condition, value, new_ssavalue))
          new_ssavalue = SSAValue(length(new_ir.defs))
        end
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      elseif stmt.expr isa ReturnNode
        push!(new_ir, ReturnNode(old_to_new_ssavalue[stmt.expr.val]))
      elseif isgoto(stmt.expr)
        if is_latch(b_cfg, b.id) | is_header(b_cfg, b.id)
          if stmt.expr isa GotoIfNot
            new_stmt = GotoIfNot(old_to_new_ssavalue[stmt.expr.cond], stmt.expr.dest)
            push!(new_ir, new_stmt)
            block!(new_ir)
          else
            push!(new_ir, stmt)
            block!(new_ir)
          end
        end
      elseif stmt.expr isa Expr
        new_args = map(x->if x isa SSAValue old_to_new_ssavalue[x] else x end,stmt.expr.args)
        new_stmt = Expr(:call, new_args[1], new_args[2:length(new_args)]...)
        push!(new_ir, new_stmt)
        new_ssavalue = SSAValue(length(new_ir.defs))
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      elseif stmt.expr isa PiNode
        if stmt.expr.val isa SSAValue
          old_to_new_ssavalue[ssavalue] = old_to_new_ssavalue[stmt.expr.val]
        else
          #TODO
        end
      end
    end
  end
  new_ir = map(x->fix_phinode(x, old_to_new_ssavalue, old_to_new_block), new_ir)
  (new_ir, old_to_new_block, block_to_cond)
end

function pass_call(ir::IR, block_to_cond)
  new_ir = IR(ir.lines, ir.args)
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if stmt.expr isa Expr
        is_and_mask(x) = x == GlobalRef(SPMD, :and_mask)
        is_not_mask(x) = x == GlobalRef(SPMD, :not_mask)
        is_or_mask(x) = x == GlobalRef(SPMD, :or_mask)
        new_stmt = nothing
        funct = stmt.expr.args[1]
        if is_and_mask(funct) || is_not_mask(funct) || is_or_mask(funct)
          new_stmt = stmt
        else
          new_stmt = xcall(SPMD, :spmd, block_to_cond[b.id], stmt.expr.args...)
        end
        # new_stmt = xcall(SPMD, :spmd, stmt.expr.args[1], block_to_cond[b.id], stmt.expr.args[2:length(stmt.expr.args)]...)

        push!(new_ir, new_stmt)
      else
        push!(new_ir, stmt)
      end
    end
    if (b.id != length(blocks(ir)))
      block!(new_ir)
    end
  end
  new_ir
end

unwraptype(::Type{Vec{T,N}}) where {T,N} = T
unwraptype(x) = x

function pass(ir)
  ir, block_to_cond = pass_create_masks(ir)
  ir = pass_call(ir, block_to_cond)
  ir, old_to_new_block = pass_if(ir, block_to_cond)
  ir = pass_loops_result_value(ir, block_to_cond)
  ir = pass_for_loops_gotos(ir, old_to_new_block)
  ir
end

@generated function spmd(mask, f, args...)
  m = meta(Tuple{f,unwraptype.(args)...})
  ir = IR(m)
  ir = pass(ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(spmd)), (:mask, mask))
  ir = varargs!(m, ir, 3)
  update!(m, ir)
  return m.code
end

using IRTools: @code_ir

function f(x)
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

function d(x)
  a = x + 1
  for i in 1:x
    a += x
  end
  a
end

# function naive_spmd(f, args)
#   map(f, args)
# end
# code = @code_ir f(6)
# println(code)
# pass(code) |> println
# println(pass_if(code)...)
# println(pass_for_loops_gotos(pass_if(code)...))


# println(pass_call(pass_for_loops_gotos(pass_if(code)...)))
# println(spmd(vect(true,true,true,true), f, vect(5,5,5,5)))
# println(@code_ir spmd(vect(true, true), d, vect(3,4)))
# using BenchmarkTools
#
# input = Vector{Int16}(repeat([256], 8))
#
# println(spmd(f, vect(1,6,11,-1)))
#
# @btime spmd(g, Vec{Int16, 8}(256))
# @btime naive_spmd(g, input)
# println(spmd(g, vect(3,4,5,10)))
# println(naive_spmd(g, input))

using InteractiveUtils
using InteractiveUtils: typesof

macro code_spmd(ex)
  @capture(ex, f_(xs__)) || error("@code_spmd f(xs...)")
  :(IRTools.code_ir($(esc(f)), typesof($(xs...))) |> pass)
end
