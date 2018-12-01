using Core.Compiler: PhiNode, GotoNode, GotoIfNot, ReturnNode, SSAValue, BasicBlock, Argument, PiNode
using IRTools

struct FutureLoopMask end

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

_select(conds::NTuple{0}, first_vals::NTuple{0}, second_vals::NTuple{0}) = ()
function _select(conds::NTuple{N}, first_vals, second_vals) where {N}
  cond = first(conds).value
  v1 = first(first_vals)
  v2 = first(second_vals)
  other_conds = Base.tail(conds)
  other_v1s = Base.tail(first_vals)
  other_v2s = Base.tail(second_vals)
  (cond ? v1 : v2, _select(other_conds, other_v1s, other_v2s)...)
end

function select(conds::Mask{N}, first_vals::AbstractVec{T,N}, second_vals::AbstractVec{S,N}) where {T,S,N}
  res = _select(conds.data, first_vals.data, second_vals.data)
  vect(res...)
end

function select(conds::Mask{N}, first_val::T, second_val::S) where {T <: ScalarTypes, S <: ScalarTypes, N}
  if any(conds)
    first_val
  else
    second_val
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
  new_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if stmt.expr isa Expr
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
      else
        fix_stmt(ssavalue, stmt, new_ir, old_to_new_ssavalue)
      end
    end
    if (b.id != length(blocks(ir)))
      block!(new_ir)
    end
  end
  new_ir = map(x->fix_phinode(x, old_to_new_ssavalue), new_ir)
  new_ir, old_to_new_ssavalue
end

and_mask(masks...) = (&)(masks...)
or_mask(masks...) = (|)(masks...)
not_mask(mask) = ~mask

function fix_stmt(ssavalue, stmt, ir, old_to_new_ssavalue)
  replacement(x::SSAValue) = old_to_new_ssavalue[x]
  replacement(x) = x
  if stmt.expr isa GotoIfNot
    new_stmt = GotoIfNot(old_to_new_ssavalue[stmt.expr.cond], stmt.expr.dest)
    push!(ir, new_stmt)
  elseif stmt.expr isa GotoNode
    push!(ir, stmt)
  elseif stmt.expr isa ReturnNode
    push!(ir, ReturnNode(replacement(stmt.expr.val)))
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
  cfg = CFG(ir)
  forward_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  block_to_block_mask = Dict{Tuple{Int64, Int64}, Union{SSAValue, Argument}}()
  block_to_block_mask[(0,1)] = Argument(0)
  cfg.blocks[1] = SPMD.BasicBlock(cfg.blocks[1].stmts, [0], cfg.blocks[1].succs)
  block_to_mask = Dict{Int64, Union{SSAValue, Argument}}()
  for (b, b_cfg) in zip(blocks(ir), cfg.blocks)
    forward_preds = filter(k-> k == 0 || k < b.id, b_cfg.preds)
    backward_preds = filter(k-> k != 0 && k > b.id, b_cfg.preds)
    succs = b_cfg.succs
    if length(backward_preds) == 0
      incoming_masks = map(x->block_to_block_mask[(x,b.id)], forward_preds)
      if length(incoming_masks) == 0
        continue
      end
      cond = incoming_masks[1]
      for other_cond in incoming_masks[2:end]
        push!(forward_ir, xcall(SPMD, :or_mask, cond, other_cond))
        cond = SSAValue(length(forward_ir.defs))
      end
      block_to_mask[b.id] = cond
    else
      push!(forward_ir, FutureLoopMask())
      block_to_mask[b.id] = SSAValue(length(forward_ir.defs))
    end
    for (ssavalue,stmt) in b
      if !iscontrol(stmt.expr)
        fix_stmt(ssavalue, stmt, forward_ir, old_to_new_ssavalue)
      end
    end
    if length(succs) == 1
      block_to_block_mask[(b.id, succs[1])] = block_to_mask[b.id]
    elseif length(succs) > 1
      cond = old_to_new_ssavalue[b.bb.gotos[1].expr.cond]
      else_dest = b.bb.gotos[1].expr.dest
      if_dest = b.id + 1
      push!(forward_ir, xcall(SPMD, :and_mask, block_to_mask[b.id], cond))
      if_cond = SSAValue(length(forward_ir.defs))
      block_to_block_mask[(b.id, if_dest)] = if_cond
      push!(forward_ir, xcall(SPMD, :not_mask, cond))
      not_cond = SSAValue(length(forward_ir.defs))
      push!(forward_ir, xcall(SPMD, :and_mask, block_to_mask[b.id], not_cond))
      else_cond = SSAValue(length(forward_ir.defs))
      block_to_block_mask[(b.id, else_dest)] = else_cond
    end
    for goto in b.bb.gotos
      fix_stmt(nothing, goto, forward_ir, old_to_new_ssavalue)
    end
    if (b.id != length(blocks(ir)))
      block!(forward_ir)
    end
  end
  forward_ir = map(x->fix_phinode(x, old_to_new_ssavalue), forward_ir)
  backward_ir = IR(ir.lines, ir.args)
  for (b, b_cfg) in zip(blocks(forward_ir), cfg.blocks)
    for (ssavalue, stmt) in b
      if stmt.expr isa FutureLoopMask
        preds = b_cfg.preds
        conds = map(k->block_to_block_mask[(k,b.id)], preds)
        loop_mask = PhiNode(preds, conds)
        push!(backward_ir, loop_mask)
      else
        push!(backward_ir, stmt.expr)
      end
    end
    if (b.id != length(blocks(ir)))
      block!(backward_ir)
    end
  end
  backward_ir, block_to_mask, block_to_block_mask
end

function pass_from_phi_to_select(ir, block_to_block_mask)
  new_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  for (b, b_cfg) in zip(blocks(ir), CFG(ir).blocks)
    for (ssavalue, stmt) in b
      if stmt.expr isa PhiNode
        if is_header(b_cfg, b.id)
          push!(new_ir, stmt)
          new_ssavalue = SSAValue(length(new_ir.defs))
          old_to_new_ssavalue[ssavalue] = new_ssavalue
          continue
        end
        conds = map(x -> old_to_new_ssavalue[block_to_block_mask[(x,b.id)]], stmt.expr.edges)
        values = map(x -> old_to_new_ssavalue[x], stmt.expr.values)
        second_cond = conds[2]
        first_val, second_val = values[1], values[2]
        push!(new_ir, xcall(SPMD, :select, second_cond, second_val, first_val))
        new_ssavalue = SSAValue(length(new_ir.defs))
        for (condition, value) in zip(conds[3:end], values[3:end])
          push!(new_ir, xcall(SPMD, :select, condition, value, new_ssavalue))
          new_ssavalue = SSAValue(length(new_ir.defs))
        end
        old_to_new_ssavalue[ssavalue] = new_ssavalue
      else
        fix_stmt(ssavalue, stmt, new_ir, old_to_new_ssavalue)
      end
    end
    if (b.id != length(blocks(ir)))
      block!(new_ir)
    end
  end
  map(x->fix_phinode(x, old_to_new_ssavalue), new_ir)
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

function pass_delete_gotos(ir, block_to_block_mask)
  new_ir = IR(ir.lines, ir.args)
  old_to_new_ssavalue = Dict{SSAValue, SSAValue}()
  cfg = CFG(ir).blocks
  for b in blocks(ir)
    for (ssavalue, stmt) in b
      if stmt.expr isa GotoIfNot
        continue
      elseif stmt.expr isa GotoNode
        if stmt.expr.label < b.id
          mask = block_to_block_mask[(b.id, stmt.expr.label)]
          push!(new_ir, xcall(SPMD, :not_mask, mask))
          not_mask = SSAValue(length(new_ir.defs))
          push!(new_ir, xcall(:all, not_mask))
          new_ssavalue = SSAValue(length(new_ir.defs))
          new_stmt = GotoIfNot(new_ssavalue, stmt.expr.label)
          push!(new_ir, new_stmt)
          block!(new_ir)
        end
      else
        fix_stmt(ssavalue, stmt, new_ir, old_to_new_ssavalue)
      end
    end
    if (b.id+1 <= length(cfg) && is_header(cfg[b.id+1], b.id+1))
      block!(new_ir)
    end
  end
  map(x->fix_phinode(x, old_to_new_ssavalue), new_ir)
end

unwraptype(::Type{SVec{T,N}}) where {T,N} = T
unwraptype(x) = x

function pass(ir)
  ir, block_to_cond, block_to_block_mask = pass_create_masks(ir)
  ir = pass_call(ir, block_to_cond)
  ir = pass_from_phi_to_select(ir, block_to_block_mask)
  ir, old_to_new_ssavalue = pass_loops_result_value(ir, block_to_cond)
  block_to_block_mask = Dict(k=>v isa SSAValue ? old_to_new_ssavalue[v] : v for (k,v) in block_to_block_mask)
  ir = pass_delete_gotos(ir, block_to_block_mask)
  ir
end

@generated function spmd(mask, f, args...)
  f <: Core.IntrinsicFunction && return :(error("Can't SPMD intrinsic $f"))
  T = Tuple{f,unwraptype.(args)...}
  m = meta(T)
  m == nothing && return :(error($"Can't SPMD $T"))
  ir = IR(m)
  ir = pass(ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(spmd)), (:mask, mask))
  ir = varargs!(m, ir, 3)
  update!(m, ir)
  return m.code
end

using InteractiveUtils
using InteractiveUtils: typesof

macro code_spmd(ex)
  @capture(ex, f_(xs__)) || error("@code_spmd f(xs...)")
  :(IRTools.code_ir($(esc(f)), typesof($(xs...))) |> pass)
end
