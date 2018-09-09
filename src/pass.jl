spmd(::typeof(println), xs...) where {T,N} = println(unvect.(xs)...)
spmd(::typeof(print), xs...) where {T,N} = print(unvect.(xs)...)

function spmd(f, args...)
  if any(isvect, args)
    tospmd(f, args...)
  else
    f(args...)
  end
end

function modifier(stmt)

end

function pass(stmt)
  if isexpr(stmt, :call)
    stmt = xcall(SPMD, :spmd, stmt.args...)
  end
  stmt
end

@generated function tospmd(f, args...)
  m = meta(Tuple{f,map(datatype, args)...})
  ir = IR(m)
  ir = varargs!(m, ir)
  argnames!(m, :f, :args)
  ir = spliceargs!(m, ir, (Symbol("#self#"), typeof(tospmd)))
  ir = map(pass, ir)
  update!(m, ir)
  return m.code
end
