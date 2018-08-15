using SPMD, Test
using SPMD: roundtrip

add(a, b) = a+b
_relu(x) = x > 0 ? x : 0
f(a, b...) = +(a, b...)

@test roundtrip(add, 1, 2) == 3
@test roundtrip(_relu, 1) == 1
@test roundtrip(Complex, 1, 2) == 1+2im
@test roundtrip(f, 1, 2, 3) == 6
