library(caracas)
source("lin_alg3.R")


U <- matrix_sym_power(4, c(0,1,2))
V <- matrix_sym_power(4, c(0,1,3))
B <- intersectionspace(U,V)

nullspace(t(B))


B1 <- matrix_sym_power(4, 0:1)

nullspace(t(B1))

t(B) |> as_character_matrix()
t(B1) |> as_character_matrix()

sympy_func(t(B), "nullspace")

B2 <- as_sym(as_character_matrix(B))

sympy_func(t(B), "nullspace")

sympy_func(t(B2), "nullspace")

sympy_func(t(B1), "nullspace")

