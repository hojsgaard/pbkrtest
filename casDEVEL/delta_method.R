library(caracas)

options(caracas.print.method = "prettyascii")


def_sym(x1, x2)
V <- matrix_sym_symmetric(2)
V

z <- 2*x1 + 4*x2
vrs <- c(x1, x2)  |> to_matrix()

g <- t(matrix_(c(2, 4)))

g %*% V %*% t(g)

g <- der(z, vrs) |> to_matrix()
g
class(g)
t(g) %*% V %*% g

z <- x1 / x2
g <- der(z, vrs) |> to_matrix()
Vz <- (t(g) %*% V %*% g)  |> simplify()
Vz

V. = subs(V, all_vars(V), c(2,1,2))

Vz <- (t(g) %*% V. %*% g)  |> simplify()
