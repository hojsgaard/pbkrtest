library(caracas)

integrand <- function(x) {1/((x+1)*sqrt(x))}
integrate(integrand, lower = 0, upper = Inf)

x <- symbol("x")
f <- as_sym(paste(body(integrand)[2]))
f <- 1 / ((x+1)*sqrt(x))
int(f, x, 0, Inf)



sympy <- reticulate::import("sympy")
i <- sympy$integrate("a*x", c("x", -1, 2))
i
i$evalf(subs = list(a=1))

# or use the function "latex" for printing in LaTeX
sympy$latex(i)








W <- matrix_sym(2, 2, "w")
S <- matrix_sym(2, 2, "s")

load_all()
tr <- trace_(W)
tr$pyobj |> as.character()

?def_sym

def_sym(x,y,z)

z <- x^2 +y^3
dd <- der(z,c(x,y))
dd$pyobj

v <- vector_sym(3)

is_matrix(v)
symbol_class(v)

der(W, all_vars(W)) ## Strange behaviour

lapply(all_vars(W), function(w) der(W, w))


der(W, all_vars(W)[1])



caracas:::vars_to_array(all_vars(W))

## Vector
s <- as_sym("[2*x, 3*y**2]")
symbol_class(s)

s <- as_sym("[[2*x, 3*y**2]]")
symbol_class(s)


s <- as_sym("(2*x, 3*y**2)")
symbol_class(s)

s  <- as_sym("[[r1, r2, r3], [u1, u2, u3]]")



