
poly_div2 <- function(x) {
    num <- numerator(x)
    den <- denominator(x)
    sympy_func(num, "div", den)
}

poly_div <- function(num, denom) {
    sympy_func(num, "div", denom)
}


def_sym(x)
f = 5 * x**2 + 10 * x + 3
g = 2 * x + 2
