#' # Symbolic evaluations with `sympy`
#'
#' ### Mikkel Meyer Andersen and Søren Højsgaard
#' 
#' This example is provided by Mikkel Meyer Andersen and
#' modified/expanded slightly by Søren Højsgaard.
#'
#' The reticulate package provides acces to the python lab 'sympy'
#'
 
library(reticulate)
sympy <- reticulate::import("sympy")

#' ### Helper functions

#' A little helper to get from R-syntax to sympy-syntax
to_sympy <- function(x){
    x <- as.character(x)
    x <- gsub("pi", "Pi", x, fixed=TRUE)
    x <- gsub("^", "**", x, fixed=TRUE)
    x
}

#' A little helper to get from sympy-syntax to R-syntax
to_r <- function(x) {
  x <- as.character(x)
  x <- gsub("Pi", "pi", x, fixed = TRUE)
  x <- gsub("**", "^", x, fixed = TRUE)
  x <- parse(text = x)
  return(x)
}

#' A little helper to save some typing:
sym_parse <- sympy$parsing$sympy_parser$parse_expr

#' ### Solving quadratic equation

## Solve x**2 + x - y = 0 for x
## Need to tell sympy about symbols to operate on:
x <- sympy$symbols('x')

eqns_str <- "x**2 + x - y"
eqns <- sym_parse(eqns_str)
eqns
sol <- sympy$solve(eqns,  list(x), dict = TRUE)
unlist(sol)


#' ### Systems of (linear) equations

## Solve for x, y, z:
## x + y + z  = 1
## x + y + 2z = 3
## Need to tell sympy about symbols to operate on:
x <- sympy$symbols('x')
y <- sympy$symbols('y')
z <- sympy$symbols('z')

eqns_str <- "[x + y + z - 1, x + y + 2*z - 3]"
eqns <- sym_parse(eqns_str)
sol <- sympy$solve(eqns,  list(x, y, z), dict = TRUE)
sol <- sol[[1]]
sol

#' ### Lagrangian for tin can problem

#' Area and volume as fns of diameter and height
area <- expression(pi/2 * d^2 + pi * h * d)
vol  <- expression(pi/4 * d^2 * h)

#' just in case you don't know:
eval(area, list(d=2, h=3))
eval(vol, list(d=2, h=3))

## Need to tell sympy about symbols to operate on:
d <- sympy$symbols('d')
h <- sympy$symbols('h')
lam <- sympy$symbols('lam')

area_str <- to_sympy(area)
vol_str <- to_sympy(vol)

## Alternative:
## area_str <- "Pi/2 * d**2 + Pi * h * d"
## vol_str  <- "Pi/4 * d**2 * h"

lagrange_str  <- paste0("(", area_str, ") - lam*((", vol_str, ") - 1)")
lagrange_str
## Alternative:
## lagrange_str <- "(Pi/2 * d**2 + Pi * h * d) - lam*((Pi/4 * d**2 * h) - 1)"

#' The variables we solve by:
vars  <- list('d' = d, 'h' = h, 'lam' = lam)

#' An intermediate representation
lagrange <- sym_parse(lagrange_str)
lagrange

#' Gradient
grad <- sympy$derive_by_array(lagrange, list(d, h, lam))
grad

#' Solve for d, h, lam
sol <- sympy$solve(grad, list(d, h, lam), dict = TRUE)
sol
sol[[1]]

sol_d <- to_r(sol[[1]][["d"]])
sol_d
sol_d_num <- eval(sol_d)
sol_d_num

sol_h <- to_r(sol[[1]][["h"]])
sol_h
sol_h_num <- eval(sol_h)
sol_h_num

## Numerical evaluation of area and volume
eval(vol, list(d=eval(sol_d), h=eval(sol_h)))
eval(area, list(d=eval(sol_d), h=eval(sol_h)))

