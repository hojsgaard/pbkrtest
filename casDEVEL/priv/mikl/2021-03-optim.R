obj_f <- function(XY) { 
  print(unname(XY))
  
  (XY[["X"]] + 1)^4 + (XY[["Y"]] - 2)^4
}

# min = c(-1, 2)

optim(c(X = 1, Y = 1), obj_f, method = "BFGS", 
      hessian = TRUE)


# min = c(0.001, 1.5) 
optim(c(X = 1, Y = 1), obj_f, method= "L-BFGS-B", 
      lower = c(0.001, 0.001), upper = c(1.5, 1.5), 
      hessian = TRUE)

