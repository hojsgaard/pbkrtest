library(caracas)

W <- matrix_sym(2, 2, "w")
S <- matrix_sym(2, 2, "s")

trace(W)


der(W, all_vars(W)) ## Strange behaviour

lapply(all_vars(W), function(w) der(W, w))


der(W, all_vars(W)[1])



caracas:::vars_to_array(all_vars(W))
