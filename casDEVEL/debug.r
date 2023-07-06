
library(caracas)
load_all("caracas")

M <- as_sym(toeplitz(c("a", "b", 0))) ## as_sym() converts an R object to caracas symbol
Minv <- inv(M) %>% simplify()
M

d <- det(M)
d

Mi <- inv(M)
M * d
Mi * d



M
d <- det(M)
d

M * (M / d)
Minv * d




