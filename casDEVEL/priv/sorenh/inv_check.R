## Matrix inversions; various alternatives to inv()

library(caracas)
source("inv_caracas_ryacas.R")

Ac <- as_sym(toeplitz(letters[1:3]))
Ac

Ai1 <- inv(Ac) %>% simplify()
Ai2 <- inv_cf(Ac) %>% simplify()
Ai3 <- inv_lu(Ac) %>% simplify()
Ai4 <- inv_yac(Ac) %>% simplify()

## (Ai1 - Ai2)
## (Ai1 - Ai3) %>% simplify()
## (Ai1 - Ai4) %>% simplify()

(Ac %*% Ai1) %>% simplify()
(Ac %*% Ai2) %>% simplify()
(Ac %*% Ai3) %>% simplify()
(Ac %*% Ai4) %>% simplify()


## 2x2
B2 <- as_sym(toeplitz(letters[1:2]))

## 3x3
B3 <- as_sym(toeplitz(letters[1:3]))

## 4x4
B4 <- as_sym(toeplitz(letters[1:4]))

## 5x5
B5 <- as_sym(toeplitz(letters[1:5]))

## 6x6
B6 <- as_sym(toeplitz(letters[1:6]))

## 7x7
B7 <- as_sym(toeplitz(letters[1:7]))

library(microbenchmark)
mb <- microbenchmark(
    inv_yac(B2), 
    inv_yac(B3), 
    inv_yac(B4), 
    inv_yac(B5), 
    inv_yac(B6),
    inv_yac(B7), 
    times=3
)
tt <- doBy::summary_mb(mb)

## Benchmark
library(microbenchmark)
mb <- microbenchmark(
    inv_lib(B2), inv_yac(B2), 
    inv_lib(B3), inv_yac(B3), 
    inv_lib(B4), inv_yac(B4), 
    inv_lib(B5), inv_yac(B5), 
    inv_lib(B6), inv_yac(B6), 
    times=3
)
doBy::summary_mb(mb)
