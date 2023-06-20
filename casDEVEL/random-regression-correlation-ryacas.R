library(Ryacas)
v1 <- ysym("v1")
v2 <- ysym("v2")
c <- ysym("c")
S <- rbind(c(v1, c), c(c, v2))
S


w <- ysym("w")
xt <- ysym("{{1, t}}")
v_t <- xt %*% S %*% t(xt) + w^2
v_t <- simplify(v_t[1, 1])
v_t

xtk <- ysym("{{1, t+k}}")
v_tk <- xtk %*% S %*% t(xtk) + w^2
v_tk <- simplify(v_tk[1, 1])
v_tk


cv_t_tk <- (xt %*% S %*% t(xtk))
cv_t_tk <- simplify(cv_t_tk[1, 1])
cv_t_tk


k <- ysym("k")
cv2 <- cv_t_tk * (1/k)


ss <- strsplit(as.character(cv_t_tk), "\\+")[[1]]

ss2 <- paste0(ss, "/k")

ss3 <- lapply(ss2, function(s) yac_str(paste0("Simplify(",s, ")")))

ss4 <- paste(unlist(ss3), collapse="+")

ss5 <- as_y(ss4)

ss6 <- paste0("Limit(k, Inf)",ss5)
ss6
yac_str(ss6)


lim(ss5, "k", Inf)


cv2 <- simplify(cv2)
cv2

lim(cv2, "k", Inf)

yac_str("MaxEvalDepth(10000)")


corr_t_tk <- cv_t_tk / sqrt( v_t * v_tk)
corr_t_tk






## y: (v1+t^2*v2+2*t*c+t*v2*k+c*k)/Sqrt((w^2+v1+t^2*v2+2*t*c)*(w^2+v1+t^2*v2+2*t*k*v2+2*t*c+k^2*v2+2*k*c))
Assume that t = 0:
  c_t0 <- with_value(corr_t_tk, "t", 0)
c_t0
## y: (v1+c*k)/Sqrt((w^2+v1)*(w^2+v1+k^2*v2+2*k*c))
v1 + ck
p
(w2 + v1) (w2 + v1 + k
            2v2  + 2kc)
lim(c_t0, "k", Inf)
