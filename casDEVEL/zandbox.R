setwd("casDEVEL/caracas/")
load_all()




caracas:::as_expr_worker(x)






x <- as_sym(c("a", "b"))

e <- as_expr(x)


s  <- as_sym("[[r1,r2,r3], [u1,u2,u3]]")
s2 <- apply(as_character_matrix(s), 2, function(x) (paste("1/(", x, ")")))
as_sym(s2)




  xx1 <- as.character(x)
  xx1 <- remove_mat_prefix(xx1)  
  xx1 ## "[[a, c, e], [b, d, f]]"
  
  
  xx2 <- gsub("^\\[(.*)\\]$", "\\1", xx1)
  xx2
  
  xx3 <- strsplit(xx2, "\\],")[[1]]
  for (i in 1:(length(xx3)-1))
    xx3[i] <- paste(xx3[i], "]")
  xx3 ## "[a, c, e ]" " [b, d, f]"
  
  xx4 <- gsub("[[:space:]]*\\[(.*)\\][[:space:]]*", "\\1", xx3)
  xx4 ##  "a, c, e " "b, d, f" 
  
  vv <- strsplit(xx4, ",")
  vv
  
  ww <- lapply(vv, function(v) paste("(", v, ")", sep="") )
  ww
  
  xx <- lapply(ww, function(w) paste0(num, "/", w))
  xx

  xx2 <- lapply(xx, function(x) paste(x, collapse=", "))
  xx2


  yy <- lapply(xx2, function(x) {paste("[", x, "]", collapse=", ")})
  yy

  zz <- unlist(yy) 
  zz

  out<- paste("Matrix([", paste0(zz, collapse=", "), "])")
out

  as_sym(out)






s <- "[[[r1],[r2],[r3]], [[u1],[u2],[u3]]]"



x <- g
s <- g$pyobj
s <- h$pyobj




identical(substring(s, 1, 3), "[[[")




s2 <- do_unbracket(s)
s2


do_unbracket(s2)

s3 <- do_split_rows(s2)

s4 <- list(unlist(s3[1:2]), unlist(s3[3:4]))

s4 %>% do_comma %>% do_bracket %>% do_comma %>% do_bracket %>% as_sym






rrr <- lapply(s3, function(r) paste("1/(",r,")"))

rr0 <- do_comma(rrr)
rr1 <- do_bracket(rr0)
rr2 <- do_comma(rr1)
rr3 <- do_bracket(rr2)
as_sym(rr3)









## strip outer (extra) []:
s2 <- gsub("\\[(.+?)\\]", "\\1", s) 
s2
# "[[r1,r2,r3], [u1,u2,u3]]"

# "[[r1,r2,r3 ]" " [u1,u2,u3]]"

## drop []



## drop blands
out <- lapply(out, function(o) gsub("[[:space:]]*", "", o))



rrr <- s2
rrr













out










rr0 <- lapply(rrr, paste, collapse=",")



rr2 <- paste0(rr1, collapse=", ")

as_sym(do_bracket(rr2))







  out<- paste0("Matrix(", paste0(rr2, collapse=", "), ")")
out

as_sym(out)


eval_to_symbol(rr2)

  







  xx1 <- as.character(x)
  xx1 <- remove_mat_prefix(xx1)  
  xx1 ## "[[a, c, e], [b, d, f]]"

  identical(substring(xx1, 1, 3), "[[[")



gsub("\\[(.+?)\\]", "\\1", xx1)





  gsub(xx1, "\\[{1}(.*)", "\\1")


  xx2 <- gsub("^\\[(.*)\\]$", "\\1", xx1)
  xx2
 
  xx3 <- strsplit(xx2, "\\]\\],")[[1]]
  for (i in 1:(length(xx3)-1))
    xx3[i] <- paste(xx3[i], "]]")
  xx3 ## "[a, c, e ]" " [b, d, f]"





  xx4 <- gsub("[[:space:]]*\\[(.*)\\][[:space:]]*", "\\1", xx3)
  xx4 ##  "a, c, e " "b, d, f" 





  vv <- strsplit(xx4, ",")
  vv
  
  ww <- lapply(vv, function(v) paste("(", v, ")", sep="") )
  ww
  
  xx <- lapply(ww, function(w) paste0(num, "/", w))
  xx

  xx2 <- lapply(xx, function(x) paste(x, collapse=", "))
  xx2


  yy <- lapply(xx2, function(x) {paste("[", x, "]", collapse=", ")})
  yy

  zz <- unlist(yy) 
  zz

  out<- paste("Matrix([", paste0(zz, collapse=", "), "])")
out

  as_sym(out)















x / (x+y)

load_all("caracas")

r <- reciprocal_matrix(den)

B <- as_sym("[[x, a], [a, x**2]]")
x <- B

num <- 1
x <- den











p <- as_sym(paste0("p", 1:3))
r <- 2 * p
r <- 0 / p


A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
B <- as_sym(A)


reciprocal_matrix(B)







[x^(-2)]] ])")

a<-expect_equal(Bchar, "Matrix([[[1/x], [1.00000000000000]], [[0.500000000000000], [x^(-2)]]])")






r <- as_sym(paste0("r", 1:3))





num <- 1






  
  expect_equal(as.character(det(B)), "x^3 - 2")




out <- paste("Matrix([",paste0("[", paste0("1/",uu), "]", collapse=", "),"])")
as_sym(out)







reciprocal_matrix(B)




as_sym("[[2], [2], [2]]") / as_sym("[[p1], [p2], [p3]]")





load_all("caracas"); p * r

load_all("caracas"); p / r


as.character(r)


symbol_is_matrix(r)


xstr <- "[[r1], [r2], [r3]]"

z<-as_sym(xstr)





reciprocal_matrix(r)





uu


zz <- strsplit(gsub("\\(\\[(.*)\\]\\)$", "\\1", xstr), ",")







grepl("^Matrix\\(\\[\\[.*\\]\\]\\)$", xstr)



inv(as_sym("Matrix([[r1], [r2]])"))


