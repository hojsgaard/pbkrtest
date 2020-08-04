matrx <- function(mm){
  paste(apply(mm,1,function(rr){paste(rr, collapse="& ")}), collapse="\\")
}
