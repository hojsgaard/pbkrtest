headf <- function(x,n=6){
  instr <- sprintf("head -%i %s",n,x)
  aa<-system(instr, T)
  cat(paste(aa,"\n"))
}
