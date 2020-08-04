less <- function(a){
  fn <- paste(tempdir(),"\\dataframe.txt",sep='',collapse='')
  write.table(a, quote=F, file=fn)
  system(paste("less ",fn))
}

