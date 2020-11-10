## Approximate matching; find the values of 'table' which are
## closest to those of 'x'. Return NA when the numeric difference
## is larger than 'eps'.

## Should also handle NA's in the arguments.

## Candidate for doBy package???

approxmatch <- function(x, table, eps=0.1){
  ans <- unlist(lapply(x, function(xx){
    which.min(abs(xx-table))
  }))
  ans[abs(table[ans]-x)>eps]<-NA
  ans
}
