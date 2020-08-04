
## Find those entries of elements of x where these elements are in
## one of the intervals given by lim.
## No checks are made as to the ordering of lim.
## NA's are handled properly
##
## Example:
## x   <- 1:20
## lim <- rbind(c(2,5),c(10,15))
## lim <- c(2,5,10,15)
## lim <- list(c(2,5), c(10,15))
## inInterval(x,lim)

inInterval <- function(x, lim, interval="closed"){
  interval <- match.arg(tolower(interval), c("closed","open","left.open","right.open"))
  switch(interval,
         "closed"    ={leftop <- `>=`
                       rightop <- `<=`},
         "open"      ={leftop <- `>`
                       rightop <- `<`},
         "left.open" ={leftop <- `>`
                       rightop <- `<=`},
         "right.open"={leftop <- `>=`
                       rightop <- `<`})
  
  if (class(lim)=="numeric"){ 
    lim <- matrix(lim, nc = 2, byrow=TRUE)
  } else {
    if (class(lim)=="list") 
      lim <- do.call(rbind, lim)   
  }
  ans <- rep.int(0, length(x))
  for (ii in 1:nrow(lim)){
    ##    ans <- ans + print(x >= lim[ii,1] & x <= lim[ii,2])
    ans <- ans + leftop(x, lim[ii,1]) & rightop(x, lim[ii,2])
  } 
  return(ans)
}




## inInterval <- function(x, lim, index=FALSE){

##   if (inherits(lim,"numeric"))
##     lim <- matrix(lim, nr=1)

##   lim <- split(lim, row(lim))

##   ans <- lapply(lim,
##                 function(a) which((x>= a[1] & x<=a[2])))

##   if (!index)
##     ans <- lapply(ans, function(a) x[a])

##   if (length(ans)==1)
##     ans <- unlist(ans)

##   return(ans)
## }
