
## scaleBy <- function(formula, data, center=TRUE, scale.=TRUE){
##   createFunBy(formula, data, FUN=scale, center=center, scale=scale.)
## }





## scaleBy <- function(formula, data, center=TRUE, scale=TRUE){

##   lhs      <- formula[[2]]
##   lh.var   <- .lhsParse(lhs)
##   if (length(lh.var)==1)
##     lh.var <- list(lh.var)
##   lh.var   <- paste(unlist(lh.var))	
##   rh.var   <- .rhsVar(formula)
  
##   data.var <- names(data)
##   cls      <- lapply(data, class)
  
##   if (length(lh.var)==1 && lh.var=="."){		
##     num.var   <- data.var[  cls %in% c("numeric","integer")]
##     lh.var    <- setdiff(num.var, rh.var)
##     #cat(sprintf("deriving numeric lh.var: %s\n", toString(lh.var))) 
##     if (length(lh.var)==0)
##       stop("There are no numeric variables to scale")
##   } else {
##     #cat(sprintf("using specified numeric lh.var: %s\n", toString(lh.var))) 
##   }
  
##   if (length(rh.var)==1 && rh.var=="."){		
##     fac.var <- data.var[!(cls %in% c("numeric","integer"))]
##     if (length(fac.var)>0){
##       #cat(sprintf("stratifying by non-numerics fac.var: %s\n", toString(fac.var)))
##       rh.var    <- fac.var 
##       rh.data   <- data[, rh.var, drop=FALSE]
##       rh.string <- apply(rh.data, 1, paste, collapse="@")
##       rh.unique <- unique(rh.string)
##     } else {
##       #cat(sprintf("no non--numerics to stratify by\n"))
##       rh.string <- rep.int(1, NROW(data))
##       rh.unique <- 1 
##     }
##   } else {
##     #cat(sprintf("stratifying by specified rh.var: %s\n", toString(rh.var)))    
##     rh.data   <- data[, rh.var, drop=FALSE]
##     rh.string <- apply(rh.data, 1, paste, collapse="@")
##     rh.unique <- unique(rh.string)
##   }
  
##   lh.idx   <- match(lh.var, names(data))
##   lh.data  <- as.matrix(data[,lh.var,drop=FALSE])
  
##   for (ii in seq_along(rh.unique)){	
##     idx  <- rh.unique[ii] == rh.string
##     sc   <- scale(lh.data[idx,,drop=F], center=center, scale=scale)
##     lh.data[idx,] <- sc
##   }
  
##   data[,lh.idx] <- lh.data
##   data
## }	


