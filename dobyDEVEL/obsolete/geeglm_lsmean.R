#require(pda)

lsmean.geeglm <- function( object, data = eval( object$call$data ), 
                       factors, ..., 
                       rdf = 1000,     #df.resid( object ), 
                       coef = object$coefficients, 
                       cov = summary(object)$cov.scaled )
{
  if( !missing( factors ) & missing( rdf )) {
    ## rdf <- df.resid( object, factors = factors )
    rdf <- 1000
    lsmean.lm( object, data, factors, ..., rdf = rdf, coef = coef, cov = cov )
  }
  else
    lsmean.lm( object, data, ..., rdf = rdf, coef = coef, cov = cov )
}

# lsmean.gee <- function( object, data = eval( object$call$data ), 
#                        factors, ..., 
#                        rdf = 1000,     #df.resid( object ), 
#                        coef = object$coefficients, 
#                        cov = object$robust.variance )
# {
#   if( !missing( factors ) & missing( rdf )) {
#     ## rdf <- df.resid( object, factors = factors )
#     rdf <- 1000
#     lsmean.lm( object, data, factors, ..., rdf = rdf, coef = coef, cov = cov )
#   }
#   else
#     lsmean.lm( object, data, ..., rdf = rdf, coef = coef, cov = cov )
# }


##############################################################################
