.First <-
function () 
{
    cat("Calling .First() in .Rprofile\n")
    cat("libPaths(): \n")
    print(.libPaths())
    options(repos = "https://cran.rstudio.com/")
    library(shTools)
    library(devtools)
    library(Rcpp)
}
.Last <-
function () 
{
    cat("Calling .Last() in .Rprofile\n")
}
