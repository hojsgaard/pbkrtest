.First <-
function () 
{
    cat("Calling .First() in .Rprofile\n")
    options(repos = c(CRAN = "http://cran.dk.r-project.org", 
        CRANextra = "http://www.stats.ox.ac.uk/pub/RWin"))
    cat("exiting .First()\n")
}
.Last <-
function () 
{
    cat("Calling .Last() in .Rprofile\n")
}
