.First <-
function () 
{
    cat("Calling .First() in .Rprofile +++ ...\n")
    .libPaths(c("~/R/x86_64-pc-linux-gnu-library/4.3", .libPaths()))
    cat("libPaths(): \n")
    print(.libPaths())
    Sys.setenv(MC_CORES = 4)
    Sys.setenv(`_R_BUILD_COMPACT_VIGNETTES_` = "gs+qpdf")
    Sys.setenv(`_R_CHECK_FORCE_SUGGESTS_` = 0)
}
.Last <-
function () 
{
    cat("Calling .Last() in .Rprofile\n")
}
