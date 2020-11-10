
#Perhaps something like this could be added to "Writing R extensions":

make_initc <- function(name){

    if (!dir.exists(name))
        stop(paste0("dir ", name, " does not exist"))

    sink(paste0(name, "/src/init.c"))
    tools::package_native_routine_registration_skeleton(name)
    sink()
    ## sink("gRbase/src/init.c")
    ## tools::package_native_routine_registration_skeleton("gRbase")
    ## sink()

}


