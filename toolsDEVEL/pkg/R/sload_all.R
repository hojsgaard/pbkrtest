sload_all <- function(path="."){
    if (!dir.exists(path)) 
        stop(paste0("path ", path, " does not exist\n"))
    cat("** removing *.o\n")
    file.remove(list.files(file.path(paste0(path, "/src")), pattern="*\\.o", full.names=T))
    cat("** removing *.so\n")
    file.remove(list.files(file.path(paste0(path, "/src")), pattern="*\\.so", full.names=T))
    cat("** creating init.c\n")
    make_initc(path)
    cat("** compileAttributes\n")
    compileAttributes(path)
    cat("** load_all\n")
    load_all(path)
}
