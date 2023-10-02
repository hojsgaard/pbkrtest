seqPBmodcomp <-
    function(largeModel, smallModel, h = 20, nsim = 1000, seed=NULL, cl=NULL, details=0) {
        t.start <- proc.time()
        chunk.size <- 50
        nchunk <- nsim %/% chunk.size
        LRTstat <- getLRT(largeModel, smallModel)
        ref <- NULL
        for (ii in 1:nchunk) {
            ref <- c(ref, PBrefdist(largeModel, smallModel, nsim = chunk.size, seed=seed, cl=cl, details=details))
            n.extreme <- sum(ref > LRTstat["tobs"])
            if (n.extreme >= h)
                break
        }
        ans <- PBmodcomp(largeModel, smallModel, ref = ref)
        ans$ctime <- (proc.time() - t.start)[3]
        ans
    }
