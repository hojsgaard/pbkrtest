#'
#' Calculate "time since event" in a vector
#'
#' Calculates the time since the nearest event in a sequence,
#' optionally using a custom time scale.
#'
#' Events are coded as 1 (or TRUE). Non-events are anything else. The
#' result includes absolute and signed distances to events.
#'
#' @param yvar A numeric or logical vector indicating events.
#' @param tvar An optional numeric vector specifying time values. Defaults to the index.
#' @return A data frame with columns 'yvar', 'tvar', 'abs.tse' (absolute time since event), 
#'         'sign.tse' (signed time since event), and other helper columns.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{subSeq}}, \code{\link{rle}}
#' @examples
#' 
#' ## Example 1: Basic usage with default time index
#' y <- c(0, 0, 1, 0, 0, 1, 0)
#' tse <- time_since_event(y)
#' print(tse)
#'
#' ## Example 2: Custom (non-integer) time variable
#' y <- c(0, 0, 1, 0, 0, 0, 1, 0)
#' t <- seq(0.5, 3.5, length.out = length(y))
#' tse <- time_since_event(y, t)
#' print(tse)
#'
#' ## Example 3: Plotting the signed time since event
#' plot(sign.tse ~ tvar, data = tse, type = "b",
#'      main = "Signed time since event",
#'      xlab = "Time", ylab = "Signed time since event")
#' grid()
#' abline(h = 0, col = "red", lty = 2)
#' 
#' @export 
time_since_event <- function(yvar, tvar=seq_along(yvar)) {

    if (!(is.numeric(yvar) | is.logical(yvar))) {
        stop("yvar must be either numeric or logical")
    }
    
    yvar[is.na(yvar)] <- 0
    
    event.idx <- which(yvar == 1)
    
    if (length(event.idx) == 0) {
        return(NULL)
    }
    
    n.event <- length(event.idx)

    ## find event times
    event.time <- tvar[event.idx]

    ## get time difference to each event
    rrr <-  do.call(rbind, lapply(event.idx, function(ii) tvar-tvar[ii]))
    abs.tse <- apply(abs(rrr), 2, min)


    ## get the event windows (~ symmetrical around event time)
    ewin<-rep.int(NA, length(yvar))
    if (n.event > 1) {
        ff <- event.time[1:(n.event-1)] + diff(event.time) / 2
        ewin[tvar <= ff[1]] <- 1
        for (ii in 2:(length(ff) - 0)){
            ewin[tvar > ff[ii - 1] & tvar <= ff[ii] ] <- ii
        }
        ewin[tvar > ff[length(ff)]] <- n.event
    } else {
        ewin[] <- n.event
    }

  ## get the signs
    ggg <- list()
    for (ii in 1:(length(event.idx))){
        ggg[[ii]] <- rrr[ii, ewin == ii]
    }
    ggg <- unlist(ggg)
    sign.tse <- sign(ggg) * abs.tse

    run <- cumsum(yvar)

    un <- unique(run)
    tlist <- list()
    for (ii in 1:length(un)){
        vv <- un[ii]
        yy <- yvar[run == vv]
        tt <- tvar[run == vv]
        tt <- tt - tt[1]
        tlist[[ii]] <- tt
    }
    tae <- unlist(tlist)
    tae[run == 0] <- NA

    yvar2 <- rev(yvar)
    tvar2 <- rev(tvar)

    run2 <- cumsum(yvar2)
    un2 <- unique(run2)
    tlist2 <- list()
    for (ii in 1:length(un2)){
        vv <- un2[ii]
        yy <- yvar2[run2 == vv]
        tt <- tvar2[run2 == vv]
        tt <- tt - tt[1]
        tlist2[[ii]] <- tt
  }
    tbe <- unlist(tlist2)
    tbe[run2==0] <- NA

    tbe <- rev(tbe)
    run[run==0]<-NA
    
    ans <- cbind(data.frame(yvar=yvar, tvar=tvar), abs.tse, sign.tse, ewin=ewin,
                 run, tae=tae, tbe=tbe)
    ans
}






## yvar, tvar=seq_along(yvar)

#' @rdname time_since_event
#' @param ... Arguments pased on to time_since_event
#' @export
timeSinceEvent <- function(...) {
    .Deprecated("time_since_event")
    time_since_event(...)
}




