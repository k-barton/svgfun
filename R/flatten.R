flatten.list <-
function(x) {
  repeat {
    k <- vapply(x, is.list, logical(1L))
    if(!any(k)) return(x)
    n <- length(x)
    m <- as.integer(!k)
    m[k] <- vapply(x[k], length, integer(1L))
    l <- rep.int(k, m)
    out <- vector("list", sum(m))
    out[!l] <- x[!k]
    out[l] <-  u <- unlist(x[k], recursive = FALSE, use.names = TRUE)
    names(out)[!l] <- names(x)[!k]
    names(out)[l] <- names(u)
    x <- out
  }
}

