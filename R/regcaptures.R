regcaptures <-
function(x, m) {
    if (length(x) != length(m)) 
        stop(gettextf("%s and %s must have the same length", 
            sQuote("x"), sQuote("m")), domain = NA)
    if(is.list(m)) {
        n <- length(m)
        rval <- vector("list", n)
        for(i in seq_len(n)) {
            cs <- attr(m[[i]], "capture.start")
            cl <- attr(m[[i]], "capture.length")
            rval[[i]] <-array(substring(x[[i]], cs, cs + cl - 1L), dim = dim(cs))
        }
    } else {
        cs <- attr(m, "capture.start")
        cl <- attr(m, "capture.length")
        rval <- array(substring(x, cs, cs + cl - 1L), dim = dim(cs))
    }
    rval
}