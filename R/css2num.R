#The attached unit identifier is case insensitive. There is never a space or any
#other characters between a the number and the unit identifier: i.e. 1 cm is not
#valid.
# Angle units are converted to degrees
# Absolute length units are converted to px
# Percentages are converted to fractions
css2num <-
function(x) {
    
    .kwds <- c("left" = 0, "center" = 0.5, "right" = 1, "top" = 0, "bottom" = 1)

    m <- gregexpr("([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)([%a-zA-Z]*)", x, perl = TRUE)
    bad <- is.na(m) | vapply(lapply(m, "==", -1L), isTRUE, FALSE)
    
    iskwd <- match(x[bad], names(.kwds), 0L)
   
    if(all(bad) && all(iskwd == 0L)) {
        rval <- rep_len(NA_real_, length(x))
        attr(rval, "unit") <- rep.int("NA", length(x)) 
        attr(rval, "element") <- seq_along(x)
        return(rval)
    }
    
    matches <- regcaptures(x[!bad], m[!bad])
    #nmatch <- insert(vapply(matches, nrow, integer(1L)), which(bad), 1L)
    #elt <- rep(seq_along(nmatch), nmatch)
    matches <- do.call(rbind, matches)
    units <- tolower(matches[, 2L])
    k <- match(units, names(.unitconv))
    f <- .unitconv[k]
    f[na <- is.na(f)] <- 1
    rval <- as.numeric(matches[, 1L]) * f
    #rval <- as.numeric(sapply(matches, "[", 2L)) * f
    units <- names(.unitconv)[k]
    units[na] <- ""
    nm <- sapply(m, length)
    rbad <- which(rep(bad, nm))
    rval <- insert(rval, rbad, NA)
    attr(rval, "unit") <- insert(units, rbad, "NA")
    names(rval) <- rep(names(x), nm)

    if(any(iskwd != 0L)) {
        rval[bad][k <- iskwd != 0L] <- .kwds[iskwd]
        attr(rval, "unit")[bad][k] <- "%"
    }
    
    #rep(seq_along(x), sapply(m, length))
    #split(rval, rep(seq_along(x), sapply(m, length)))
    #if(length(x) != 1L)
    attr(rval, "element") <- rep.int(seq_along(x), nm)
    rval
}

.unitconv <- c(
  "%" = 0.01,
  # Angle units --> degrees
  c(deg = 1, rad = 180 / pi, grad = 360 / 400,
    turn = 360), # degrees are default
  #Absolute length units --> px
  c(
  cm = 96 / 2.54,
  mm = 96 / 2.54 / 10,
  Q =  96 / 2.54 / 40,
  "in" = 96,
  pc = 96 / 16,
  pt = 96 / 72,
  px = 1
  ) # * 1 px
 )
