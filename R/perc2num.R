
perc2num <-
function(x, which = attr(x,"unit") == "%", box) {
    
    if(!any(which)) return(x)
    if(is.null(names(x))) stop("'x' must be a named vector")
    k <- match(names(x[which]), names(.perc2num.map), nomatch = 0L)
    j <- which & k != 0L
    if(!any(j)) return(x)

    i <- .perc2num.map[k]
    if(any(i == 3L))
        box <- c(box[1L:2L], sqrt(sum(box^2)) / 1.414214) # sqrt(2)

    x[j] <- x[j] * box[i]
    
    if(!is.null(attr(x,"unit")))
        attr(x,"unit")[j] <- ""
    x
}


.perc2num.map <-
c(x = 1L, y = 2L, cx = 1L, cy = 2L,
  width = 1L, height = 2L,
  rx = 1L, ry = 2L, r = 3L,
  x1 = 1L, x2 = 1L, y1 = 2L, y2 = 2L,
  refx = 1L, refy = 2L,
  markerwidth = 1L, markerheight = 1L
  )