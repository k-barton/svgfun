#cat(paste0("#' @param ", unlist(lapply(list(vgline, vgellipse, vgrect, vgpolyline),
#              function(x) names(formals(x))))), sep = "\n")
vgline <-
function(x) {
    xy <- matrix(x, 2L, 2L)
    class(xy) <- c("vgshape", "vgelement", class(xy))
    attr(xy, "svg:name") <- "line"
    xa <- c(xy)
    names(xa) <- c("x1", "x2", "y1", "y2")
    attr(xy, "svg:xml-attrs") <- xa
    xy
}

vgellipse <-
function(cx, cy, rx, ry = rx, n = 64L, ...) {
    #a <-  c(seq(0,  n - 1, by = 2 * pi / (n - 1)), 0)
    a <- c(seq.int(0, length.out = n - 1, by = 2 * pi / (n - 1)), 0)
	xy <- cbind(
		x = rx * cos(a) + cx,
		y = ry * sin(a) + cy
	)
    is.circle <- rx == ry
    class(xy) <- c("vgshape", "vgelement", class(xy))
    attr(xy, "svg:name") <- if(is.circle) "circle" else "ellipse"
    xa <- c(cx, cy, if(is.circle) rx else c(rx, ry))
    names(xa) <- c("cx", "cy", if(is.circle) "r" else c("rx", "ry"))
    attr(xy, "svg:xml-attrs") <- xa 
    xy
}


vgrect <-
function(x, y, width, height = width, rx = 0, ry = 0, f = 0.554, ...) {
    
    if(rx == 0) rx <- ry
        else if(ry == 0) ry <- rx
    w2 <- width / 2
    rx <- min(w2, rx)
    ry <- min(height / 2, ry)
    
    #message(paste(c(width, height, rx, ry), collapse = " "))
    if(is.sharp <- rx == 0 && ry == 0) {
        xy <- matrix(c(x + c(w2, width, width, 0, 0, w2),
             y + c(0, 0, height, height, 0, 0)), 6L, 2L,
                dimnames = list(NULL, c("x", "y")))
        class(xy) <- c("vgshape", "vgelement", class(xy))
    } else  {
       w <-  width - 2 * rx
       w2 <- w / 2
       xy <- vgpath(paste(c("
        m", x + rx + w2, y,
        w2, ",0
        c ", f * rx, ",0 ", rx, ry * (1 - f), " ", rx,  ry, "
        l 0,", height - 2 * ry, "
        c 0,", f * ry, -rx * (1 - f), ry, -rx, ry, "
        l", -w, ",0
        c ", -f * rx, ",0", -rx,-ry * (1 - f), -rx,-ry, "
        l 0,", -height + 2 * ry, "
        c", 0,-f * ry, rx * (1 - f),-ry, rx,-ry, "
        l ", w2,  "0
        Z"), collapse = " "), ...)
    }
    attr(xy, "svg:name") <- "rect"
    sym.corners <- is.sharp || rx == ry
    xa <- c(x, y, width, height,
        if(!is.sharp) if(sym.corners) rx else c(rx, ry))
    names(xa) <- c("x", "y", "width", "height",
        if(!is.sharp) if(sym.corners) "rx" else c("rx", "ry"))
    attr(xy, "svg:xml-attrs") <- xa
    xy
}

vgpolyline <-
function(points, closed = FALSE) {
    if(is.character(points))
        points <- listofnumbers(points)
    xy <- matrix(as.numeric(points), ncol = 2L, byrow = TRUE,
        dimnames = list(NULL, c("x", "y")))
    if(closed <- isTRUE(closed))
        xy <- rbind(xy, xy[1L, , drop = FALSE], deparse.level = 0L)
    class(xy) <- c("vgshape", "vgelement", class(xy))
    attr(xy, "svg:name") <- if(closed) "polygon" else "polyline"
    attr(xy, "svg:xml-attrs") <- c(points = paste(points, collapse = " "))
    xy
}

vgtext <-
function(x, y, label, size = 1, rotation = 0L,
        family = c("sans", "serif", "mono"),
        weight = c("normal", "bold"),
        style = c("normal", "italic"),
        col = 1
        ) {
    xy <- matrix(c(x, y), ncol = 2L, dimnames = list(NULL, c("x", "y")))
    class(xy) <- c("vgtext", "vgelement", class(xy))
    attr(xy, "svg:text") <- label
    attr(xy, "svg:name") <- "text"
    attr(xy, "svg:rotation") <- rotation
    attr(xy, "svg:size") <- size
    xa <- c(x, y)
    names(xa) <- c("x", "y")
    attr(xy, "svg:xml-attrs") <- xa
    
    if(is.numeric(col)) col <- rgb2col(col2rgb(col))
    
    # family = "serif", "sans" and "mono"
    # font 1 = plain text, 2 bold, 3 italic, 4 bold italic. 5 symbol font, in Adobe symbol encoding.
    xy
}

print.vgtext <-
function (x, ...) {
    cat("Graphics text object: \n")
    cat(dQuote(attr(x, "svg:text")), "\n")
    invisible(x)
}


style.vgtext <-
function(x, ...) {
    .NotYetImplemented()
    x
}

`style<-.vgtext` <-
function(x, overwrite = FALSE, value) {
    .NotYetImplemented()
    x
}

vgtransform.vgtext <- 
function(x, rotate = 0, translate = c(0, 0), scale = c(1, 1),
    skew = c(0, 0), origin = c(0, 0), matrix. = NULL) {
    attr(x, "svg:rotation") <- (attr(x, "svg:rotation") + rotate) %% 360
    attr(x, "svg:size") <- abs(attr(x, "svg:size") * scale[1L])
    x
}

plot.vgtext <- function(x, add = FALSE, ...) {
    .NotYetImplemented()
    label <- attr(x, "svg:text")
    cex <- attr(x, "svg:size")
    if(!isTRUE(add)) {
        cex <- 1
        label <- "sqeweW"
        strwidth(label, cex = cex, font = 1, units = "f", srt = 90)
        strheight(label, cex = cex, font = 1, units = "f")
    }
    text(x, labels = attr(x, "svg:text"), adj = c(0, 0),
        srt = attr(x, "svg:rotation"),
        cex = attr(x, "svg:size"))  
}
