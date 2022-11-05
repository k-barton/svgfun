
.localenv <- new.env()

.getdefaultmarkers <-
function() {
    if(!exists("Markers", .localenv, mode = "list", inherits = FALSE)) {
        fi <- system.file("svg", "markers.svg", package = .packageName, mustWork = TRUE)
        assign("Markers", read.svg(fi, flat = FALSE), .localenv)
    }
    get("markers", attr(get("Markers", .localenv, inherits = FALSE), "defs"),
        inherits = FALSE)
}

defaultMarkers <-
function(plot. = TRUE) {

    markers <- .getdefaultmarkers()
    lab <- names(markers)

    if(!isTRUE(plot.)) return(lab)

    op <- par(mar = c(0,0,2,0))
    on.exit(par(op))
    
    n <- length(markers)
    lim1 <- lapply(markers, range)
    lim <- array(unlist(lim1), dim = c(2L, 2L, length(lim1)))
    lim <- apply(apply(lim, 1L:2L, range), 3L, diag)
    asz <- abs(apply(lim, 2L, diff))
    pin <- par("pin")
    pr <- pin[2L] / pin[1L]
    x <- sapply(1L:n, function(nc) { 
        nr<- (n %/% nc) + (n %% nc > 0)
        fsz <- asz * c(nc, nr)
        fsz[2L] / fsz[1L]
    })
    nc <- which.min(abs((pr / x) - 1))
    nr <- (n %/% nc) + (n %% nc > 0)
    fsz <- asz * c(nc, nr)
    ii <- seq_len(n)
    xpos <- (((ii - 1L) %% nc) + .5) * asz[1L]
    ypos <- (((ii - 1L) %/% nc) + .5) * asz[2L]
    cxy <- t(vapply(markers, function(x) apply(range(x), 2, mean), numeric(2)))
   
    plot.new()
    plot.window(c(0, fsz[1]), c(fsz[2], 0),  asp = 1, xaxs = "i", yaxs = "i")
    segments(xpos - asz[1] / 2, ypos - cxy[, 2],
        xpos + asz[1] / 2, ypos - cxy[, 2], lty = 3)
    for(i in ii) {
        plot(vgtransform(markers[[i]],
            translate = c(xpos[i], ypos[i]) - cxy[i, ]),
            add  = TRUE)
        points(rbind(c(xpos[i], ypos[i])- cxy[i, ]), pch = 20,
            col = "#ff990099")
    }
    cex <- asz[1] / max(strwidth(lab)) * .98
    text(xpos, ypos + asz[2] / 2.5, lab, cex = cex)
    rect(xpos - asz[1] / 2, ypos - asz[2] / 2,
        xpos + asz[1] / 2, ypos + asz[2] / 2)

    title("Default path markers:")
    invisible(lab)
}
