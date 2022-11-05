plot.vgshape <-
function (x,
    nodes = !is.null(attr(x, "nodes")),
    add = FALSE, axes = TRUE,
    col = NULL, border = NULL,
    asp = 1, xlim = NULL, ylim = NULL,
    rule = "evenodd",
    marker.scale = 0.62,
    ...) {
    
    rule <- match.arg(rule, choices = c("winding", "evenodd"))

    if(nodes && attr(x, "svg:name")[1L] != "path")
        nodes <- FALSE
    
    if((nodes <- isTRUE(nodes)) && is.null(attr(x, "nodes"))) {
        warning(paste0(sQuote("x"), " has no attribute ", sQuote("nodes")))
        nodes <- FALSE
    }
   
    if(nodes) {
        Tmat <- attr(x, "transform.applied")
        trans <- if(is.null(Tmat)) identity else
            function(x) vgtransform(x, matrix. = Tmat)
        
        nd <- attr(x, "nodes")
        n <- sapply(nd, length)
        arcnodes <- which(names(nd) %in% c("a", "A") & n == 7L)
        if(hasArcs <- length(arcnodes) != 0L) {

            arcs <- do.call("cbind", nd[arcnodes])
            
            arcs.cxy <- vapply(nd[arcnodes], attr, numeric(2L), "center")
            arcs.p1 <- vapply(nd[arcnodes - 1L],
                function(x) x[seq.int(length(x) - 1L, length.out = 2L)],
                numeric(2L))
            arcs <- rbind(arcs.p1, arcs.cxy, arcs)
            rownames(arcs) <- c("x1", "y1", "cx", "cy", "rx", "ry", "rotation",
                "largeArc", "sweep", "x2", "y2")

            t <- seq(0, 1, length.out = 5L)
            arclim <- arcs2 <- vector("list", length(arcnodes) * 2L)

            for (i in seq_along(arcnodes)) {
                a <- arcnodes[i]
                for(k in seq_len(2L)) {
                    ik <- (i * 2L) - 2L + k
                    arc1 <- arc(arcs[c("x1", "y1"), i], arcs[c("x2", "y2"), i],
                            arcs[c("rx", "ry"), i], arcs["rotation", i],
                            TRUE, k == 1L, t)
                    arclim[[ik]] <- .xylimits(trans(arc1))
                    arcs2[[ik]] <- c(attributes(arc1)[c("center", "rxy")],
                        rotation = arcs["rotation", i]) 
                } #k
            } #i
            
            nd[arcnodes] <- lapply(nd[arcnodes], "[", 6L:7L)
            n[arcnodes] <- 2L
                        
        } # hasArcs
        nd <- matrix(unlist(nd), byrow = TRUE, ncol = 2L)
        nodetype <- rep.int(2L, nrow(nd))
        nodetype[cumsum(n %/% 2L)] <- 1L
        nd <- trans(nd)
    } # nodes
    
    # TODO xy-lim for vgcontainer with nodes = TRUE respecting "arclim"
    
    if(!isTRUE(add)) {
        lim <- .xylimits(x)
        if(nodes) {
            lim <- .xylimits(rbind(lim, .xylimits(nd)))
            if(hasArcs)
                lim <- .xylimits(rbind(.xylimits(do.call("rbind", arclim)), lim))
        }
        plot.new()
        plot.window(if(is.null(xlim)) lim[, 1L] else xlim,
                    if(is.null(ylim)) lim[2L:1L, 2L] else ylim,
                    asp = asp)
        if(isTRUE(axes)) {
            axis(1L)
            axis(2L)
            box()
        }
    }
       
    #if(is.null(style(x))) {
    #    # XXX: Should use R's par or SVG default style?
    #    gpar <- par(c("fg", "bg", "lwd", "lend", "ljoin", "lty"))
    #    names(gpar) <- c("col", "border", "lwd", "lend", "ljoin", "lty")
    #    gpar[["rule"]] <- rule
    #} else {
        gpar <- css2par(style(x))
    #}
    
    
    lwd <- lend <- ljoin <- lty <- NULL
    dots <- list(...)
    for(a in c("lwd", "lend", "ljoin", "lty"))
        assign(a, dots[[a]], inherits = FALSE)
    
    for(a in c("col", "border", "rule", "lwd", "lend", "ljoin", "lty")) {
        if(is.null(get(a, inherits = FALSE)))
            assign(a, gpar[[a]], inherits = FALSE)
        #cat(a, ":", get(a, inherits = FALSE), "\n")
    }
    
    n <- nrow(x)

    isClosed <- all(x[1L, ] == x[n, ])
    
    xy <- if(isClosed)
        rbind(x, x[2L, ]) # slower with deparse.level = 0, why?
        else x
    # XXX much slower: x[c(seq_len(n), 1L), , drop = FALSE]
        
    polypath(xy, col = col, lty = 0L, rule = rule)
    if(!is.na(border) && lwd != 0)
        lines(xy, lwd = lwd, col = border,
            ljoin = ljoin, lend = lend, lty = lty)        

    plotMarkers(x, scale = lwd * marker.scale, flwd = 3.5)
    #plotMarkers(x, scale = lwd * marker.scale, flwd = 3.5,
        #border = border, col = border)
    
    if(nodes) {
        i <- 2L:nrow(nd)
        j <- 1L + which(nodetype[i] != nodetype[i - 1L])
        segments(nd[j, 1L], nd[j, 2L], nd[j - 1L, 1L], nd[j - 1L, 2L], lty = 2L)
        points(nd, 
            pch = ifelse(nodetype == 1L, 22, 21),
            bg = ifelse(nodetype == 1L, "#000000cc", "white")
        )
        
        if(hasArcs) {
            alpha <- arcs["rotation", ] / 180 * pi
            ca <- cos(alpha)
            sa <- sin(alpha)
            
            seg1 <- trans(t(arcs[c("cx", "cy"), , drop = FALSE]))
            seg2 <- trans(cbind(arcs["cx", ] + c(arcs["rx", ] * ca, arcs["ry", ] * -sa),
                       arcs["cy", ] + c(arcs["rx", ] * sa, arcs["ry", ] * ca)))
                  
            segments(seg1[,1L], seg1[,2L], seg2[,1L], seg2[,2L], lty = 3)
            points(seg1[, 1L], seg1[, 2L], pch = 21, bg = "red")
            dt <- pmax(strwidth("00"), .33 * arcs["rx", ])
            txy <- trans(cbind(arcs["cx", ] - (dt * ca), arcs["cy", ] - (dt * sa)))
            
            text(txy, labels = sprintf("%g\u00B0", arcs["rotation", ]))
            for(a in arcs2) {
                o <- draw.arc(a$center[1L], a$center[2L], a$rxy[1L], a$rxy[2L],
                    rotation = a$rotation / 180 * pi, plot. = FALSE)
                polygon(trans(o), lty = 2L)
            }
        }  
    }
    
    invisible()
}

plot.vgcontainer <-
function(x, nodes = FALSE,
        #ids = FALSE,
        flwd = 1.7, marker.scale = .62,
        asp = 1,
        col = NULL, border = NULL,
        add = FALSE, axes = TRUE, ann = FALSE,
        xlab = NULL, ylab = NULL,
        xlim = NULL, ylim = NULL,
          ...) {

    #is.tree <- any(vapply(x, inherits, logical(1L), "vgcontainer"))
    n <- sum(rapply(x, classes = "vgshape", is.matrix, how = "unlist"))

    dots <- list(...)
    style <- dots[names(dots) %in% c("rule", "lwd", "lend", "ljoin", "lty")]
    for(a in c("col", "border")) style[[a]] <- get(a, inherits = FALSE)
    for(a in c("col", "border", "rule", "lwd", "lend", "ljoin", "lty"))
        if(!is.null(style[[a]]))
            style[[a]] <- rep_len(style[[a]], n)
            
    if(!add) {            
        lim <- range(x)
        if(is.null(xlim)) xlim <- lim[, 1L]
        if(is.null(ylim)) ylim <- lim[2L:1L, 2L]
        plot(lim, type = "n", xlim = xlim, ylim = ylim, asp = asp,
             axes = axes, ann = ann, xlab = xlab, ylab = ylab)
    }

    .count <- 0L
    rapply(x, classes = "vgshape", function(path) {
        
       colorref <- getDef(path, "colorref")
        .count <<- i <- .count + 1L
        gpar <- css2par(attr(path, "svg:style"), colorref) # flwd
        gpar[names(style)] <- lapply(style, "[[", i)
        
        if(!gpar$show) return()

        attr(path, "svg:style") <- NULL
        
        plot(path, col = gpar$col, rule = gpar$rule,
             lwd = gpar$lwd * flwd, border = gpar$border,
             ljoin = gpar$ljoin, lend = gpar$lend, lty = gpar$lty,
             nodes = nodes, add = TRUE,
             marker.scale = marker.scale)
    })
    
            
        #if(ids) {
        #    id <- attr(xy, "svg:attr")["id"]
        #    if(!is.null(id) && !is.na(id)) {
        #        text(min(xy[, 1L]), max(xy[, 2L]), id, cex = .666)
        #    }
        #}
    invisible()
}
