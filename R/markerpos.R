markerpos <-
function(path, type, orient) {
    type <- match.arg(type, choices = c("start", "mid", "end"))
    if(is.character(orient))
        orient <- match.arg(orient, choices = c("auto", "auto-start-reverse"))    

    vert <- 
    switch(attr(path, "svg:name"), path = {
        k <- cumsum(attr(path, "seglen"))
        if(length(k) == 0L) return(NULL)
        # Shift by 1 at each row of NAs:
        br <- cumsum(is.na(path[, 1L]))
        k + br[k]
    }, line = , polyline =, polygon = {
        seq_len(nrow(path)) 
    }, return(NULL))
    
    nvert <- length(vert)
    
    message(type)
    #TODO 'auto-start-reverse'
    #If placed by marker-start, the marker is oriented 180° different from the
    #orientation that would be used if 'auto' where specified.
    
    
    if(is.numeric(orient)) {
        phi <- orient
    } else {
        
        if(type == "mid" && nvert <= 2L) return(NULL)
    
        if(type != "end") {
            sdxy <- path[vert[-nvert] + 1L, ] - path[vert[-nvert], , drop = FALSE]
            sdir <- angleBetween2(matrix(c(1, 0), nrow(sdxy), 2L, byrow = TRUE), sdxy)
            l0 <- !is.na(sdxy[, 1L]) & rowSums(sdxy == 0) == 2L
        }
        if(type != "start") {
            edxy <- path[vert[-1L], ] - path[vert[-1L] - 1L, , drop = FALSE]
            edir <- angleBetween2(matrix(c(1, 0), nrow(edxy), 2L, byrow = TRUE), edxy)
        }
        
        phi <- 180 / pi *
            switch(type, mid = {
                # Directions of zero-length segments:
                # https://www.w3.org/TR/SVG/paths.html#PathDirectionality
                if(any(l0)) {
                    if(all(l0)) {
                        sdir[] <- edir[] <- pi / 2
                    } else {
                        pos <- prec <- foll <- seq_along(l0)
                        last <- NA
                        for(i in pos) if(l0[i]) prec[i] <- last else last <- pos[i] 
                        last <- NA
                        for(i in rev(pos)) if(l0[i]) foll[i] <- last else last <- pos[i]   
                        i <- l0 & is.na(prec)
                        sdir[i] <- edir[i] <- sdir[foll[i]]
                        i <- l0 & !is.na(prec)
                        sdir[i] <- edir[prec[i]]
                        i <- l0 & is.na(foll)
                        edir[i] <- edir[prec[i]]
                        i <- l0 & !is.na(foll)
                        edir[i] <- sdir[foll[i]]
                    }
                }
                mapply(angular.mean, sdir[-1L], edir[1L - nvert])
            }, start = {
                if(any(l0)) {
                    if(all(l0)) pi / 2
                        else sdir[which(!l0)[1L]]
                } else sdir[1L]
            }, end = {
                l0 <- !is.na(edxy[, 1L]) & rowSums(edxy == 0) == 2L
                if(any(l0)) {
                    if(all(l0)) pi / 2
                        else {
                        nonzero <- which(!l0)
                        edir[nonzero[length(nonzero)]]
                    }
                } else edir[nvert - 1L]
            }, return(NULL)) # -> phi
        } # orient
    
    k <- switch(type, mid = -c(1L, nvert), start = 1L, end = nvert)
    cbind(path[vert[k], , drop = FALSE], dir = phi)
}
