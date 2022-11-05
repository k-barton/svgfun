plotMarkers <-
function(path, scale = 1, ...) {
    
    mref <- attr(path, "svg:marker")
    if(is.null(mref) || length(mref) == 0L) return()

    markers <- if(exists("markers", attr(path, "defs"), inherits = FALSE))
        get("markers", attr(path, "defs"), inherits = FALSE) else
        return()

    mref <- cssurl(mref)
    mref <- mref[mref %in% names(markers)]
    if(length(mref) == 0L) return()
        

    mapply(function(id, type) {
        marker1 <- markers[[id]]
        orient <- attr(marker1, "svg:marker-orient")
        refXY <- attr(marker1, "svg:marker-refXY")
        marker1 <- vgtransform(marker1, translate = -refXY)
        pos <- markerpos(path, type = type, orient = orient)
        if(is.null(pos)) return()
        apply(pos, 1L, function(x) {
            plot(vgtransform(marker1,
                translate =  x[1L:2L],
                rotate = x[3L], scale = scale),
                add = TRUE, ...)
        })
    }, id = mref, type = substr(names(mref), 8L, 12L))
    
   invisible()
}
