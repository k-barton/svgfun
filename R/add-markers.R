
`markers<-` <-
function(path, type = c("start", "mid", "end"), value)
    set.markers(path, type, value)

set.markers <-
function(path, type = c("start", "mid", "end"), marker) {
    
    type <- match.arg(type)
    
    defscur <- attr(path, "defs")
    if(is.null(defscur)) {
        defscur <- new.env(parent = emptyenv())
        defscur -> attr(path, "defs")
    }
    if(!exists("markers", envir = defscur, inherits = FALSE, mode = "list")) {
        assign("markers", list(), envir = defscur, inherits = FALSE)
    }
    
    if(missing(marker)) {
        markerid <- NULL
    } else if(inherits(marker, c("vgshape", "vgcontainer"))) {
        marker <- list(marker)
        names(marker) <- markerid <- paste0("marker", sample.int(100000L, 1L))
        defscur$markers <- c(defscur$markers, marker)
    } else if(is.character(marker)) {
        markerid <- if(is.na(marker[1L]) || marker[1L] == "none") NULL else marker[1L]
        
        if(!is.null(markerid)) {
            existingMarkerIds <- names(defscur$markers)
            defaultmarkers <- .getdefaultmarkers()
            defaultMarkerIds <- names(defaultmarkers)
            
            #in.new <- markerid %in% newMarkerIds
            in.existing <- markerid %in% existingMarkerIds
            in.default <- !in.existing & markerid %in% defaultMarkerIds
            ok <- in.existing | in.default
            if(all(!ok))
                stop(gettextf("marker named \"%s\" not found.", markerid))
            if(!in.existing)
                defscur$markers <- c(defscur$markers, defaultmarkers[markerid])
        }

    } else NULL
    
    value <- attr(path, "svg:marker")
    if(is.null(value)) value <- character()
    if(is.null(markerid)) {
        value <- value[names(value) != paste0("marker-", type)]
    } else {
        value[paste0("marker-", type)] <- sprintf("url(\"#%s\")", markerid)
    }
    attr(path, "svg:marker") <- value
    path
}
