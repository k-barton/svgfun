print.vgshape <-
function (x, ...) {
    lim <- .xylimits(x)
    br <- sum(is.na(x[, 1L]))
    id <- attr(x, "svg:id")

    cat("path object of type", sQuote(attr(x,"svg:name")),
        "with", nrow(x) - br, "points and", br + 1L,
        ngettext(n = br + 1L, "subpath", "subpaths"), "\n",
        if(!is.null(id)) paste0("id: ", dQuote(id), "\n")
        )
    
    dimnames(lim) <- list(c("min", "max"), c("x", "y"))
    print(lim)
    invisible(x)
}

print.vgcontainer <-
function (x, ...) {

    nr <- rapply(x, classes = "vgshape", how = "unlist", nrow)
    id <- attr(x, "svg:id")
    if(length(nr) == 0L) {
        cat("An empty list of paths",
            if(!is.null(id)) c("with id", dQuote(id)), "\n")
    } else {
        is.tree <- any(vapply(x, inherits, logical(1L), "vgcontainer"))
        type <- rapply(x, classes = "vgshape", how = "unlist", function(x) attr(x, "svg:name"))
        ids <- rapply(x, classes = "vgshape", how = "unlist", function(x) attr(x, "svg:id"))
        nsp <- rapply(x, classes = "vgshape", how = "unlist", function(x) sum(is.na(x[, 1L]))) + 1L
        tmp <- cbind(id = ids, subpaths = nsp, points = nr)
        rownames(tmp) <- type
    
        if(is.tree) {
            fun <- function(x) if(r <- is.list(x)) r + sum(unlist(lapply(x, fun))) else 0L
            ngr <- fun(x)
        }
        
        cat("List of", length(nr), "paths",
            if(is.tree) c("and", ngr, "groups"),
            "with a total of", sum(nr), "points", "\n",
            if(!is.null(id)) paste0("id: ", dQuote(id), "\n"))
    
        cat("Objects:", "\n")
        print(tmp, quote = FALSE, right = TRUE)
    }
    invisible(x)
}

