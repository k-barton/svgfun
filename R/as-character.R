as.character.vgshape <-
function (x, ...) {
    xattr <- list()
    switch(xtype <- attr(x, "svg:name"),
        path = {
            nodes <- attr(x, "nodes")
            if(is.null(nodes)) stop("'x' must have a \"nodes\" atribute")
            nm <- toupper(names(nodes))
            nm[nm == "T"] <- "C"
            nm[nm == "S"] <- "Q"
            nm[nm %in% c("H", "V")] <- "L"
            if(length(nm > 1L)) {
                i <- c(1L, cumsum(rle(nm)$lengths) + 1L)
                nm[-i] <- ""
                nm[c(FALSE, nm[-1L] == "L" & nm[-length(nm)] == "M")] <- ""
            }
            #i <- nm != ""
            #nm[i] <- paste0(nm[i], " ")
            #nm <- paste0(nm, " ")
            nodes[nm == "Z"] <- list(numeric(0L))
            nodes <- lapply(nodes, signif, 10L)
            xattr$d <-
            paste0(nm, lapply(nodes, paste, collapse = ","), collapse = " ")
        }, ellipse =, circle =, rect =, line =, polyline =, polygon = {
            xattr <- as.list(attr(x, "svg:xml-attrs"))
            #sprintf("<%s%s />", , paste0(" ", names(xa), "=\"", xa, "\"", collapse = ""))
        }, stop("unknown SVG object type"))
            
    if(!is.null(Tmat <- attr(x, "transform.applied"))) {
        xattr$transform <- sprintf("matrix(%s)", paste(signif(Tmat[-3L, ], 9L), collapse = " "))
    }
    
    style <- attr(x,"svg:style")
    style <- c(style, attr(x,"svg:marker"))
    if(!is.null(style)) {
        xattr$style <- paste0(names(style), ":", style, collapse = ";")
    }
    
    paste0("<", xtype, paste0(" ", names(xattr), "=\"", xattr, "\"", collapse = ""), "/>")  
}

# TODO: defs
# add <marker style="overflow:visible"

as.character.vgcontainer <-
function (x, ...) {
    xtype <- attr(x, "svg:name")
    xattr <- attr(x, "svg:xml-attrs")
    tagopen <- paste0("<", xtype, paste0(" ", names(xattr), "=\"", xattr, "\"", collapse = ""), ">")
    tagclose <- paste0("</", xtype, ">")
    children <- unlist(lapply(x, as.character), use.names = FALSE)
    c(tagopen, children, tagclose)
}
