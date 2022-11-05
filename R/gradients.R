getgradients <-
function(doc) {
    
    grad <- xml_find_all(doc, "//linearGradient | //radialGradient")
    #grad <- xml_find_all(doc, "//d1:linearGradient | //d1:radialGradient")
    n <- length(grad)
    if(n == 0L) return(NULL)
    gradstyle <- vector("list", n)
    
    for(i in seq_len(n))
        gradstyle[[i]] <- gradientstops(grad[[i]])
        
      
    names(gradstyle) <- xml_attr(grad, "id")
    gradstyle
}

#node <- grad[[i]]
#x <- xml_attr(stops, "stop-opacity")


gradient2col <-
function(x) {
    r <- vapply(x, function(z) rowMeans(col2rgb(z, alpha = TRUE)), numeric(4L))
    storage.mode(r) <- "integer"
    structure(rgb2col(r), names = names(x))
}

# TODO: include offsets?
gradientstops <-
function(node) {
    
    .getstops <- function(node) {
        stops <- xml_find_all(node, "./stop")
        if(length(stops) == 0L) return(NULL)
        opacity <- css2num(xml_attr(stops, "stop-opacity"))
        opacity[is.na(opacity)] <- 1 
        addopacity(css2col(xml_attr(stops, "stop-color")), opacity)
    }
    
    for(i in seq_len(64L)) { # prevent infinite loops, 64 should be enough (?)
        stops <- .getstops(node)
        if(is.null(stops) && xml_has_attr(node, "href")) {
            node <- getreferencenode(node, FALSE)
        } else return(stops)
    }  
}
