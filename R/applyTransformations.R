
applyTransformations <- 
function(tree) {
    .local <- function(tree) {
        parentT <- attr(tree, "svg:transform")
        parentStyle <- attr(tree, "svg:style")
        hasParentT <- is.matrix(parentT)
        defs <- attr(tree, "defs")
        
        rval <- lapply(tree, function(b) {
            elementT <- attr(b, "svg:transform")
            T2 <- if(!is.null(elementT)) {
                if(hasParentT) parentT %*% elementT else elementT
            } else parentT 
            
            
            # style(b, overwrite = NA) <- parentStyle
            elementStyle <- attr(b, "svg:style")
            style <- parentStyle
            elementStyle[elementStyle == "inherit"] <- NULL
            style[names(elementStyle)] <- elementStyle
            style -> attr(b, "svg:style")
            
            defs -> attr(b, "defs")

            
            b <- if(is.list(b)) { #if(inherits(b, "vgcontainer")) {
                T2 -> attr(b, "svg:transform")
                .local(b)
            } else {
                if(is.matrix(b) && length(b) != 0L) {
                    if(is.null(T2)) b else vgtransform(b, matrix. = T2)
                } else NULL
            }
            attr(b, "svg:transform") <- NULL
            b
        })
        attributes(rval) <- attributes(tree)
        attr(rval, "svg:transform") <- NULL
        rval
    }
    .local(tree)
}
