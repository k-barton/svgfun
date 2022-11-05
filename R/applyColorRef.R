
applyColorRef <-
function(x) {
    colorref <-  attr(x, "svg:colorref")
    if(is.null(colorref)) return(x)
    x <- treeapply(x, function(x) {
           style <- attr(x, "svg:style")
           if(!is.null(style)) {
            i <- names(style) %in% .styleColors
            style[i] <- css2col(style[i], colorref)
            style -> attr(x, "svg:style")
           }
           x
    })
}
