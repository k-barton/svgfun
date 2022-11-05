vgcontainer <- function(..., type = "g") {
    x <- list(...)
    # TODO: check for class
    x <- x[vapply(x, inherits, logical(1L), "vgelement")]
    class(x) <- c("vgcontainer", "vgelement", "list")
    attr(x, "svg:name") <- type
    x
}
