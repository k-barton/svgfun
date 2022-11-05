#cat(sprintf('#\' * `%s` ', names(.styletype)), sep ="\n")
style <-
function(x, ...) UseMethod("style")

`style<-` <-
function(x, overwrite = TRUE, value) UseMethod("style<-")

style.vgshape <-
style.vgcontainer <-
function(x, ...) {
    attr(x, "svg:style")
}

`style<-.vgshape` <-
function(x, overwrite = TRUE, value) {
 
    colorref <- getDef(x, "colorref")
      
    st <- attr(x, "svg:style")
    if(!isTRUE(overwrite))
       value <-  value[! names(value) %in% names(st)]
       
       # XXX: overwrite = NA --> merge
       #value$opacity  * current$opacity
       
    if(length(value) == 0L) {
        if(is.null(value)) attr(x, "svg:style") <- NULL
        return(x)
    }
    
    k <- match(names(value), names(.styletype), nomatch = 0L)
    value <- value[k != 0L]
    k <- k[k != 0]
    for(i in seq_along(value))
       value[i] <- eval(.styletype[[k[i]]], list(x = value[i]))
      
     if (any(i <- is.na(value)))
           value[i] <- "none"

    if (is.null(st)) {
        attr(x, "svg:style") <- value
    } else {
        st[names(value)] <- value
        if (any(i <- !is.na(st) & st == ""))
            st <- st[!i]
        attr(x, "svg:style") <- st
    }
    x
}

`style<-.vgcontainer` <-
function(x, overwrite = FALSE, value) {
    for(i in seq_along(x))
        style(x[[i]], overwrite = overwrite) <- value
    x
}

as.num <- function(x, xmin = -Inf, xmax = Inf) pmax(pmin(css2num(x), xmax), xmin)
anyof <- function(x, choices) choices[pmatch(x, choices, nomatch = 0L)]
as.listofnumbers <- function(x) paste(as.numeric(x), collapse = " ")

.styletype <- alist(
    opacity = as.num(x, 0, 1), 
    color = css2col(x, colorref),
    fill = css2col(x, colorref),
    `fill-opacity` = as.num(x, 0, 1),
    `fill-rule` = anyof(x, names(.styleArgs[["fill-rule"]])), 
     stroke = css2col(x, colorref),
     `stroke-width` = as.num(x, 0),
    `stroke-linecap` = anyof(x, names(.styleArgs[["stroke-linecap"]])), 
    `stroke-linejoin` = anyof(x, names(.styleArgs[["stroke-linejoin"]])),
    `stroke-dasharray` = x, 
    `stroke-opacity` = as.num(x, 0, 1),
    display = if(is.character(x)) x[x %in% c("", "none")] else if(isTRUE(x)) "" else "none"
    )

