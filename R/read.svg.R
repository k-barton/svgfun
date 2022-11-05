read.svg <-
function(file, n.curve = 15L, nodes = FALSE,
        flat = TRUE, encoding = "", verbose = FALSE,
        viewbox = c(0, 0, 672, 672), # css2num("7in") default size of R's svg device
        ...) {

    if(!is.numeric(viewbox) || length(viewbox) != 4L)
        stop("'viewbox' must be numeric vector of length 4.")
    if(any(viewbox[3L:4L] <= 0))
        stop("\"width\" and \"height\" in 'viewbox' must be positive")
    
    .msg <- .messageCtor(verbose)

    if(identical(file, "clipboard")) {
        file <- tempfile()
        conClosed <- -1L
        con <- base::file(file, "w", encoding = "UTF-8")
        writeLines(readLines("clipboard", warn = FALSE), con = con)
        conClosed <- close(con)
        on.exit({
            if(conClosed != 0L) close(con)
            file.remove(file)
        })
    }
    doc <- read_xml(file, encoding = encoding)
    on.exit(xml_remove(doc, free = TRUE), add = TRUE, after = FALSE)
    rootattrs <- xml_attrs(doc)
    
    xml_ns_strip(doc)
    
    tags <- c("svg", "g", "path", "ellipse", "circle", "rect", "line",
        "polyline", "polygon", "use")
    xpath <- paste0("./*[", paste(gsub("(?<=\\b)([a-z])", "self::\\1", tags,
        perl = TRUE), collapse = " or "), "]")

    defsenv <- new.env(parent = emptyenv())

    tree <- .object(doc, xpath, viewbox, .msg,
        n.curve = n.curve, nodes = nodes, verbose = verbose)
    attr(tree, "defs") <- defsenv

    tree <- applyTransformations(tree)
    if(flat) tree <- flatten.list(tree)
       
    gcol <- getgradients(doc)
    if(length(gcol) != 0L)
        assign("colorref", gradient2col(gcol), envir = defsenv)

    markerNodes <- xml_find_all(doc, "//marker")
    if(length(markerNodes) != 0L) {
        markers <- lapply(markerNodes, .object, xpath = xpath, viewbox = NULL,
            .msg = .msg, verbose = verbose)
        attr(markers, "defs") <- defsenv
        markers <- applyTransformations(markers)
        markerids <- xml_attr(markerNodes, "id")
        names(markers) <- markerids
        markers <- markers[!is.na(markerids)] # remove useless markers w/o id attribute.
        assign("markers", markers, envir = defsenv)
    }

    attr(tree, "svg:xml-attrs") <- rootattrs
    class(tree) <-  c("vgcontainer", "vgelement", "list")
    invisible(tree)
}

.matchNames <- function(a, x) match(a, names(x), nomatch = 0L)

# DONE:
# circle r - refer to the normalized diagonal of the current SVG viewport
# x,cx,ry refer to the width of the current SVG viewport
# y,cy,ry refer to the height of the current SVG viewport
# TODO
# The value "auto" for width and height on the ‘svg’ element is treated as 100%.
# The used value of width may be constrained by the value of the max-width
# and min-width properties. The used value of height may be constrained by
# the value of the max-height and min-height properties.
   
.shape <-
function(type, props, ...) {
    
    attrNum <- switch(type,
        ellipse = c("cx", "cy", "rx", "ry"),
        circle = c("cx", "cy", "r"),
        rect = c("x", "y", "width", "height", "rx", "ry"),
        line = c("x1", "x2", "y1", "y2"), NULL)
    
    #props$geometry <- perc2num(props$geometry, box = viewbox[3L:4L])
   
    if(!is.null(attrNum))
        v <- xmatch(props$geometry, attrNum, 0)
  
    rval <- switch(type,
        path = vgpath(props$pathdef, ...),
        ellipse = vgellipse(v[1L], v[2L], v[3L], v[4L], ...),
        circle = vgellipse(v[1L], v[2L], v[3L], v[3L], ...),
        rect = vgrect(v[1L], v[2L], v[3L], v[4L], v[5L], v[6L], ...),
        line = vgline(v),
        polyline = vgpolyline(props$points),
        polygon = vgpolyline(props$points, closed = TRUE),
        stop(dQuote(type), "is not a recognized shape"))

    .setattr(rval, props)
} # .shape


.setattr <-
function(x, props) {
    if(is.null(x)) x <- list()
        
    if(has(props, "marker"))
        attr(x, "svg:marker") <- props$marker
    attr(x, "svg:transform") <- props$transform
    attr(x, "svg:style") <- props$painting
    attr(x, "svg:name") <- props$tagname
    attr(x, "svg:id") <- props[["id"]]
    x
}


.container <-
function(props, node, viewbox,  xpath, .msg,
         sizevars = c("width", "height"),
         posvars = c("x", "y"), ...) {
    
    hasviewbox <- has(props, "viewBox")

    if(props$tagname == "svg" || has(props, "geometry"))
        posz <- xmatch(props$geometry,
            c(posvars, sizevars),
            c(0, 0, viewbox[3L:4L]), pos = TRUE)
        
    if(has(props, c("geometry", "viewBox"))) {
        if(hasviewbox) {
            viewbox <- props$viewBox
            if(has(props, "geometry")) {
                vpTrans <- viewporttransform(props$viewBox,
                    posz[posvars], posz[sizevars],
                    props$preserveAspectRatio)
                if(!is.null(vpTrans)) {
                    props$transform <-
                        if(has(props, "transform"))
                            props$transform %*% vpTrans else vpTrans
                }
            }
        } else if(has(props, "geometry")) {
            # if width & height & !viewbox -> viewbox = c(x, y, width, height)
            viewbox <- posz
        }
    } # has viewBox or geometry
    
    nodelist <- xml_find_all(node, xpath = xpath)
    n <- length(nodelist)
    rval <- vector("list", n)
    if(n != 0L) {
        ids <- xml_attr(nodelist, "id")
        noid <- is.na(ids)
        ids[noid] <- which(noid)
        names(rval) <- paste0(xml_name(nodelist), ".", ids)
        for(i in seq_len(n))
            rval[[i]] <- .object(nodelist[[i]], xpath, viewbox, .msg, ...)
    }
    rval <- .setattr(rval, props)
    class(rval) <- c("vgcontainer", "vgelement", class(rval))
    
    if(hasviewbox) attr(rval, "svg:viewbox") <- props$viewBox
    if(props$tagname == "marker") {
        attr(rval, "svg:marker-refXY") <- xmatch(props$geometry, c("refx", "refy"), 0)
        orient <- xmatch(props$orient, "orient", "auto")[[1L]]
        if(! orient %in% c("auto", "auto-start-reverse"))
            orient <- css2num(orient)
        attr(rval, "svg:marker-orient") <- orient
        attr(rval, "svg:marker-units") <- xmatch(props$markerUnits, "markerunits", "auto")[[1L]]
    }
    rval
 
}

.object <-
function(node, xpath, viewbox, .msg, ...) {
    
    if(!inherits(node, "xml_node"))
        stop("'node' must be an \"xml_node\"")
    
    props <- objectprops(node, box = viewbox[3L:4L])

    .msg("object: %s", props$tagname)
    
    if(props$tagname == "use") {
        target <- use.element(node, props)
        if(is.null(target)) return(NULL)
        node <- target$node
        props <- target$props
        .msg("target: %s", props$tagname)
    }
    rval <- list()

    rval <- switch(props$tagname,
        marker = 
            .container(props, node, viewbox, xpath, .msg,
                sizevars = c("markerWidth", "markerHeight"),
                ...),
        symbol =, defs =, svg =, g =
             .container(props, node, viewbox, xpath, .msg, ...), {
            rval <- .shape(props$tagname, props, ...)
        })
            
    #cat(props$tagname, if(has(props, "id")) paste("#", props$id), "\n")
 

    rval
} # .object
