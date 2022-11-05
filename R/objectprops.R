objectprops <-
function(obj, attrs = xml_attrs(obj), box = NULL) {
    if(!inherits(obj, "xml_node"))
        stop("'obj' should be an \"xml_node\" object")
    names(attrs) <- tolower(names(attrs))
    props <- splitattrs(attrs)
    if(!is.null(props$style)) {
        style2 <- splitattrs(make.style(props$style))
        props$style <- NULL
        for(v in c("painting", "geometry", "marker"))
            props[[v]] <- combine2(props[[v]], style2[[v]])
    }
    if(!is.null(props$geometry)) {
        props$geometry <- css2num(props$geometry)
        if(!is.null(box))
            props$geometry <- perc2num(props$geometry, box = box)
    }
        
    if(!is.null(props$points))
        props$points <- listofnumbers(props$points)
        
    # Note: [1L] is unnecessary, as duplicate attributes are not allowed anyway.
    if(!is.null(props$transform))
        props$transform <- make.transmat(props$transform[1L])
    if(!is.null(props$viewBox)) {
        props$viewBox <- css2num(props$viewBox[1L])
        #names(props$viewBox) <- c("x", "y", "width", "height")
    }
    props$tagname <- xml_name(obj)
   
    props
}

# presentation attributes:
.properties <- list(
    painting = c(
"color", "fill", "stroke", "opacity",
"fill-opacity", "stroke-opacity",
"stroke-width" , "stroke-dasharray", "stroke-linejoin", "stroke-linecap",
"fill-rule", "display"),
    geometry = c("x", "y", "width", "height", "cx", "cy", "rx", "ry", "r",
                 "x1", "x2", "y1", "y2",
                 "refx", "refy", "markerwidth", "markerheight"
                 ),
    marker = c("marker-start", "marker-mid", "marker-end"),
    orient = "orient",
    markerUnits = "markerunits",
    style = "style",
    transform = "transform",
    href = "href",
    viewBox = "viewbox",
    preserveAspectRatio = "preserveaspectratio",
    id = "id",
    pathdef = "d",
    points = "points"
)
.proptypes <- factor(rep.int(names(.properties), sapply(.properties, length)),
    levels = names(.properties))
names(.proptypes) <- unlist(.properties)

