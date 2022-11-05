css2par <-
function(css, ref = NULL) {
    
    # make.style takes care there are no duplicates
    # css <- css[!duplicated(names(css), fromLast = TRUE)]
    
    n <- length(css) 
    style <- vector("list", n)
    nm <- names(style) <- names(css)
    
    for(i in seq_len(n)) {
        rule <- nm[i]
        value <- css[[i]]
        
        style[[i]] <- 
            switch(rule,
            "stroke-dasharray" =  {
                # XXX: percentages in _stroke-dasharray_ property will not be
                # properly interpreted
                if(value == "none") 1 else {
                    v <- css2num(value)
                    v <- as.integer(v / min(v))
                    if(length(v) %% 2L == 1L) v <- rep.int(v, 2L)
                    paste0(sprintf("%x", pmin(v, 15L)), collapse = "")
                }
            }, "stroke-linejoin"=,
               "stroke-linecap" =,
               "fill-rule" =
                   .styleArgs[[rule]][value],
                "fill" =, "stroke" =, "color" = {
                   css2col(value, ref)
            }, "opacity" =, "fill-opacity" =, "stroke-opacity" =
                    pmin(1, pmax(0, as.numeric(value))),
                "stroke-width" =
                    pmax(0, css2num(value)), # XXX css2num?
                "display" = value != "none",
                value #default
              )
    }
    
    style2 <- .defaultStyle
    style2[names(style)] <- style
    
    colorattr <- c("fill", "stroke")
    colors <- unlist(style2[colorattr])
    colors[colors == "currentcolor"] <- style2[["color"]]
    opacity <- paste0(colorattr, "-opacity")
    colors <- addopacity(colors, as.numeric(style2[opacity]) * style2[["opacity"]])
    style2[colorattr] <- colors
       
    style2 <- style2[c("fill", "stroke", "stroke-width", "stroke-dasharray",
        "stroke-linejoin", "stroke-linecap",  "fill-rule", "display")]
    names(style2) <- c("col", "border", "lwd", "lty", "ljoin", "lend", "rule", "show")
    style2
}

.defaultStyle <-
list(opacity = 1,
     color = "#000000", fill = "currentcolor", `fill-opacity` = 1,
    `fill-rule` = "evenodd", 
     stroke = NA, `stroke-width` = 1,
    `stroke-linecap` = 1, 
    `stroke-linejoin` = 1,
    `stroke-dasharray` = 1, 
    `stroke-opacity` = 1,
    display = TRUE
    )

.styleColors <- c("color", "fill", "stroke")

.styleArgs <- list(
    "stroke-linejoin" = c("round" = 0, "miter" = 1, "mitre" = 1,
        "bevel" = 2, "arcs" = 1),
    "stroke-linecap" = c("round" = 0, "butt" = 1, "square" = 2),
    "fill-rule" = c("nonzero" = "winding", "evenodd" = "evenodd")
    )
