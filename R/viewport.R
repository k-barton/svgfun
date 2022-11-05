
##This process converts the min-x, min-y, width and height values of a viewBox
#attribute, the position and size of the element on which the viewBox attribute
#is defined, and the value of the preserveAspectRatio attribute on that element
#into a translation and a scale that is applied to content contained by the
#element.

#contained(
#        viewBox = c(0, 0, 10, 25),
#        x = 200,
#        y = 0,
#        width = 100,
#        height = 1 * 100,
#        preserveAspectRatio = "xMidYMin meet"
#)
#
#x <- 
#contained("0 0 10 25", x=200, y=15, width=100, height=100,
#        preserveAspectRatio="xMidYMid meet")
#cat(paste0(names(x), "(", sapply(x, paste, collapse = ","), ")", collapse = " "))


# Reference:
# https://svgwg.org/svg2-draft/coords.html#ComputingAViewportsTransform
#contained <-
#function(viewBox, x, y, width, height, preserveAspectRatio = NULL) {
#    
#    if(is.character(viewBox))  viewBox <- listofnumbers(viewBox)
#        
#    #print(c(viewBox = viewBox,x = x, y = y, width = width, height= height))
#    
#    vbX <- viewBox[1L]
#    vbY <- viewBox[2L]
#    vbWidth <- viewBox[3L]
#    vbHeight <- viewBox[4L]
#        
#    eX <- x
#    eY <- y
#    eWidth <- width
#    eHeight <- height
#    
#    if(is.null(preserveAspectRatio)) {
#        align <- "xMidYMid"
#        meetOrSlice <- "meet"
#    } else {
#        preserveAspectRatio <-
#        regmatches(preserveAspectRatio,
#            regexec(" *(xM[axidn]{2}YM[axidn]{2}\\b)?(?:(?(1) +)(meet|slice)?)?",
#            preserveAspectRatio, perl = TRUE))[[1L]][-1L]
#        
#        dflt <- preserveAspectRatio == ""
#        preserveAspectRatio[dflt] <- c("xMidYMid", "meet")[dflt]
#        #align <- preserveAspectRatio[1L]
#        align <- substring(preserveAspectRatio[1L], c(1L, 5L), c(4L, 8L))
#        meetOrSlice <- preserveAspectRatio[2L]
#    }
#    
#    scaleX <- eWidth / vbWidth
#    scaleY <- eHeight / vbHeight
#    
#    if(align[1L] != "none") {
#        scaleX <- scaleY <- switch(meetOrSlice, meet = min, slice = max)(scaleX, scaleY)
#        
#        if(meetOrSlice == "meet") {
#            scaleX <- scaleY <- min(scaleX, scaleY)
#        } else if(meetOrSlice == "slice") {
#            scaleX <- scaleY <- max(scaleX, scaleY)
#        }
#    
#    translateX <- eX - (vbX * scaleX)
#    translateY <- eY - (vbY * scaleY)
#    
#    
#    
#    translateX <- translateX + switch(substr(align, 1L, 4L),
#        xMid = (eWidth - vbWidth * scaleX) / 2,
#        xMax = (eWidth - vbWidth * scaleX),
#        0)
#    
#    translateY <- translateY + switch(substr(align, 5L, 8L),
#        YMid = (eHeight - vbHeight * scaleY) / 2,
#        YMax = (eHeight - vbHeight * scaleY),
#        0)
#    
#    list(
#        translate = c(translateX, translateY),
#        scale = c(scaleX, scaleY)
#    )
#}




##This process converts the min-x, min-y, width and height values of a viewBox
#attribute, the position and size of the element on which the viewBox attribute
#is defined, and the value of the preserveAspectRatio attribute on that element
#into a translation and a scale that is applied to content contained by the
#element.

#contained(
#        viewBox = c(0, 0, 10, 25),
#        x = 200,
#        y = 0,
#        width = 100,
#        height = 1 * 100,
#        preserveAspectRatio = "xMidYMin meet"
#)
#
#x <- 
#contained("0 0 10 25", x=200, y=15, width=100, height=100,
#        preserveAspectRatio="xMidYMid meet")
#cat(paste0(names(x), "(", sapply(x, paste, collapse = ","), ")", collapse = " "))


# Reference:
# https://svgwg.org/svg2-draft/coords.html#ComputingAViewportsTransform
viewporttransform <-
function(viewBox, pos, size, preserveAspectRatio = NULL) {
    
    if(is.character(viewBox))  viewBox <- listofnumbers(viewBox)
        
    if(is.null(preserveAspectRatio) || preserveAspectRatio == "") {
        align <- c("xMid", "YMid")
        meetOrSlice <- "meet"
    } else {
        preserveAspectRatio <-
        regmatches(preserveAspectRatio,
            regexec(" *(xM[axidn]{2}YM[axidn]{2}\\b)?(?:(?(1) +)(meet|slice)?)?",
            preserveAspectRatio, perl = TRUE))[[1L]][-1L]
        dflt <- preserveAspectRatio == ""
        preserveAspectRatio[dflt] <- c("xMidYMid", "meet")[dflt]
        align <- substring(preserveAspectRatio[1L], c(1L, 5L), c(4L, 8L))
        meetOrSlice <- preserveAspectRatio[2L]
    }

    scaleXY <- size / viewBox[3L:4L]
    
    if(align[1L] != "none")
        scaleXY[] <- switch(meetOrSlice, slice = max, meet = min, )(scaleXY)

    translateXY <- pos - (viewBox[1L:2L] * scaleXY)
    
    z <- size - viewBox[3L:4L] * scaleXY 
    a <- c(switch(align[1L], xMid = .5, xMax = 1, 0),
           switch(align[2L], YMid = .5, YMax = 1, 0))
    
    translateXY <- translateXY + z * a
    
    if(all(translateXY == 0) && all(scaleXY == 1)) NULL else
        transmat(translate = translateXY, scale = scaleXY)
}
