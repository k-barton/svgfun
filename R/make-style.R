
#.presentationAttributes <- c(.paintingProperties, .geometryProperties)

#attrs <- c(id = "id", x = "10%", fill = "red", junk1 = "...", stroke="green",
#    height = "100px", style="fill:blue;fill-opacity:.5;width:.666cm", width="100px",
#    href="#id", d = "M 10 666", junk = "...")
####

make.style <-
function(s) {
   stl <- unlist(strsplit(s, "\\s*;\\s*", perl = TRUE))
   stl <- strsplit(stl, "\\s*:\\s*", perl = TRUE)
   style <- vapply(stl, "[", "", 2L)
   names(style) <- vapply(stl, "[", "", 1L)
   #style <- style[names(style) %in% .presentationAttributes]
   style[!duplicated(names(style), fromLast = TRUE)]
}

# Note: style values have precedence over attributes
