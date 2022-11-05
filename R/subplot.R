subplot <-
function(expr, fig = .33, pos = "topleft", inset = c(0, 0)) {
    fig <- rep(fig, length.out = 2L)
    xl <- diff(grconvertX(c(0, 1), from = "npc", to = "lines"))
    yl <- diff(grconvertY(c(0, 1), from = "npc", to = "lines"))
    if(!missing(inset)) {
        inset <- rep(inset, length.out = 2L)
        xi <- grconvertX(inset[1L], from = "inches", to = "lines")
        yi <- grconvertY(inset[2L], from = "inches", to = "lines")
    } else 
        xi <- yi <- 0
    mar <- par("mar")
    pos <- tolower(pos)
    .haspos <- function(a, p) any(grepl(a, p, fixed = TRUE))
    mar[c(2L, 4L)] <- mar[c(2L, 4L)] + xi
    mar[c(1L, 3L)] <- mar[c(1L, 3L)] + yi
    sx <- xl  * (1 - fig[1L])
    sy <- yl  * (1 - fig[2L])
  
    if(.haspos("left", pos)) {
        mar[4L] <- mar[4L] + sx
    } else if(.haspos("right", pos)) {
        mar[2L] <- mar[2L] + sx
    } else 
        mar[c(2L, 4L)] <- mar[c(2L, 4L)] + (sx / 2)
        
    if(.haspos("bottom", pos)) {
        mar[3L] <- mar[3L] + sy
    } else if(.haspos("top", pos)) {
        mar[1L] <- mar[1L] + sy
    } else 
        mar[c(1L, 3L)] <- mar[c(1L, 3L)] + (sy / 2)
    
    mf <- min(fig)
    op <- par(no.readonly = TRUE)
    par(mar = mar, cex = if(mf < .5) .66 else if(mf < 1) .88 else 1,
        new = TRUE)
    on.exit(par(op))
    if(!missing(expr)) eval.parent(substitute(expr))
    invisible()
}
