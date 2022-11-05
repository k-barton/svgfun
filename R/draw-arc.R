draw.arc <-
function(cx, cy, rx, ry = rx, rotation = 0, start = 0, end = 2 * pi,
    add = TRUE, n = 100L, plot. = TRUE, ...) {
    
	a <- seq(start, end, length.out = max(n, 3) + 1L)

	ellipseComponentX <- rx * cos(a)
	ellipseComponentY <- ry * sin(a)
	
    cosPhi <- cos(rotation)
    sinPhi <- sin(rotation)

	xy <- cbind(
		x = cosPhi * ellipseComponentX - sinPhi * ellipseComponentY + cx,
		y = sinPhi * ellipseComponentX + cosPhi * ellipseComponentY + cy
	)

    if(isTRUE(plot.)) {
        if(isTRUE(add))
            lines(xy, ...)
        else
            plot(xy, type = "l", ...)
        invisible(xy)
      } else xy
}
