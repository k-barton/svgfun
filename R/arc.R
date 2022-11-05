
# Implementation Notes:
# https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes

# Based on JS function `pointOnEllipticalArc` in SVG Curve Library v0.2.0:
# https://github.com/MadLittleMods/svg-curve-lib/blob/master/src/js/svg-curve-lib.js

arc <-
function(p0, p1, r, xAxisRotation, largeArcFlag, sweepFlag,  t) {

	phi <- toRadians(mod2(xAxisRotation, 360))
	# If the endpoints are identical, then this is equivalent to omitting the elliptical arc segment entirely.
	if(all(p0 == p1)) return(matrix(p0, nrow = 1L))

	r <- abs(r)
	
	# If rx <- 0 or ry <- 0 then this arc is treated as a straight line segment joining the endpoints.    
	#if(rx == 0 || ry == 0) return(p1)
	if(any(r == 0)) return(matrix(p1, nrow = 1L))

	# Following "Conversion from endpoint to center parameterization"
	# http:#www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
	
	p0 <- as.matrix(p0)
	p1 <- as.matrix(p1)
	
	cosPhi <- cos(phi) 
	sinPhi <- sin(phi) 
	
	# Step #1: Compute transformed point
	trPoint <- matrix(c(cosPhi, -sinPhi, sinPhi, cosPhi), 2, 2) %*% ((p0 - p1) / 2)
	
	# Ensure radii are large enough
	radiiCheck <- sum(trPoint^2 / r^2)
	if(radiiCheck > 1) 
		r <- sqrt(radiiCheck) * r

	# Step #2: Compute transformed center

    cSqDenom <- sum(r^2 * trPoint[2L:1L]^2)
	cSqNum <- prod(r^2) - cSqDenom
	# Make sure this never drops below zero because of precision
	cRadicand <- max(0, cSqNum / cSqDenom)
	cCoef1 <- (if(largeArcFlag != sweepFlag) 1 else -1) * sqrt(cRadicand)
	trCenter <- cCoef1 * r * c(1, -1) * (trPoint / r)[2L:1L, , drop = FALSE]
	
	# Step #3: Compute center
	center <- matrix(c(cosPhi, sinPhi, -sinPhi, cosPhi), 2L, 2L) %*%
		trCenter + (p0 + p1) / 2

	# Step #4: Compute start/sweep angles
	# Start angle of the elliptical arc prior to the stretch and rotate operations.
	# Difference between the start and end angles
	startVector <- (trPoint - trCenter) / r
    endVector <- (-trPoint - trCenter) / r
	startAngle <- angleBetween(c(1, 0), startVector)
	sweepAngle <- angleBetween(startVector, endVector)
	
	if(!sweepFlag && sweepAngle > 0) {
		sweepAngle <- sweepAngle - 2 * pi
	} else if(sweepFlag && sweepAngle < 0) {
		sweepAngle <- sweepAngle + 2 * pi
	}
	# We use % instead of `mod(..)` because we want it to be -360deg to 360deg
	# (but actually in radians)
	sweepAngle <- mod2(sweepAngle, 2 * pi)
	
	# From http:#www.w3.org/TR/SVG/implnote.html#ArcParameterizationAlternatives
	angle <- startAngle + (sweepAngle * t)
	ellipseComponentX <- r[1L] * cos(angle)
	ellipseComponentY <- r[2L] * sin(angle)
	
	xy <- cbind(
		x = cosPhi * ellipseComponentX - sinPhi * ellipseComponentY + center[1L],
		y = sinPhi * ellipseComponentX + cosPhi * ellipseComponentY + center[2L]
	)
	
	attr(xy, "end.angles") <- startAngle + c(0, sweepAngle)
	attr(xy, "angle") <- angle
	attr(xy, "center") <- center
	attr(xy, "rxy") <- r
	xy
}
