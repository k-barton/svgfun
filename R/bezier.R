
#http://graphics.cs.ucdavis.edu/education/CAGDNotes/Cubic-Bezier-Curves/Cubic-Bezier-Curves.html
#http://graphics.cs.ucdavis.edu/education/CAGDNotes/Quadratic-Bezier-Curves/Quadratic-Bezier-Curves.html


bezier <-
function(t, p)  {
    t <- rep(t, each = 2L)
    omt <- 1 - t
    n <- nrow(p)
    if(n == 4L) {
        xyt <- omt^3 * p[1L, ] + 3 * t * omt^2 * p[2L, ] +
            3 * omt * t^2 * p[3L, ] + t^3 * p[4L, ]
    } else if(n == 3L) {
        xyt <- omt^2 * p[1L, ] + 2 * t * omt * p[2L, ] + t^2 * p[3L, ]
    } else stop("need 'p' with 3 or 4 rows")
    matrix(xyt, ncol = 2L, byrow = TRUE)
}