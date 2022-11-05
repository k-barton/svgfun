\encoding{utf-8}
\name{shapes}
\alias{shapes}
\alias{vgline}
\alias{vgellipse}
\alias{vgrect}
\alias{vgpolyline}
\title{Create basic shapes}
\usage{
vgline(x)

vgellipse(cx, cy, rx, ry = rx, n = 64L, ...)

vgrect(x, y, width, height = width, rx = 0, ry = 0, f = 0.554, ...)

vgpolyline(points, closed = FALSE)
}
\arguments{
\item{x}{for \code{vgline} a numeric vector of length 4,
specifying the x and y coordinates of the line start and end positions
(i.e. \code{c(x1, x2, y1, y2)}). For \code{vgrect} an x coordinate of the
upper-left corner of the rectangle.}

\item{cx, cy}{x and y coordinates or the ellipse center.}

\item{rx, ry}{x and y radii of the ellipse or of the horizontal and vertical
corners of the rectangle. If only one is given, the other one defaults to
the one specified.}

\item{n}{number of points on the ellipse to calculate. Larger numbers produce
smoother representation.}

\item{\dots}{other arguments, in \code{vgrect} passed to \code{\link{vgshape}},
otherwise not used.}

\item{y}{an y coordinate of the upper-left corner of the rectangle.}

\item{width, height}{the width and height of the rect. If height is not given,
it is taken to be equal to \code{"width"}.}

\item{f}{the length of a B\enc{é}{e}zier handle as a fraction of a corner radius.
The default produces visually best result and is compatible with \acronym{SVG}'s \verb{<rect>}.}

\item{points}{a numeric vector of x and y coordinate pairs, or a character
string with such pairs separated by spaces or commas.}

\item{closed}{if \code{TRUE} produces a polygon, otherwise an open path (unless
the first and last points are identical).}
}
\value{
A matrix with two columns for x and y coordinates.
}
\description{
\code{vgellipse} creates ellipse based on a center coordinate, and both their
\emph{x} and \emph{y} radius.
\code{vgrect} creates rectangle, defined by their position, width, and
height. The rectangle may have their corners rounded.
\code{vgpolyline} creates a shape consisting of a set of connected
straight line segments. The shape can be closed, then the last point is
connected to the first point.
}
\examples{
# Set up a plot with 'reversed' y-axis:
plot.new()
plot.window(c(0, 100), c(100, 0), asp = 1)

# Line, poly-line and filled polygon:
lines(vgline(c(80,60,10,60) - c(30, 30, 0,0)), lwd = 5)
lines(vgpolyline("80,10 60,40 75,40 60,60"), col = 1, lwd = 5)
polypath(vgpolyline(c(80,10, 60,40, 75,40, 60,60) + c(30, 0), closed = TRUE),
    col = 1, border = NA)

# Rectangle and rounded rectangle:
polygon(vgrect(0, 10, 20, 50, 0), col = 4, border = 1, lwd = 3)
polygon(vgrect(5, 15, 10, 40, 5), col = 2, border = 1, lwd = 3)

# Circle and ellipses:
polygon(vgellipse(10, 85, 15), col = 3, border = 1, lwd = 3)
polygon(vgellipse(50, 85, 5, 15), col = 4, border = 1, lwd = 3)
polygon(vgellipse(50, 85, 15, 5), col = 5, border = 1, lwd = 3)

}
\keyword{graphics}
