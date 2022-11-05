\encoding{utf-8}
\name{vgtransform}
\alias{vgtransform}
\alias{vgtransform.vgcontainer}
\alias{vgtransform.vgshape}
\title{Graphical path transformations}
\usage{
vgtransform(
  x,
  rotate = 0,
  translate = c(0, 0),
  scale = c(1, 1),
  skew = c(0, 0),
  origin = c(0, 0),
  matrix. = NULL
)
}
\arguments{
\item{x}{a \code{"vgshape"} object or a two-column, numeric matrix of \emph{x} and \emph{y} coordinates.}

\item{rotate}{numeric, the rotation angle, in degrees. Optionally, it can be a vector
of length 3, giving the rotation angle, and \emph{x} and \emph{y} coordinates of
the rotation center. It overrides the argument \code{origin}.}

\item{translate}{1 or 2-element numeric vector giving the horizontal and
vertical translation. If only one number is given, \emph{y}-translation is taken to
be zero.}

\item{scale}{1 or 2-element numeric vector for the scaling factor in the
horizontal and vertical dimension.}

\item{skew}{specifies a skew transformation along the \emph{x} and \emph{y} axes in
degrees. If only one value is given, it is taken as both \emph{x} and
\emph{y}-skew degree.}

\item{origin}{numeric of length 2, specifying the transformation origin.
The default origin is \emph{(0, 0)}.}

\item{matrix.}{numeric of length 9 or 6, defining a 3x3 transformation
matrix. For compatibility with the \acronym{SVG} attribute, only the
first two rows may be specified (i.e. 2x3 matrix) and
they will be completed to a square matrix.
See \sQuote{Details}.}
}
\description{
Applies transformations to the 2-dimensional coordinates: translation,
scaling, skew and rotation.
}
\details{
The \code{matrix.} argument specifies an affine transformation in the form of a
transformation matrix of six values. Setting \code{matrix.} argument to
\code{c(a, b, c, d, e,f)} is equivalent to applying the transformation matrix: \cr
\if{html}{\figure{vgtransform1.svg}{options: width=137 alt="TM"}}
\if{latex}{\figure{vgtransform1.pdf}{options: width=1.18in}} \cr
which maps coordinates from a previous coordinate system to a new user
coordinate system by the following matrix multipication: \cr
\if{html}{\figure{vgtransform2.svg}{options: width=209 alt="TM"}}
\if{latex}{\figure{vgtransform2.pdf}{options: width=1.8in}} \cr
Transformations are applied in the following order: rotation, scale, skew,
translation and user-defined matrix. Applying a skew along both axes
simultaneously produces a different result than the same skew applied
sequentially.

A \emph{xy}-coordinate matrix in which the original coordinates are
mapped into a new coordinate system. A new attribute \code{"transform.applied"} is added,
containing the transformation matrix. If this attribute already existed in \code{x},
the current matrix is post-multiplied by the existing one.
}
\examples{
shape <- vgrect(10, 10, width = 100, height = 50, rx = 5, ry = 5)

plot(shape, col = 2, lwd = 3, border = 1, ylim = c(200, 0))
abline(h = 0, v = 0, lty = 3)

# The default origin is at 0,0: 
polypath(vgtransform(shape, scale = .1),
     col = 3, lwd = 3, border = 1, asp = 1)
polypath(vgtransform(shape, scale = .5, origin = c(60, 35)),
     col = 3, lwd = 3, border = 1, asp = 1)

# Multiple transformations:
polypath(vgtransform(shape, translate = c(0, 100),
    rotate = 35, origin = c(60, 35), skew = c(-45, 5)),
     col = 3, lwd = 3, border = 1, asp = 1)
polypath(vgtransform(shape, translate = c(0, 100),
    origin = c(0, 35), skew = c(15, 0)),
     col = 4, lwd = 3, border = 1, asp = 1)


}
\keyword{graphics}
