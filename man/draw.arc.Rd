\name{draw.arc}
\alias{draw.arc}
\title{Draw an elliptical arc}
\usage{
draw.arc(
  cx,
  cy,
  rx,
  ry = rx,
  rotation = 0,
  start = 0,
  end = 2 * pi,
  add = TRUE,
  n = 100L,
  plot. = TRUE,
  ...
)
}
\arguments{
\item{cx, cy}{x,y numeric, coordinates of the center of the ellipse}

\item{rx, ry}{the major and minor axes of the ellipse. If \code{ry} is missing
it is taken to be equal to \code{rx} and a circle is drawn.}

\item{rotation}{the rotation angle, in radians.}

\item{start, end}{start and end angle of the arc, in radians.}

\item{add}{logical; if \code{TRUE}, add to an already existing plot.
Has no effect if \code{plot.} is `FALSE.}

\item{n}{numeric, the number of straight lines that the arc is broken into.}

\item{plot.}{if \code{TRUE}, the result is plotted, otherwise only the calculated
cordinates are returned.}

\item{\dots}{additional arguments, passed to \code{plot} or \code{lines}.}
}
\value{
a matrix of x,y-coordinates of the resulting ellipse or elliptical arc.
}
\description{
Draws a circle, ellipse or an elliptical arc.
}
