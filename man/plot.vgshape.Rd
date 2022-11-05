\encoding{utf-8}
\name{plot.vgshape}
\alias{plot.vgshape}
\alias{plot.vgcontainer}
\title{Plot vector graphics}
\usage{
\method{plot}{vgshape}(
  x,
  nodes = !is.null(attr(x, "nodes")),
  add = FALSE,
  axes = TRUE,
  col = NULL,
  border = NULL,
  asp = 1,
  xlim = NULL,
  ylim = NULL,
  rule = "evenodd",
  marker.scale = 0.62,
  ...
)

\method{plot}{vgcontainer}(
  x,
  nodes = FALSE,
  flwd = 1.7,
  marker.scale = 0.62,
  asp = 1,
  col = NULL,
  border = NULL,
  add = FALSE,
  axes = TRUE,
  ann = FALSE,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  ...
)
}
\arguments{
\item{x}{a \code{"vgshape"} or \verb{"vgcontainer}" object.}

\item{nodes}{logical, should path nodes and handles be displayed? Only effective
for \code{"vgshape"} objects of type \code{"path"}.}

\item{add}{logical; if \code{TRUE}, add to an already existing plot.}

\item{axes}{a logical value indicating whether both axes should be drawn on the plot.}

\item{col}{the colour[s] for filling the paths. If not \code{NULL} it overrides the
colours defined in the \acronym{SVG} document.}

\item{border}{the colour[s] to draw the borders. The default, \code{NULL}, means to use
the colour specified in the \acronym{SVG} document or if it is missing, \code{\link[=par]{par("fg")}}.}

\item{asp}{the \emph{y/x} aspect ratio, see \link{plot.window}.}

\item{xlim}{the \emph{x} limits \verb{(x1, x2)} of the plot.}

\item{ylim}{the \emph{y} limits of the plot. Note that unlike in \R plots, the
coordinate system origin of \acronym{SVG} is in the upper-left corner, which is
achieved \emph{via} "reversing" the \emph{y}-axis by setting \code{ylim} to \verb{(upper.y, lower.y)}.}

\item{rule}{character value specifying the path fill mode: either \code{"winding"} or
\code{"evenodd"}. See \code{\link{polypath}} for details.}

\item{marker.scale}{numeric, scaling factor for the path markers, if present.}

\item{\dots}{other \link[=par]{graphical parameters} The only useful are line type
parameters (\code{lwd}, \code{lty}, \code{lend}, \code{ljoin}). These override the styling
defined in the \acronym{SVG} document.}

\item{flwd}{line width scaling factor. As line widths in \R graphics are specific to a device,
this allows for adjusting them to desired widths.}

\item{ann, xlab, ylab}{axes annotation, see \link{plot.default}.}
}
\description{
Draw a vector graphic object created by \code{read.svg} or one of the shape
definition functions.
}
\details{
Draw a vector graphic object created by \code{read.svg} or one of the shape
definition functions.
}
\keyword{graphics}
