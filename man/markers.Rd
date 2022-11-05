\name{markers}
\alias{markers}
\alias{markers<-}
\alias{defaultMarkers}
\title{Add markers to path}
\usage{
markers(path, type = c("start", "mid", "end")) <- value

defaultMarkers(plot. = TRUE)
}
\arguments{
\item{path}{\code{"vgshape"} or \verb{"vgcontainer}" object to which the markers should be attached.}

\item{type}{character, one of "start", "mid" or "end",}

\item{value}{an id of an existing or default marker, or a \code{"vgshape"} or \verb{"vgcontainer}" object.}

\item{plot.}{logical, if \code{TRUE}, a plot is displayed, otherwise only marker names
are returned.}
}
\description{
Add start, end or mid- markers to the existing path. A marker is a graphic
used for drawing arrowheads or polymarkers on a path, line, polyline or
polygon elements.
}
\details{
Start and end markers are rendered at the first or last
vertex the shape. A mid-marker is drawn on every vertex except the first
and last vertices.
If \code{value} is a character string, marker objects with that name are first
searched for in the object's current markers, then in the default marker
library. Use \code{defaultMarkers()} to display available ones.
Assigned markers are stored in the environment pointed to by the
\code{"defs"} attribute, and is shared by all objects originating from the same
\acronym{SVG} file. Manually created objects have their own \code{"defs"}
environment.
}
\examples{

path <- vgpath("
  m 570,145
  c  40,25 5,60 -30,50
  s -55,-55 -40,-95 75,-65 125,-40
     70,90 45,145 -105,80 -165,45
    -75,-135 -50,-180 35,-45  65, -60
")

# SVG defaults to black fill and transparent stroke,
# so we set colours here:
plot(path, col = NA, border = 1)

# Add some markers at both ends and at the vertices:
markers(path, "start") <-  "Arrow1Lstart"
markers(path, "end") <-  "Arrow1Lend"
markers(path, "mid") <-  "Scissors"

plot(path, asp = 1, col = NA, border = 1, lwd = 1)
# Marker size and colour matches the line width and colour:
for(i in 1:5) {
    path <- vgtransform(path, translate = c(-30, 10),
        scale = .8, rotate = -25, origin = "center")
    plot(path, add = TRUE, lwd = (6 - i) / 2, col = NA, border = i + 1)
}

# Show all available markers:
# (For best effect, maximize the device before plotting.)
defaultMarkers()


}
