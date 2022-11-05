\encoding{utf-8}
\name{vgpath}
\alias{vgpath}
\title{Generate path coordinates from an SVG path definition}
\usage{
vgpath(d, origin = c(0, 0), n.curve = 25L, verbose = FALSE, nodes = FALSE)
}
\arguments{
\item{d}{character string (or a vector of such) containing \acronym{SVG} path commands.}

\item{origin}{initial position. It is irrelevant when \code{d} starts with an
absolute 'move' command (\code{"M"}).}

\item{n.curve}{numeric, the number points for B\enc{é}{e}zier curves and
elliptical arcs (excluding the endpoints). The larger the number the
smoother the curve.}

\item{verbose}{if \code{TRUE}, prints information about the processed commands.}

\item{nodes}{if \code{TRUE}, information about path nodes is included in the result.}
}
\value{
Returns an object of class \code{vgshape}, being a two-column matrix with
\emph{x} and \emph{y} coordinates. Subpaths are separated with a row of \code{NA}s,
making the matrix suitable for use as an argument to \code{\link{polypath}} and
\code{\link{lines}}. If argument \code{nodes} is \code{TRUE}, a \code{"nodes"} attribute is added,
with a list of parameters specific for each command. It can be
used for plotting path details (vertices, curve control points and arcs),
and generating \acronym{SVG} element code using \code{as.character}.
An attribute \code{"svg:name"} specifies the object type (\code{"path"}).
}
\description{
Calculates path point coordinates based on \acronym{SVG} path
definition.
}
\details{
Paths create complex shapes by combining multiple straight or curved lines.
Path definition is a sequence of commands where each command is composed of a
command letter and numbers that represent the command
parameters. \acronym{SVG} defines the following commands:\tabular{cll}{
   Command \tab Description \tab Parameters \cr
   \code{M} \tab \emph{moveTo} \tab \code{(x,y)+} \cr
   \code{L} \tab \emph{lineTo} \tab \code{(x,y)+} \cr
   \code{H} \tab horizontal \emph{lineTo} \tab \verb{(y)+} \cr
   \code{V} \tab vertical \emph{lineTo} \tab \verb{(x)+} \cr
   \code{C} \tab cubic B\enc{é}{e}zier curve \tab \code{(x1,y1 x2,y2 x,y)+} \cr
   \code{S} \tab smooth cubic B\enc{é}{e}zier curve \tab \code{(x2,y2 x,y)+} \cr
   \code{Q} \tab quadratic B\enc{é}{e}zier curve \tab \code{(x1,y1 x,y)+} \cr
   \code{T} \tab smooth quadratic B\enc{é}{e}zier curve \tab \code{(x,y)+} \cr
   \code{A} \tab elliptical arc \tab \code{(rx,ry angle largeArc sweep x,y)+} \cr
   \code{Z} \tab \emph{closePath} \tab  \cr
}


Commands are case-sensitive. An upper-case command specifies absolute
coordinates, while a lower-case command specifies coordinates relative to the
previous point (i.e the end point resulting from the previous command).
Hence, in the relative commands use \verb{(dx,dy)} instead of \verb{(x,y)}.
Coordinates are in the user coordinate system. The last set of coordinates
Command letters and parameters can be separated by white space or commas (here, for
readability, coordinate pairs are separated by comma, otherwise by space).
Each command letter can be followed by multiple sets of parameters (e.g. \code{"M 0,0 L 0,10 20,30"}
draws a line from (0,0) to (0,10) and then to (20,30).
\emph{moveTo} moves the pen to \code{(x,y)} without drawing anything. \emph{lineTo}
commands draw a straight line from the previous point to \code{(x,y)}.
Horizontal and vertical \emph{lineTo} obviously take only one coordinate, the
other one being 0.
Second and further parameter sets after \code{"M"} or \code{"m"} are assumed to be \emph{lineTo} command
(absolute or relative, respectively). Therefore, \code{"M 0,0 0,10 20,30"} is
equivalent to the previous example.
\subsection{Bézier Curves}{

A cubic Bézier curve \code{"C"} takes two control points and an end point. A
square curve \code{"Q"} requires one common control point for both the start and
end points. Several Bézier curves can be linked together to create smooth
shapes. The \code{"S"} and \code{"T"} commands for the cubic and quadratic curve,
respectively, create a curve in which the first control point is a reflection
of the last control point from the previous curve, thereby creating a
symmetric node. The control points essentially define the slope of the line
starting at each point.
\code{"S"} command should come after \code{"C"} or another \code{"S"} command. Similarly,
\code{"T"} should come after \code{"Q"} or \code{"T"}. Otherwise, the first control point is
assumed to be in the same place as the starting point. See \sQuote{Examples}.
}

\subsection{Elliptical arcs}{

Draws an elliptical arc from the current point to \code{x,y}. The ellipse has
radius \code{rx,ry} (major/minor axis) and is rotated by \code{angle} (in degrees,
clockwise). The \code{largeArc} and \code{sweep} are flags controling which part
of the ellipse is used to cut the arc (they should be either 1 or 0).
See \sQuote{Examples}.
}
}
\note{
\code{vgpath} accepts also other separators between the parameters (any sequence
that cannot be interpreted as a number or a command letter is assumed to be a
separator), but this is not a part of \acronym{SVG} standard and should
be avoided.
}
\examples{

# Draw a line using both absolute ("L") and relative ("l") line-to commands:
path <- vgpath("M 50,50 L 100,100 l 25,0")
plot(path, lwd = 3, border = 4, col = NA)

# Draw a cross using horizontal and vertical line commands "h" and "v"
# with relative coordinates:
path <- vgpath("M2 1 h1 v1 h1 v1 h-1 v1 h-1 v-1 h-1 v-1 h1 z")
plot(path, lwd = 3, col = 4)

# Bezier curves:

op <- par(mfrow = c(2,1), mar = c(1,1,3,1))

path <- vgpath("M100,200 C100,100 250,100 250,200 S400,300 400,200", nodes = TRUE)
style(path) <- c(fill="none", stroke="#888888", "stroke-width"="2")
plot(path, axes = FALSE)
title("Cubic Bezier curve")

path <- vgpath("M200,300 Q400,50 600,300 T1000,300", nodes = TRUE)
style(path) <- c(fill="none", stroke="#888888", "stroke-width"="2")
plot(path, axes = FALSE)
title("Quadratic Bezier curve")

par(op)



# Bezier curve followed by a smooth Bezier curve:
path <- vgpath("
    M 100,200
    c 0,-100 150,-100 150,0
    s 150,100 150,0
    ",
    nodes = TRUE)
# SVG default is black fill and transparent stroke, so we have to specify
# no-fill explicitly:
plot(path, col = NA, border = 1, lwd = 2, xlim = c(80, 420))

# If we shift second control point in "c", the first, implicit control point
# in "s" shifts symmetrically:
path2 <- vgpath("
    M 100,200
    c 0,-100 100,50 150,0
    s 150,100 150,0
    ", nodes = TRUE)
plot(path2, col = NA, border = 2, lwd = 2, add = TRUE)

arrow1 <- vgpath("M250,100 q -50,55 -50,145")
markers(arrow1, "end") <- "Arrow2Lend"
plot(arrow1, add = TRUE, col = NA, border = 2, lwd = 2, lty = 5)
text(c(100, 100, 250, 250), c(200, 100, 100, 200),
     expression(C[start], C[1], C[2], C[end]), pos = 2)
text(c(250, 400, 400), c(200, 300, 200),
     expression(S[start], S[1], S[end]), pos = 4)


# Elliptical arcs

# Demonstrate how largeArc and sweep flags work:
vg <- vgcontainer()
for(largeArc in c(0, 1))
    for(sweep in c(0, 1)) {
        d <- sprintf("M 125,75 a100,50 0 \%d,\%d 100,50", largeArc, sweep)
        vg[[paste0("largeArc=", largeArc, "; sweep=", sweep)]] <-
            vgtransform(vgpath(d, nodes = TRUE),
                translate = c(largeArc * 350, sweep * 200))
    }
    
plot(vg, col = NA, border = 1, lwd = 3, nodes = TRUE,
    xlim = c(0, 700), ylim = c(400, 20), axes = FALSE)
text(rep(c(170, 520), each = 2), c(190, 390),
    labels = names(vg), adj = c(.5, .5))

# "Picture of a pie chart with two pie wedges
# and a picture of a line with arc blips"
# Based on an example from:
# https://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands

vg <- vgcontainer(
    rect1 = vgrect(x=1, y=1, width=1198, height=398),
    arcs1 = vgpath("M 600,350 l 50,-25
                a 25,25 -30 0,1 50,-25 l 50,-25
                a 25,50 -30 0,1 50,-25 l 50,-25
                a 25,75 -30 0,1 50,-25 l 50,-25
                a 25,100 -30 0,1 50,-25 l 50,-25",
                nodes = TRUE),
    pie = vgcontainer(
        small.slice = vgpath("M300,200 h-150 a150,150 0 1,0 150,-150 z",
            nodes = TRUE),
        large.slice = vgpath("M275,175 v-150 a150,150 0 0,0 -150,150 z",
            nodes = TRUE)
    ))
    
style(vg)  <- c(fill = "none", stroke="blue", `stroke-width`="1")
style(vg$arcs1) <- c(fill = "none", stroke = "red", `stroke-width` = "2px")

style(vg$pie, overwrite = TRUE) <- c(fill = "red", stroke = "blue",
    `stroke-width` = "2px")
style(vg$pie$large.slice) <- c(fill = "yellow")
plot(vg, nodes = TRUE, asp = 1)

}
\references{
SVG Reference at \href{https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d#path_commands}{Mozilla Developer Network: Path commands} \cr
Chapter on Paths in \href{https://www.w3.org/TR/SVG/paths.html}{Scalable Vector Graphics (SVG) 2 W3C Candidate Recommendation}
}
\seealso{
\code{\link{markers}} for adding arrowheads, \code{\link{style}} for styling paths.
\code{\link{shapes}} for basic shapes.
}
\keyword{graphics}
