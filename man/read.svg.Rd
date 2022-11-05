\encoding{utf-8}
\name{read.svg}
\alias{read.svg}
\title{Read SVG file}
\usage{
read.svg(
  file,
  n.curve = 15L,
  nodes = FALSE,
  flat = TRUE,
  encoding = "",
  verbose = FALSE,
  viewbox = c(0, 0, 672, 672),
  ...
)
}
\arguments{
\item{file}{path to the \acronym{SVG} file or an \acronym{URL} from which the paths are to be read from, or
\code{\link[=file]{"clipboard"}} to get the contents from clipboard, or a string with
the \acronym{SVG} code.}

\item{n.curve}{numeric, the number of straight lines B\enc{é}{e}zier curves and
elliptical arcs are broken into, plus 1 (i.e. number of points excluding the
endpoints). The larger the number the smoother will be rendering of the
curve. Passed to \code{\link{vgshape}} on reading path objects.}

\item{nodes}{if \code{TRUE}, information about path nodes and handles is included
in the result. Passed to \code{\link{vgshape}} on reading path objects.}

\item{flat}{should a "flat" list be returned, rather than a tree with
original object grouping structure? Defaults to \code{TRUE}, which simplifies
plotting.}

\item{encoding}{encoding for the \acronym{SVG} document. See \code{\link{read_xml}}.}

\item{verbose}{if \code{TRUE}, prints information about the processed objects.}

\item{viewbox}{default viewbox size. A vector of four elements \verb{(min.x, min.y, width, height)}, specifying a rectangle in user space. Viewbox is normqlly set
by the top \verb{<svg>} element, which overrides this value.}

\item{\dots}{other arguments, passed to \code{\link{read_xml}}.}
}
\value{
An object of class \code{"vgcontainer"}, being a list of \code{"vgshape"} objects
and other \code{"vgcontainer"} objects (if \code{"flat"} is \code{FALSE}). A \code{"vgshape"} is
a two-column numeric matrix of \emph{x} and \emph{y} coordinates. Each element
has an attribute \code{"svg:name"} specifying the object type, \code{"svg:style"}
holding the \acronym{SVG} element's \acronym{CSS} style entries,
including \acronym{SVG} presentation attributes, \code{"svg:attr"} for other
attributes, and optionally \code{"svg:transform"} with the transformation matrix.
\code{as.character} method produces \acronym{SVG} code.
}
\description{
Reads a file in \acronym{SVG} format and creates path coordinates for the
graphical objects defined in the file.
}
\details{
All \acronym{SVG} graphics elements types are recognised, except for
\code{<text>} which is silently ignored. Text objects must therefore be
converted to paths to be imported. The function tries to convert \acronym{SVG}
styling to the much simpler one available for \R graphics
devices. Only fill and border colours, line widths and types are used.
Gradient fills are rendered as an average colour of the gradient. Pattern
fills will be omitted (rendered as unfilled objects). Unlike in \acronym{SVG}, border widths are not
\link[=vgtransform]{transformed} with the object. Generally, object border styles are not
accurately reproduced and may need some adjustment,
either by setting \code{flwd} argument in \code{\link[=plot.vgcontainer]{plot}} or providing new values with
\code{lwd} argument for \code{\link[=plot.vgcontainer]{plot}}.
Clipping paths (as well as clipping within nested viewports), masks, markers (arrows etc.)
are omitted. Symbols currently cause an error.
Currently only presentation attributes (such as \code{fill="black"}) and inline
styles (e.g. \code{style="fill:black;"}) are applied. \acronym{CSS} style sheets are ignored.
}
\examples{

# Examples of all basic SVG shapes:
vg <- read.svg(system.file("svg", "basic.svg", package = "svgfun"))
plot(vg, asp = 1)

# Plot single object: 
plot(vg[[8]])

# As 'vgpath' is a matrix with 2 columns, we can also use 
# the default plot method:  
plot.default(vg[[7]], type = "l")


# Read SVG code from the string:
# (example from https://www.w3.org/TR/SVG/paths.html)
triangle <- 
read.svg('<svg xmlns="http://www.w3.org/2000/svg">
  <path d="M 100 100 L 300 100 L 200 300 z"
    fill="red" stroke="blue" stroke-width="3" />
</svg>', nodes = TRUE)

plot(triangle)
print(triangle)
# Generate SVG code:
cat(as.character(triangle), sep = "\n")

# Gradients are represented as an average colour of gradient stops:
rlogofile <- system.file(package = "svgfun", "svg", "rlogo.svg")
rlogo <- read.svg(rlogofile, nodes = TRUE)

op <- par(mfcol = c(2, 2), mar = c(3,3,3,3))
plot(rlogo, asp = 1)

# Show path vertices and control points:
plot(rlogo, col = gray(.5, alpha = .5), border = 1, nodes = TRUE, asp = 1)

# Replace colours:
plot(rlogo, border = 1, lwd = 1, col = c("blue", "red"))

# Apply multiple transformations:
rlogo.tr <- vgtransform(rlogo, rotate = 66, skew = c(0, 33),
    scale = c(1, .33), origin = "center")

plot(rlogo, asp = 1, , col = "#0000ff33")
plot(rlogo.tr, add = TRUE, border = 1, col = "#00ff0033")

par(op)
}
\references{
\href{https://developer.mozilla.org/docs/Web/SVG#documentation}{SVG Documentation at MDN}
}
\seealso{
\code{\link{vgshape}} for creating paths, \code{\link{shapes}} for creating other
basic graphical objects. \code{\link{vgtransform}} for geometric transformations.
}
\author{
Kamil Barto\enc{ń}{n}.
}
\keyword{graphics}
