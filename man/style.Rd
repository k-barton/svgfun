\name{style}
\alias{style}
\alias{style,style.vgshape,style.vgcontainer}
\alias{style<-}
\title{Graphical object's style properties}
\usage{
style(x, ...)

style(x, overwrite = TRUE) <- value
}
\arguments{
\item{x}{a \code{"vgshape"} or \verb{"vgcontainer}" object.}

\item{\dots}{other arguments, currently not used.}

\item{overwrite}{logical, should existing properties be replaced?}

\item{value}{a named vector of new style properties. See \sQuote{Details}.}
}
\description{
Get or set style properties of a graphical object.
}
\details{
The following properties are allowed:
\itemize{
\item \code{opacity} numeric (0, 1), element's opacity;
\item \code{color} a \link[=css2col]{CSS color specification};
\item \code{fill}  a \link[=css2col]{CSS color specification} for the object's fill;
\item \code{fill-opacity} numeric (0, 1), the element's fill opacity;
\item \code{fill-rule} either \code{"nonzero"} (equivalent to \R's \code{"winding"}) or \code{"evenodd"};
See \code{\link{polypath}}'s argument \code{rule};
\item \code{stroke} character with a \link[=css2col]{CSS color specification} for the object's outline;
\item \code{stroke-width} width of the object's outline. A number or a string with
a number suffixed with a unit (e.g. \code{"10px"}, \code{"0.1in"}, \code{"3mm"});
Percentage form (e.g. \code{"2\%"}) is also allowed, but currently is interpreted simply as \code{1/100}.
\item \code{stroke-linecap} the line end style. One of \code{"round"}, \code{"butt"} or \code{"square"};
See \code{lend} in \link[=par]{graphical parameters};
\item \code{stroke-linejoin} the line join style. One of: \code{"round"}, \code{"miter"} or \code{"mitre"}, \code{"bevel"} or \code{"arcs"}
(mapped as \code{"mitre"}). See \code{ljoin} in \link[=par]{graphical parameters};
\item \code{stroke-dasharray} string with a list of numbers, defining the pattern of
dashes and gaps for the object's outline;
\item \code{stroke-opacity} numeric (0, 1), element's outline opacity;
\item \code{display} logical or character (\code{"none"} or empty string \code{""}).
All properties may be given as character vector or a \code{list}.
}
}
\examples{

rlogo <- read.svg(system.file(package = "svgfun", "svg", "rlogo.svg"))
plot(rlogo)

# For single objects, properties are overwritten by default:
style(rlogo[[2]]) <- c(stroke = "blue", `stroke-width` = "0.25cm",
                       `stroke-opacity` = 0.5, `stroke-dasharray` = "3 1 1 1")
plot(rlogo)

# For groups, properties are not overwritten:
style(rlogo) <- c(fill = "red")
plot(rlogo) # no effect

style(rlogo, overwrite = TRUE) <- c(fill = "red", stroke = "none")
plot(rlogo) 
}
