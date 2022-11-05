\name{css2col}
\alias{css2col}
\title{CSS colour specification to RGB hexadecimal conversion}
\usage{
css2col(x, ref = NULL)
}
\arguments{
\item{x}{a character vector with valid \acronym{CSS} colour values}

\item{ref}{a named character vector of valid \R colour values to substitute \code{url}
references (i.e. \code{"url(#elementId)"}). Names in \code{ref} should match the
referenced \code{id}-s. See \sQuote{Examples}.}
}
\description{
Converts colour values in formats accepted by \acronym{CSS} into hexadecimal
\acronym{RGB} notation, which can be used in \R.
}
\details{
Accepted formats of \code{x}:
\itemize{
\item \acronym{RGB} hexadecimal notation, including the shorthand 3-digit form
\code{"#rgb[a]"} which is expanded to \code{"#rrggbb[aa]"};
\item numerical color values: \code{"rgb[a](r, g, b[, alpha])"} and
\code{"hsl[a](h, s\%, l\%[, alpha])"}), and extended colour keywords.
The numbers ("arguments") can be separated by commas or spaces.
In \code{rgb()}, the integer value 255 corresponds to 100\%.
In \code{hsl()}, hue is represented as an angle of the color circle (0-360);
saturation and lightness as percentages. Opacity is always a decimal in
range 0-1.
\item keywords \code{"currentcolor"} and \code{"inherit"} are left unmodified.
\item the \code{"transparent"} keyword and all unrecognized values are converted to \code{NA}.
\item \code{"url(#id)"} values, which in \acronym{SVG} document are used
to point to a gradient or pattern fill, can be substituted with any
colour, \emph{via} the \code{ref} argument. The \code{"#id"} is case-sensitive and may be
single- or double quoted. See \sQuote{Examples}.
}
}
\examples{
# The many ways to specify red colour:
css2col(c("red", "#f00", "rgb(255,0,0)", "rgb(100\% 0\% 0\%)",
    "hsl(0, 100\%, 50\%)",  "hsl(360, 100\%, 50\%)"))
 
# Substitute `url` references:
css2col("url( '#gradient2' )", ref = c(gradient1 = "blue", gradient2 = "green"))
}
\references{
\href{https://www.w3.org/TR/css-color-3}{CSS Color Module Level 3}
}
