\name{subplot}
\alias{subplot}
\title{Add subplot to the existing plot}
\usage{
subplot(expr, fig = 0.33, pos = "topleft", inset = c(0, 0))
}
\arguments{
\item{expr}{an \R expression to produce the subplot contents.}

\item{fig}{numeric of length 1 or 2. The proportion of the plotting region to
be taken as the subplot plotting region (horizontal, vertical).}

\item{pos}{character, subplot position as a keyword, including \code{"top"}, \code{"bottom"}, \code{"left"},
\code{"right"} (e.g. \code{"topleft"} or \code{"bottom-right"}). Empty string produces a centred plot.}

\item{inset}{numeric of length 1 or 2, giving inset distance(s) from the
margins, in inches.}
}
\description{
Create a subplot within the current plotting region
}
\note{
\code{subplot} works by setting new margin sizes, so the code in \code{expr} should
not change graphical parameters related to plot geometry (such as \code{mar} or
\code{oma}).
}
\examples{
x <- rnorm(100)
y <- rnorm(100, sd = 2)
plot(x, y, axes = FALSE, pch = 20)
# Add a histogram of x in the upper right corner:
subplot(fig = .4, inset = c(.05, .05), pos = "topright", {
    hist(x, col = grey.colors(5, alpha = .5), main = NA, axes = FALSE, ann = FALSE)
    axis(3)
    axis(4)
    box()
})
box("plot", lwd = 3)
axis(1)
axis(2)
p1 <- par(no.readonly = TRUE)
# Add thin density plot of y along y-axis:
usr <- par("usr")
subplot(fig = c(.5, 1), pos = "left", {
    plot.new()
    plot.window(0:1, usr[3:4], yaxs = "i", xaxs = "i")
    d <- density(y)
    polygon(c(0, d$y, 0), c(usr[1], d$x, usr[2]), col = "black")
})
box()

# Add a small R logo in the lower-right corner:
subplot(fig = .15, inset = c(.05, .05), pos = "bottom-right", {
    rlogo <- read.svg(system.file(package = "svgfun", "svg", "rlogo.svg"))
    plot(rlogo, axes = FALSE)
    mtext(side = 3, "Created with", cex = .7)
})
}
