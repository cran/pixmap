\name{addlogo-methods}
\docType{methods}
\title{Methods for Adding a Pixmap Logo to a Plot}
\alias{addlogo}
\alias{addlogo-methods}
\alias{addlogo,pixmap-method}
\description{
  This method allows the addition of a re-scaled pixmap to an existing
  plot, possibly as a logo, optionally preserving aspect.  It may be
  used interactively with \code{locator}, and is positioned in the
  coordinate system of the plot region.  Since the logo is displayed
  using \code{image}, it does not yet seem possible to use the function
  outside this region.
}
\usage{
  addlogo(x, \dots)
  \S4method{addlogo}{pixmap}(x, px, py, asp = NULL)
}
\arguments{
  \item{x}{an object of class \code{pixmap}}
  \item{px}{a vector of two x coordinates, or a list with two named
    elements x and y, such as that returned by \code{\link{locator}}.}
  \item{py}{if px is not a list, a vector of two y coordinates}
  \item{asp}{if omitted or NULL (default), output respects both y
    coordinates, if a number greater than zero, aspect is preserved in
    proportion to the difference between x coordinates, multiplied by asp,
    and only the first y coordinate is respected.}
  \item{\dots}{potentially further arguments passed to and from methods.}
}
\value{The same pixmap object with changed bounding box and cell resolution}
\author{Roger Bivand}
\examples{
x <- read.pnm(system.file("pictures/logo.ppm", package="pixmap")[1])
plot(x)
for (i in 1:7)
  addlogo(x, px=c(0, (101/77)*11), py=c((i-1)*11, i*11), asp=1)
}
\keyword{methods}

