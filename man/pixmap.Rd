\name{pixmap}
\title{Pixmap Images}
\alias{pixmap}
\alias{pixmapRGB}
\alias{pixmapGrey}
\alias{pixmapIndexed}
\alias{as.pixmapRGB}
\alias{as.pixmapGrey}
\alias{as.pixmapIndexed}
\alias{as.pixmapRGB.pixmap}
\alias{as.pixmapGrey.pixmap}
\alias{as.pixmapIndexed.pixmap}
\alias{plot.pixmap}
\alias{plot.pixmapIndexed}
\usage{
pixmap(data=0, nrow=1, ncol=1, col=NULL,
       type=c("grey","rgb","indexed"), bbox=NULL)
as.pixmapRGB(object)
as.pixmapGrey(object, coefs=c(0.30, 0.59, 0.11))
as.pixmapIndexed(object)
\method{plot}{pixmap}(x, ...)
}
\description{
  The family \code{"pixmap"} (``pixel maps'') of classes provides
  methods for creating,
  plotting and converting bitmapped images in three different formats:
  RGB, grey and indexed pixmaps.}
\arguments{
  \item{data}{An optional data vector.}
  \item{nrow}{Vertical size of the image in pixels.}
  \item{ncol}{Horizontal size of the image in pixels.}
  \item{col}{Character vector of colors to use if \code{type} is
    \code{"indexed"}.}
  \item{type}{Type of pixmap to create, see details below.}
  \item{bbox}{Bounding box of the image, vector of length 4 of form \code{c(x1,
      y1, x2, y2)} with coordinates for the lower left corner and upper
    right corner.}
  \item{x, object}{Object of class \code{"pixmap"}}.
  \item{coefs}{Coefficients used for converting a color image to
    grey. The grey value is simply the weighted sum of the channels red,
    green and blue in each pixel.}
  \item{...}{Graphical parameters passed to \code{\link{image}}.}
}
\details{
  Class \code{"pixmap"} has currently the three subclasses
  \code{"pixmapRGB"}, \code{"pixmapGrey"}, and
  \code{"pixmapIndexed"}. The optional bounding box specifies a coordinate
  system for the image and is, e.g., used for labelling the axes
  in plots. Methods for coercion between all formats are available.

  RGB pixmaps are three-dimensional arrays where
  the first two dimensions give the size of the image in pixels and the
  third dimension is for the three color channels red, green and
  blue. Elements of the array must be between 0 (=color off) and 1
  (=color at maximum). 

  Grey and indexed pixmaps are matrices, the element \code{[1,1]}
  corresponds to the upper left corner as usual. For grey pixmaps the
  elements must be between 0 (black) and 1 (white). Indexed pixmaps have
  integer elements, each giving the index number corresponding to the
  palette specified in \code{"col"}. Colors are given using the usual R
  color strings (either names like \code{"red"} or hex values like
  \code{"#FF0000"}).

  The name \code{pixmap} was chosen because both \code{image} and \code{bitmap}
  are already used in R.
}
\author{Friedrich Leisch}
\seealso{\code{\link{read.pnm}}}
\keyword{color}
\examples{
 x <- pixmap(rep(1:8, 9), nrow=6, col=rainbow(8), type="indexed")
 plot(x)
 plot(x, asp=.5)
 plot(as.pixmapGrey(x))

 x <- read.pnm(system.file("pictures/logo.ppm", package="pixmap")[1])
 plot(x)

 x <- seq(-3,3,length=100)
 z1 <- outer(x,x,function(x,y) abs(sin(x)*sin(y)))
 z2 <- outer(x,x,function(x,y) abs(sin(2*x)*sin(y)))
 z3 <- outer(x,x,function(x,y) abs(sin(x)*sin(2*y)))
 z <- pixmap(c(z1,z2,z3), 100, 100, type="rgb", bbox=c(-1,-1,1,1))
 plot(z, axes=TRUE)
}

