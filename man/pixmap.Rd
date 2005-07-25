\name{pixmap}
\title{Pixmap Images}
\alias{pixmap}
\alias{pixmapRGB}
\alias{pixmapGrey}
\alias{pixmapIndexed}
\alias{plot,pixmap-method}
\alias{show,pixmap-method}
\alias{[,pixmap-method}

\usage{
pixmap(data=NULL, nrow=dim(data)[1], ncol=dim(data)[2],
       bbox=NULL, bbcent=FALSE, cellres=NULL)
pixmapRGB(data, ...)
pixmapGrey(data, ...)
pixmapIndexed(data, col, ...)
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
  \item{bbox}{Bounding box of the image, vector of length 4 of form \code{c(x1,
      y1, x2, y2)} with coordinates for the lower left corner and upper
    right corner.}
  \item{bbcent}{Logical, if \code{TRUE} the bounding box specifies the
    coordinates of the centers of the lower left and upper right pixels,
    default is the coordinates of the lower left and upper right corner
    of the image.}
  \item{cellres}{Numeric vector of length 1 or 2, specifies the
    resolution of pixels in horizontal and vertical direction. If only
    one value is given, resolution in both directions is identical.}
  \item{col}{Character vector of colors to use for indexed pictures, or
    a function like \code{\link{rainbow}} which can
    be used to create a palette. Colors set to \code{NA} are 
    transparent; this can be used,e.g., for overlaying plots.}
  \item{...}{Additional arguments passed to \code{pixmap()}.}
}
\details{
  If the \code{data} argument is 2- or 3-dimensional, \code{nrow} and
  \code{ncol} default to the first two dimensions of \code{data}, such
  that \code{pixmap} does the expected when given a matrix or an array.
  
  The arguments \code{bbox}, \code{bbcent} and \code{cellres} can be
  used to specify a coordinate system for the image. Note that together
  with \code{nrow} and \code{ncol} the coordinate system is
  overspecified, hence not all parameters must be specified, the rest is
  computed or set to sensible defaults.

  For \code{bbcent=FALSE} we have
  \code{cellres[1] = (bbox[3]-bbox[1])/ncol}
  and
  \code{cellres[2] = (bbox[4]-bbox[2])/nrow}, for  \code{bbcent=TRUE}
  we get
  \code{cellres[1] = (bbox[3]-bbox[1])/(ncol-1)}
  and
  \code{cellres[2] = (bbox[4]-bbox[2])/(nrow-1)}.
  
  The name \code{pixmap} was chosen because both \code{image} and
  \code{bitmap} are already used in R.
}
\author{Friedrich Leisch}
\seealso{\code{\link{pixmap-class}}, \code{\link{read.pnm}}}
\keyword{color}
\examples{
 ## A simple example
 x <- pixmapIndexed(rep(1:8, 9), nrow=6, col=terrain.colors(8))
 plot(x)

 ## The same with different colors, and passing the function instead of
 ## a color vector 
 x <- pixmapIndexed(rep(1:8, 9), nrow=6, col=rainbow)
 plot(x)
 plot(x, asp=.5, axes=TRUE)

 ## Read data from a file
 x <- read.pnm(system.file("pictures/logo.ppm", package="pixmap")[1])
 plot(x)

 ## Another example that math can be beautiful
 x <- seq(-3,3,length=100)
 z1 <- outer(x,x,function(x,y) abs(sin(x)*sin(y)))
 z2 <- outer(x,x,function(x,y) abs(sin(2*x)*sin(y)))
 z3 <- outer(x,x,function(x,y) abs(sin(x)*sin(2*y)))

 ## Notice that we specify a bounding box to get the correct
 ## coordinates on the axes. z1, z2 and z3 are used as red,
 ## green and blue channel, respectively.
 z <- pixmapRGB(c(z1,z2,z3), 100, 100, bbox=c(-1,-1,1,1))
 plot(z, axes=TRUE)

 ## look at a grey version
 plot(as(z, "pixmapGrey"))

 ## subsetting works as expected
 plot(z[1:20,])
 plot(z[,1:40])
 plot(z[1:20,10:40])

 ## overlay different images using transparency
 ## base image as before
 x <- pixmapIndexed(rep(1:8, 9), nrow=6, col=terrain.colors(8))
 plot(x)
 ## make a mask of vertical bars
 mask <- array(0,dim=c(6,12))
 mask[,seq(1,12,3)] <- 1
 ## plot this mask over existing image with transparent and black color
 plot(pixmapIndexed(mask,col=c("NA","#000000")),add=TRUE)
}

