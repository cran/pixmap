\name{pnm}
\title{Read/Write Portable Anymap Images}
\alias{write.pnm}
\alias{read.pnm}
\alias{read.pnmdata}
\alias{read.pnmhead}
\usage{
read.pnm(file, ...)
write.pnm(object, file="Rimage.pnm", forceplain=FALSE,
          type=NULL, maxval=255)
}
\description{Reading and writing of bitmap images in PBM (black/white), PGM
  (grey) and PPM (color) format.}
\arguments{
  \item{file}{name of the pnm file.}
  \item{...}{Further arguments passed to \code{\link{pixmap}} (like
    \code{bbox}).} 
  \item{object}{an object of class \code{"pixmap"}.}
  \item{forceplain}{If \code{TRUE}, an ASCII pnm file is written.
    Default is to write a binary (raw) file.}
  \item{type}{One of \code{"pbm"}, \code{"pgm"} or \code{"ppm"}. Default
    is to use \code{"pgm"} for grey images and \code{"ppm"} for color
    images.}
  \item{maxval}{The maximum color-component value, the default is a
    colour depth of 8 bits, i.e., the integer 255.}
}
\details{
  \code{read.pnm} reads a pnm file and loads the image into an
  object of class \code{\link{pixmap}}.

  \code{write.pnm} writes an object of class \code{\link{pixmap}} to a
  pnm file, the \code{type} argument controls wheter the written image
  file is a black-and-white bitmap (pbm), grey (pgm) or color (ppm). 

  \code{plot.pnm} plots a pnm object using the command
  \code{\link{image}}. The only difference is that the element \code{[1,1]} of
  \code{pnmobj} is plotted as the upper left corner (plain
  \code{\link{image}} would plot \code{[1,1]} as the lower left corner.
}
\value{
  \code{read.pnm} returns an object of class \code{\link{pixmapRGB}} for color
  pixmaps (ppm), and an object of class \code{\link{pixmapGrey}} for pbm
  and pgm. Note that the \emph{type} of file as determined by the first
  two bytes according to pnm standards is important, \emph{not the
    extension} of the file. In fact, the file name extension is
  completely ignored.
}
\author{Roger Bivand and Friedrich Leisch}
\seealso{\code{\link{pixmap}}}
\keyword{file}
\keyword{color}
\examples{
 x <- read.pnm(system.file("pictures/logo.ppm", package="pixmap")[1])
 plot(x)
 print(x)

 x <- read.pnm(system.file("pictures/logo.pgm", package="pixmap")[1])
 plot(x)

 x <- read.pnm(system.file("pictures/logo.pbm", package="pixmap")[1])
 plot(x)
}
