\name{channels-methods}
\docType{methods}
\title{Methods for Channel Manipulation}
\alias{addChannels}
\alias{getChannels}
\alias{addChannels-methods}
\alias{getChannels-methods}
\alias{addChannels,pixmapRGB-method}
\alias{getChannels,pixmapChannels-method}
\description{Functions for manipulation and extraction of colors from
  channel-based pixmap formats. Methods for generic \code{addChannels} adds the
  color channels of a colored pixmap and returns a grey version. Methods
  for generic \code{getChannels} return numeric matrices or arrays
  containing the specified channels.}
\usage{
  addChannels(object, coef=NULL)
  getChannels(object, colors="all")
}
\arguments{
  \item{object}{Object of class \code{"pixmap"}.}
  \item{coef}{Coefficients for the color channels, a numeric vercot with
    as many elements as there are color channels in the pixmap. The
    default for RGB is \code{c(0.30, 0.59, 0.11)}, which makes a
    luminance-based conversion from color to grey.}
  \item{colors}{Character vector naming the color channels that shall be
    extracted. The default of \code{"all"} returns all channels
    simultaneously.}
}
\keyword{methods}
\author{Friedrich Leisch}
\examples{
  x <- pixmapRGB(rep(1:5, 3), nrow=4)
  plot(x)
  print(x)

  getChannels(x)
  getChannels(x, colors=c("red", "green"))

  y = addChannels(x)
  plot(y)
  print(y)

  ## extract only the red channel
  y = addChannels(x, coef=c(1,0,0))
  plot(y)
}
