\name{pixmap-class}
\docType{class}
\alias{pixmap-class}
\alias{pixmapRGB-class}
\alias{pixmapGrey-class}
\alias{pixmapIndexed-class}
\alias{pixmapChannels-class}
\alias{coerce,pixmapGrey,pixmapIndexed-method}
\alias{coerce,pixmapGrey,pixmapRGB-method}
\alias{coerce,pixmapRGB,pixmapGrey-method}
\alias{coerce,pixmapRGB,pixmapIndexed-method}
\alias{coerce,pixmapIndexed,pixmapRGB-method}
\alias{coerce,ANY,pixmapGrey-method}
\alias{coerce,ANY,pixmapIndexed-method}
\title{Class Family "pixmap".}
\description{The family \code{"pixmap"} (``pixel maps'') of classes provides
  methods for creating,
  plotting and converting bitmapped images in currently three different
  formats: RGB (\code{"pixmapRGB"}), grey
  (\code{"pixmapGrey"})and indexed pixmaps (\code{"pixmapIndexed"}).}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("pixmap", ...)} or
  using the creator functions \code{\link{pixmap}} (similar for all child
  classes of name (\code{"pixmapXXX"}).
}
\section{Slots}{
  \describe{
    \item{\code{size}:}{Object of class \code{"integer"} and length 2
      (number of rows and columns).}
    \item{\code{cellres}:}{Object of class \code{"numeric"} and length
      2 specifying the cell resolution of each pixel in user coordinates.}
    \item{\code{bbox}:}{Object of class \code{"numeric"} and length 4,
      the coordinates of the bounding box (x bottom, y bottom, x top, y
      top).}
    \item{\code{channels}:}{A character vector naming the channel slots
      of the object (\code{NULL} for indexed pixmaps).}
    \item{\code{red}, \code{green}, \code{blue}:}{Only for class
      \code{"pixmapRGB"} with matrices specifying the red, green
      and blue
      channel of the picture.}
    \item{\code{grey}:}{Only for class
      \code{"pixmapGrey"}, a matrix specifying the grey intensity
      (0=black, 1=white) of the picture.}
    \item{\code{col}:}{Only for class
      \code{"pixmapGrey"}, a character vector with a map of color
      names.}
    \item{\code{index}:}{Only for class
      \code{"pixmapIndexed"}, an integer matrix with codes from the
      color map.
    }
  }
}
\section{Details}{
  Class \code{"pixmap"} specifies the basic geometry of a picture: the
  size in pixels together with information for an optional coordinate
  system, see \code{\link{pixmap}} for details.

  Grey and indexed pixmaps are besically matrices (contained in the
  \code{grey} or \code{index} slot, respectively). The element \code{[1,1]}
  corresponds to the upper left corner as usual. For grey pixmaps the
  elements must be between 0 (black) and 1 (white). Indexed pixmaps have
  integer elements, each giving the index number corresponding to the
  palette specified in slot \code{"col"}.
  Colors are given using the usual R
  color strings (either names like \code{"red"} or hex values like
  \code{"#FF0000"}). Alternatively, a function to create a color palette
  can be specified, see \code{\link{rainbow}} or
  \code{\link{heat.colors}} for examples.

  RGB pixmaps have three matrices for each of the three color
  channels. Elements of the matrices must be between 0 (=color off) and
  1 (=color at maximum intensity).

  Methods for coercion between all formats are available.

  Class \code{"pixmapChannels"} is a
  helper parent class currently containing classes \code{"pixmapRGB"}
  and \code{"pixmapGrey"}.
}
\author{Friedrich Leisch}
\seealso{\code{\link{pixmap}}}
\keyword{classes}
