pixmap <- function(data=0, nrow=1, ncol=1, col=NULL,
                   type=c("grey", "rgb", "indexed"),
                   bbox=NULL)
{
    type <- match.arg(type)

    if (missing(nrow)) 
        nrow <- ceiling(length(data)/ncol)
    else if (missing(ncol)) 
        ncol <- ceiling(length(data)/nrow)
    
    if(type=="rgb"){
        data <- array(data, dim=c(nrow, ncol, 3))
        dimnames(data) <- list(NULL, NULL, c("red","green","blue"))
        class(data) <- c("pixmapRGB", "pixmap")
    }
    else{
        data <- matrix(data, nrow=nrow, ncol=ncol)
        if(type=="indexed"){
            class(data) <- c("pixmapIndexed", "pixmap")
            attr(data, "col") <- col
        }
        else{
            class(data) <- c("pixmapGrey", "pixmap")
        }
    }
    
    if(is.null(bbox))
        bbox <- c(1,1,ncol,nrow)
    attr(data, "bbox") <- bbox
    
    data
}


as.pixmapIndexed <- function(object)
    UseMethod("as.pixmapIndexed")

as.pixmapIndexed.pixmap <- function(object)
{
    if(inherits(object, "pixmapIndexed"))
        return(object)
    else if(inherits(object, "pixmapRGB"))
        x <- rgb(object[,,"red"],object[,,"green"],object[,,"blue"])
    else if(inherits(object, "pixmapGrey"))
        x <- grey(object)
    else
        error("Unknown pixmap subclass")
    
    col <- unique(x)
    x <- match(x, col)
    x <- array(x, dim=dim(object)[1:2])
    attr(x, "col") <- col
    attr(x, "bbox") <- attr(object, "bbox")
    class(x) <- c("pixmapIndexed", "pixmap")
    x
}


as.pixmapGrey <- function(object, coefs=c(0.30, 0.59, 0.11))
    UseMethod("as.pixmapGrey")


as.pixmapGrey.pixmap <- function(object, coefs=c(0.30, 0.59, 0.11))
{
    coefs <- coefs/sum(coefs)
    
    if(inherits(object, "pixmapGrey"))
        return(object)
    
    object <- as.pixmapRGB(object)
    x <- coefs[1]*object[,,"red"] + coefs[2]*object[,,"green"] +
        coefs[3]*object[,,"blue"]
    
    x <- array(x, dim=dim(object)[1:2])
    attr(x, "bbox") <- attr(object, "bbox")
    class(x) <- c("pixmapGrey", "pixmap")
    x
}


as.pixmapRGB <- function(object)
    UseMethod("as.pixmapRGB")

as.pixmapRGB.pixmap <- function(object)
{
    if(inherits(object, "pixmapRGB"))
        return(object)

    if(inherits(object, "pixmapGrey"))
        x <- array(object, dim=c(dim(object),3))
    else if(inherits(object, "pixmapIndexed")){
        x <- col2rgb(attr(object,"col")[object])/255
        x <- array(c(x["red",], x["green",], x["blue",]),
                   dim=c(dim(object),3))
    }
    else
        error("Unknown pixmap subclass")

    dimnames(x) <- list(NULL, NULL, c("red","green","blue"))
    attr(x, "bbox") <- attr(object, "bbox")
    class(x) <- c("pixmapRGB", "pixmap")
    x
}





plot.pixmap <- function(x,  ...)
    plot.pixmapIndexed(as.pixmapIndexed(x), ...)

plot.pixmapIndexed <- function(x, xlab="", ylab="", axes=FALSE, asp=1, ...)
{
    bb <- attr(x, "bbox")
    if(is.null(bb))
        bb <- c(1,1,ncol(x),nrow(x))
    
    X <- seq(bb[1], bb[3], length=ncol(x))
    Y <- seq(bb[2], bb[4], length=nrow(x))

    image(x=X, y=Y, z=t(x[nrow(x):1,]),
          col=attr(x, "col"), xlab=xlab, ylab=ylab,
          axes=axes, asp=asp, ...)
}
