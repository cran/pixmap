pixmap <- function(data=0, nrow=dim(data)[1], ncol=dim(data)[2],
                   col=NULL, type=c("grey", "rgb", "indexed"),
                   bbox=NULL, bbcent=FALSE, cellres=NULL)
{
    type <- match.arg(type)

    cellres <- rep(cellres, length=2)
    if(is.null(bbox)){
        if(is.null(cellres))
            cellres <- c(1,1)
        
        if(is.null(nrow)){
            if(!is.null(ncol))
                nrow <- ceiling(length(data)/ncol)
            else
                stop("Too few dimension attributes (nrow, ncol, bbox)\n")
        }
        else if(is.null(ncol))
            ncol <- ceiling(length(data)/nrow)
        
        if(bbcent)
            bbox <- c(1,1,cellres[1]*ncol, cellres[2]*nrow)
        else
            bbox <- c(0,0,cellres[1]*ncol, cellres[2]*nrow)
    }
    else{
        if(is.null(cellres)){
            if(is.null(nrow)){
                if(!is.null(ncol))
                    nrow <- ceiling(length(data)/ncol)
                else
                    stop("Too few dimension attributes (nrow, ncol, bbox)\n")
            }
            else if(is.null(ncol))
                ncol <- ceiling(length(data)/nrow)

            if(bbcent)
                cellres <- c((bbox[3]-bbox[1])/(ncol-1),
                             (bbox[4]-bbox[2])/(nrow-1))
            else
                cellres <- c((bbox[3]-bbox[1])/ncol,
                             (bbox[4]-bbox[2])/nrow)
        }
        else{
            if(bbcent){
                ncol <- (bbox[3]-bbox[1])/cellres[1]+1
                nrow <- (bbox[4]-bbox[2])/cellres[2]+1
            }
            else{
                ncol <- (bbox[3]-bbox[1])/cellres[1]
                nrow <- (bbox[4]-bbox[2])/cellres[2]
            }
        }
    }
    
    if(type == "indexed"){
        data <- as.integer(data)
        datamin <- min(data)
        if(datamin<=0)
            data <- data - datamin + 1
        datamax <- max(data)
    }
    else{
        datamax <- max(data)
        datamin <- min(data)
        data <- as.numeric(data)
        if(datamax>1 || datamin<0)
            data <- (data - datamin)/(datamax-datamin)
    }
    
    if(type=="rgb"){
        data <- array(data, dim=c(nrow, ncol, 3))
        dimnames(data) <- list(NULL, NULL, c("red","green","blue"))
        class(data) <- c("pixmapRGB", "pixmap")
    }
    else{
        data <- matrix(data, nrow=nrow, ncol=ncol)
        if(type=="indexed"){
            class(data) <- c("pixmapIndexed", "pixmap")
            if(is.null(col))
                col <- heat.colors(datamax)
            else{
                if(is.function(col))
                    col <- col(datamax)
                else {
                    if (is.function(col)) col <- col(datamax)
                    else if(length(col) < datamax){
                        warning("number of of colors smaller than number of data values, recycling\n")
                        col <- rep(col, length=datamax)
                    }
                }
            }
            
            attr(data, "col") <- col
        }
        else{
            class(data) <- c("pixmapGrey", "pixmap")
        }
    }
    
    attr(data, "cellres") <- cellres
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
    attr(x, "cellres") <- attr(object, "cellres")
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
    attr(x, "cellres") <- attr(object, "cellres")
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
    attr(x, "cellres") <- attr(object, "cellres")
    class(x) <- c("pixmapRGB", "pixmap")
    x
}





plot.pixmap <- function(x,  ...)
    plot.pixmapIndexed(as.pixmapIndexed(x), ...)

plot.pixmapIndexed <- function(x, xlab="", ylab="", axes=FALSE, asp=1, ...)
{
    bb <- attr(x, "bbox")
    bc <- attr(x, "cellres")

    X <- seq(bb[1], bb[3], by=bc[1])
    Y <- seq(bb[2], bb[4], by=bc[2])
    
    image(x=X, y=Y, z=t(x[nrow(x):1,]),
          col=attr(x, "col"), xlab=xlab, ylab=ylab,
          axes=axes, asp=asp, ...)
}

image.pixmap <- plot.pixmap


print.pixmap <- function(x, ...)
{
    cat("Pixmap image\n")
    cat("  Type          :", class(x)[1], "\n")
    cat("  Size          :", paste(dim(x)[1:2], collapse="x"), "\n")
    cat("  Resolution    :", paste(attr(x, "cellres"), collapse="x"), "\n")
    cat("  Bounding box  :", attr(x, "bbox"), "\n")
    if(inherits(x, "pixmapIndexed"))
        cat("  Nr. of colors :",
            length(unique(x)), "of", 
            length(attr(x, "col")), "\n")
    cat("\n")
}
        
        
