read.pnm <- function(file, ...){

    fsz <- file.info(file)$size
    con <- file(file, open="rb")

    pnmhead <- read.pnmhead(con)
    retval <- read.pnmdata(con, pnmhead, ...)    

    if (fsz != seek(con)) warning(paste("Possible reading error: file size",
		fsz, "bytes, but", seek(con), "bytes read"))
    close(con)
    retval
}

read.pnmdata <- function(con, pnmhead, ...)
{
    as.integer.byte <- function (x) {
        if (x < 0 || x > 255) stop("Not an unsigned byte")
        y <- x
        ans <- integer(8)
        for (i in 8:1) {
            z <- y%%2
            y <- floor(y/2)
            ans[i] <- z
        }
        as.integer(ans)
    }

    ds <- pnmhead$datastart
    seek(con, ds)
    type <- pnmhead$type
    nl <- ifelse(type == "ppm", 3, 1)
    nc <- pnmhead$nc
    nr <- pnmhead$nr
    ncells <- nl*nc*nr
    if (pnmhead$ascii) {
        xx <- scan(con, integer(0), n=ncells)
        res <- array(xx, c(nl, nc, nr))
    }
    else {
        if (type == "pbm") {
            BytesPerRow <- ceiling(nc/8)
            bxx <- readBin(con, "integer",
                           n=nr*BytesPerRow, size=1, signed=FALSE)
            
            xx <- NULL
            for (i in 1:length(bxx))
                xx <- c(xx, as.integer.byte(bxx[i]))
            ncb <- BytesPerRow*8
            xxa <- 1-array(xx, c(nl, ncb, nr))
            res <- array(xxa[,1:nc,], c(nl, nc, nr))
        }
        else {
            xx <- readBin(con, "integer",
                          n=ncells, size=1, signed=FALSE)
            res <- array(xx, c(nl, nc, nr))
        }
    }

    res <- res/pnmhead$maxval

    if(nl==1){
        z = pixmapGrey(t(res[1,,]), ...)
    }
    else{
        z = pixmapRGB(0, ncol=dim(res)[2], nrow=dim(res)[3], ...)
        z@red = t(res[1,,])
        z@green = t(res[2,,])
        z@blue = t(res[3,,])
    }
    
    z
}



write.pnm <- function(object, file=NULL, forceplain=FALSE,
                      type=NULL, maxval=255)
{
    if(!is(object, "pixmap"))
        error("Can only write pixmap objects")
    
    if(is.null(type)){
        if(is(object, "pixmapGray"))
            type <- "pgm"
        else
            type <- "ppm"
    }
    
    type <- match.arg(type, c("pbm", "pgm", "ppm"))
    do <- object@size
    
    if(type=="pbm"){
        object <- as(object, "pixmapGrey")
        object <- t(object@grey < 0.5)
        storage.mode(object) <- "integer"
    }
    else if(type=="pgm"){
        object <- as(object, "pixmapGrey")
        object <- t(round(object@grey*maxval, 0))
        storage.mode(object) <- "integer"
    }
    else{
        object <- as(object, "pixmapRGB")
        object1 <- array(0, dim=c(3, do[2], do[1]))
        object1[1,,] <- t(object@red)
        object1[2,,] <- t(object@green)
        object1[3,,] <- t(object@blue)
        object <- object1
        object <- round(object*maxval, 0)
        storage.mode(object) <- "integer"
    }
    
    code <- 6
    if (type == "pgm") code <- 5
    if (type == "pbm") {
        code <- 4
        forceplain <- TRUE
    }
    if (is.null(file)) file <- paste("Rimage.", type, sep="")
    comment <- "# R write.pnm output"
    if(forceplain) {
        con <- file(file, open="w")
        open(con, open="w")
        code <- code - 3
        cat("P", code, "\n", file=con, sep="")
        cat(comment, "\n", file=con, sep="")
        cat(do[2], " ", do[1], "\n", file=con, sep="")
        if (!(type == "pbm"))
            cat(maxval, "\n", file=con, sep="")

        write(object, ncolumns=3, file=con)
    }
    else {
        con <- file(file, open="wb")
        open(con, open="wb")
        writeChar(paste("P", code, "\n", sep=""), con=con, eos=NULL)
        writeChar(paste(comment, "\n", sep=""), con=con, eos=NULL)
        writeChar(paste(do[2], " ", do[1], "\n", sep=""),
                  con=con, eos=NULL)
        if (!(type == "pbm")) writeChar(paste(maxval, "\n", sep=""),
              con=con, eos=NULL)
        writeBin(as.integer(as.vector(object)), con, size=1)
    }
    close(con)
}

