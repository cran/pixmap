read.pnm <- function(file, ...){

    fsz <- file.info(file)$size
    con <- file(file, open="rb")
    open(con, open="rb")

    pnmhead <- read.pnmhead(con, fsz)
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
                           n=nr*BytesPerRow, size=1, signed=F)
            
            xx <- NULL
            for (i in 1:length(bxx))
                xx <- c(xx, as.integer.byte(bxx[i]))
            ncb <- BytesPerRow*8
            xxa <- 1-array(xx, c(nl, ncb, nr))
            res <- array(xxa[,1:nc,], c(nl, nc, nr))
        }
        else {
            xx <- readBin(con, "integer",
                          n=ncells, size=1, signed=F)
            res <- array(xx, c(nl, nc, nr))
        }
    }

    res <- res/pnmhead$maxval

    if(nl==1){
        res <- t(res[1,,])
        res <- pixmap(res, type="grey", ...)
    }
    else{
        res1 <- array(0, dim=c(nr, nc, 3))
        res1[,,1] <- t(res[1,,])
        res1[,,2] <- t(res[2,,])
        res1[,,3] <- t(res[3,,])
        res <- pixmap(res1, type="rgb", ...)
    }
    
    res
}

read.pnmhead <- function(con, consize)
{
    argvec <- function(inpstr) {
        nin <- nchar(inpstr)
        res <- unlist(strsplit(gsub("[ \t\m\n]*", " ", inpstr), " "))
        lres <- length(res)
        nres <- nchar(res)
        to5 <- min(5, lres)
        ws <- is.white.space(inpstr)
        here <- 0
        end <- NULL
        for (i in 1:to5) {
            if(is.na(ws[i,1])) break
            if (nres[i] != ws[i,1]) stop("whitespace mismatch")
            li <- nres[i] + ws[i,2]
            here <- here + li
            end <- c(end, here)
        }
        attr(res, "end") <- end
        res
    }

    is.white.space <- function(inpstr)
    {
	tmp <- unlist(strsplit(inpstr, ""))
	ltmp <- length(tmp)
	res <- rep(TRUE, ltmp)
	for (i in 1:ltmp)
            if (regexpr("[ \t\m\n]", tmp[i]) == -1) res[i] <- FALSE
	ans <- matrix(rle(res)$lengths[1:12], ncol=2, byrow=T)
	ans
    }

    strip.comments <- function(inpstr) {
	com1 <- as.integer(regexpr("#", inpstr))
	if (com1 == -1) {
            new <- inpstr
            attr(new, "ncoms") <- attr(inpstr, "ncoms")
	} else {
            ns <- nchar(inpstr)
            com2 <- regexpr("\n", substr(inpstr, com1, stop=ns)) + com1 - 1
            new <- paste(substr(inpstr, 1, com1-1),
                         substr(inpstr, com2, ns), sep="")
            attr(new, "ncoms") <- attr(inpstr, "ncoms") + 1
            new <- strip.comments(new)
            
	}
	new
    }

    s350 <- readChar(con, nchars=min(350, consize))
    ns350 <- nchar(s350)
    attr(s350, "ncoms") <- 0
    
    new <- strip.comments(s350)
    
    ncoms <- attr(new, "ncoms")
    loss1 <- ns350 - nchar(new)
    
    newa <- argvec(new)
    
    lnewa <- length(newa)
    
    P <- unlist(strsplit(newa[1], split=""))
    if (P[1] != "P") {
        close(con)
        stop("not a pnm file")
    }
    if (!(P[2] %in% c("1", "2", "3", "4", "5", "6"))) {
        close(con)
        stop("not a pnm file")
    }
    P2 <- as.integer(P[2])
    if (P2 > 0 && P2 < 4) ascii <- TRUE
    if (P2 > 3 && P2 < 7) ascii <- FALSE
    
    nc <- as.integer(newa[2])
    nr <- as.integer(newa[3])
    
    data <- 5
    datastart <- NULL
    if (P2 == 1 || P2 == 4) {
        type <- "pbm"
        data <- 4
        maxval <- as.integer(1)
        if (lnewa == 3) datastart <- ns350
    }
    if (P2 == 2 || P2 == 5) {
        type <- "pgm"
        maxval <- as.integer(newa[4])
        if (lnewa == 4) datastart <- ns350
    }
    if (P2 == 3 || P2 == 6) {
        type <- "ppm"
        maxval <- as.integer(newa[4])
        if (lnewa == 4) datastart <- ns350
    }
    
    if (is.null(datastart)) {
        end <- attr(newa, "end")
        datastart <- loss1 + end[data-1]
    }
    seek(con, 0)
    
    if (nc < 0 || nr < 0 || maxval < 1 || maxval > 255)
        warning(paste("Possible error reading heading: nc:", nc,
                      "nr:", nr, "maxval:", maxval))
    
    res <- list(nc = nc, nr = nr, maxval = maxval, type=type,
		datastart=datastart, ascii=ascii)
    invisible(res)
}

write.pnm <- function(object, file=NULL, forceplain=FALSE,
                      type=NULL, maxval=255)
{
    if(!inherits(object, "pixmap"))
        error("Can only write pixmap objects")
    
    if(is.null(type)){
        if(inherits(object, "pixmapGray"))
            type <- "pgm"
        else
            type <- "ppm"
    }
    
    type <- match.arg(type, c("pbm", "pgm", "ppm"))
    do <- dim(object)
    
    if(type=="pbm"){
        object <- as.pixmapGrey(object)
        object <- t(object < 0.5)
        storage.mode(object) <- "integer"
    }
    else if(type=="pgm"){
        object <- as.pixmapGrey(object)
        object <- t(round(object*maxval, 0))
        storage.mode(object) <- "integer"
    }
    else{
        object <- as.pixmapRGB(object)
        object <- round(object*maxval, 0)
        object1 <- array(0, dim=c(3, do[2], do[1]))
        object1[1,,] <- t(object[,,1])
        object1[2,,] <- t(object[,,2])
        object1[3,,] <- t(object[,,3])
        object <- object1
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

