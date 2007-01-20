library("pixmap")

## this triggered a bug in R <= 1.9.1

x <- pixmapIndexed(rep(1:8, 9), nrow=6, col=hsv(runif(8),runif(8),runif(8)))
plot(x)
print(x)
file <- tempfile()
write.pnm(x, file=file)
unlink(file)

###**********************************************************

# coercion of indexed -> RGB 

x1 <- as(x, "pixmapRGB")
x2 <- as(x1, "pixmapIndexed")
x3 <- as(x2, "pixmapRGB")
stopifnot(all.equal(x, x2))
stopifnot(all.equal(x1, x3))

###**********************************************************

