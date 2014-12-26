library("waveslim")
library("jpeg")
lilie <- readJPEG("pustynia.jpg")
# lilie <- readJPEG("lilie.jpg")

lilie <- lilie[,,1]+lilie[,,2]+lilie[,,3]
lilie <- lilie/max(lilie) 

if (exists("rasterImage")) {
  plot(1:2, type='n')
  rasterImage(lilie, 1.2, 1.27, 1.8, 1.73)
}


lilie.dwt <- dwt.2d(lilie, "haar", 5) # "haar", "d4", "la8"
par(mfrow=c(1,1), pty="s")
# plot.dwt.2d(lilie.dwt)
par(mfrow=c(2,2), pty="s")

# image(1:dim(lilie)[1], 1:dim(lilie)[2], lilie, xlab="", ylab="", main="Original Image")
# image(1:dim(lilie)[1], 1:dim(lilie)[2], idwt.2d(lilie.dwt), xlab="asdasds", ylab="assasdasasdasd",
#       main="Wavelet Reconstruction")
# image(1:dim(lilie)[1], 1:dim(lilie)[2], lilie - idwt.2d(lilie.dwt), xlab="", ylab="", main="Difference")

# lilie.diff <- lilie - idwt.2d(lilie.dwt)
# plot(lilie.diff)

# DOM


thr <- da.thresh(lilie.dwt, alpha = .05, max.level = 2, verbose = FALSE, return.thresh = FALSE)
iwt <- idwt.2d(thr)

lilie.diff <- lilie - idwt.2d(thr)

# image(1:dim(iwt)[1], 1:dim(iwt)[2], iwt, xlab="", ylab="", main="Inverse dwt")

lilie.lost <- sum(lilie.diff^2)/(dim(lilie.diff)[1]^2)
lilie.lost
