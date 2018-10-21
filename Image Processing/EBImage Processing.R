###############################################################################
# Image Processing Tutorial
###############################################################################


# install.packages("EBImage")

# Source Package not located in CRAN.
# Use this command to access package.

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

###############################################################################

library("EBImage")

f = system.file("images", "sample.png", package="EBImage")
img = readImage(f)



display(img)

imgc = readImage(system.file("images", "sample-color.png", package="EBImage"))
display(imgc)

nuc = readImage(system.file("images", "nuclei.tif", package="EBImage"))
display(nuc)

###############################################################################

print(img)

imageData(object)[1:5,1:6]

img1 = img+0.5
img2 = 3*img
img3 = (0.2+img)^3

img4 = img[299:376, 224:301]
img5 = img>0.5
img6 = t(img)

print(median(img))

# combine images
imgcomb = combine(img, img2, img3, img4)
display(imgcomb)

img7 = rotate(img, 30)
img8 = translate(img, c(40,70))
img9 = flip(img)


# render as grayscale

colorMode(imgc) = Grayscale

display(imgc)

# low and high pass filtering

flo = makeBrush(21, shape="disc", step=FALSE)^2
flo = flo/sum(flo)

imgflo = filter2(imgc, flo)

fhi = matrix(1, nc=3, nr=3)
fhi[2,2] = -8
imgfhi =filter2(imgc, fhi) 

# 