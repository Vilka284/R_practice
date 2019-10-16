library(imager)
library(DescTools)
library(RImagePalette)
library(raster)
library(geostatsp)
library(spatialEco)
#1
image <- jpeg::readJPEG('image.jpg', native = TRUE)

s <- c(0.11, 0.25, 0.35, 0.5, 0.57, 0.67, 0.77, 0.81, 0.9, 1)
plot(s, type = "s", xlab = "p(x)", ylab = "Func")

#2
# Ентропія зображення
Entropy(as.matrix(image))

#3
#Дискретизовані зображення з кроками 2 і 4
disc_img2 <- resize(image, round(height(image)/2),  round(width(image)/2))
disc_img2 <- grayscale(disc_img2)
display_image(disc_img2)

disc_img4 <- resize(image, round(height(image)/4),  round(width(image)/4))
disc_img4 <- grayscale(disc_img4)
display_image(disc_img4)

#4
#Квантовані зображення з кроками 8, 16 та 64
quant_img8 <- quantize_image(image, n=8)
display_image(quant_img8)

quant_img16 <- quantize_image(image, n=16)
display_image(quant_img16)

quant_img64 <- quantize_image(image, n=64)
display_image(quant_img64)

#5
#Ентропія дискретизованих та квантованих зображень
Entropy(as.matrix(disc_img2))
Entropy(as.matrix(disc_img4))

Entropy(as.matrix(quant_img8))
Entropy(as.matrix(quant_img16))
Entropy(as.matrix(quant_img64))

#6
#Ресамплінг дискретизованого зображення - відновлення
disc2 <- as.matrix(disc_img2) %>% raster()
disc4 <- as.matrix(disc_img4) %>% raster()
img <- as.matrix(image) %>% raster()

img_ResampledFrom2 <- resample(disc2, img, method = 'bilinear')

img_ResampledFrom2bicubic <- resample(disc2, img, method = 'bicubic')

img_ResampledFrom4 <- resample(disc4, img, method = 'bilinear')

img_ResampledFrom4bicubic <- resample(disc4, img, method = 'bicubic')

#7
#відносна ентропія відновленого зображення та вхідного
relEntropy2 <- cbind(image = as.matrix(image), res = as.matrix(image))
kl.divergence(relEntropy2, overlap = FALSE)

relEntropy4 <- cbind(image = as.matrix(image), res = as.matrix(img_ResampledFrom4))
kl.divergence(relEntropy4, overlap = FALSE)

relEntropy2bicubic <- cbind(image = as.matrix(image), res = as.matrix(img_ResampledFrom2))
kl.divergence(relEntropy2, overlap = FALSE)

relEntropy4bicubic <- cbind(image = as.matrix(image), res = as.matrix(img_ResampledFrom4))
kl.divergence(relEntropy4, overlap = FALSE)

#8
#відносна ентропія квантованого зображення та вхідного
relEntropy8 <- cbind(image = as.matrix(image), res = as.matrix(quant_img8))
kl.divergence(relEntropy8, overlap = FALSE)

relEntropy16 <- cbind(image = as.matrix(image), res = as.matrix(quant_img16))
kl.divergence(relEntropy16, overlap = FALSE)

relEntropy64 <- cbind(image = as.matrix(image), res = as.matrix(quant_img64))
kl.divergence(relEntropy64, overlap = FALSE)
