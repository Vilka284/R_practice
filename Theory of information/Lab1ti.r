library(imager)
library(DescTools)

image <- load.image('image.png')
hist(image)
#Розмірність зображень
#dim - N M depth chanels 
pix.image <- c(dim(image)[1:2])
# N * M, кількість пікселів
pixels <- pix.image[1] * pix.image[2]

#Створюємо матрицю з 16 ствопцями посортавиними за індексами свічення
gray.image <- grayscale(image)
image.intensity <- sort(gray.image) %>% as.matrix() 
image.intensity <- matrix(data = image.intensity, ncol = 16)

#Використовуємо коробчасту діаграму для відображення ймовірностей свічення
image.intensity %>% boxplot(main = "Інтенсивність свічення в n-ному стовпці", type = 'l', col = 1:16)

#Розрахунок ентропії вручну
#image.intensity <- sort(gray.image) %>% as.vector()
entropy1 = 0
p.x = 0
n = 0

for (j in 1:16) {
  n = matrix(table(image.intensity[,j]))
  
  for (i in 1:nrow(n)) {
    # рахуємо ймовірність х
    p.x <- n[i,] / pixels
    entropy1 <- entropy1 + (-1 * ( p.x * log2(p.x)))
  }
}

#Розрахунок ентропії по формулі
entropy2 = Entropy(image.intensity)

entropy1
entropy2
