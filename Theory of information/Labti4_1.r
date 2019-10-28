# 1 
p.x.y <- matrix(data = cbind(0.276, 0.01, 0.015, 0.009, 0.184, 0.025, 0.015, 0.03, 0.34), ncol = 3, nrow = 3)

#сума елементів к стовпця
p.yk<-c(sum(p.x.y[1,]), sum(p.x.y[2,]), sum(p.x.y[3,]))
#сума елементів і рядка
p.xi<-c(sum(p.x.y[,1]), sum(p.x.y[,2]), sum(p.x.y[,3]))

cat('p(yk) - [', p.yk, '] p(xi) - [', p.xi, ']')
#ентропія 
H.x.y <- 0
for (i in 1:nrow(p.x.y)) {
  for (k in 1:ncol(p.x.y)) {
    H.x.y <- -(H.x.y + (p.x.y[k,i] * log2(p.x.y[k,i])))
  }
}
#вбудована ф-ція
H.x.y <- entropy::entropy(p.x.y)

#Умовна ентропія
H.y.x.umovna <- 0
for (i in 1:nrow(p.x.y)) {
  for (k in 1:ncol(p.x.y)) {
    H.x.y.umovna <- H.x.y.umovna + (p.x.y[k,i] * log2(H.x.y))
  }
}
#вбудована ф-ція
H.y.x.umovna <- entropy::KL.empirical(p.yk, p.xi)

cat('Ентропія:',H.x.y,'\n','Умовна ентропія',H.y.x.umovna)

H.y <- entropy::entropy(p.yk)
I.y.x <- H.y - H.y.x.umovna
cat("К-сть інформації І: ", I.y.x, "\n", "Швидкість передачі інформації по каналу", I.y.x*100, "біт/с")

C <- abs(sum(p.yk * log2(p.yk)))
cat('Пропускна здатність каналу рівна', C*100, 'біт/с')

