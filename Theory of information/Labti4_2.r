# 2

q <- 0.87
pn <- 0.01
pb <- 0.12

p.x.y <- matrix(data = rbind(c(q, pn, pb), c(pn, q, pb)), ncol = 3, nrow = 2)

p.x1 <- sum(p.x.y[,1])
p.x2 <- sum(p.x.y[,2])

p.y1 <- p.x1 * q + p.x2 * pn
p.y2 <- p.x1 * pn + p.x2 * q
p.y3 <- p.x1 * pb + p.x2 * pb

H.y <- -(q+pn)*log2((q+pn)/2) - pb*log2(pb)
H.y.x.umovna <- q*log2(q)+pn*log2(pn)+pb*log2(pb)
C <- H.y - H.y.x.umovna
cat('Пропускна здатність каналу рівна', C*100, 'біт/с')
