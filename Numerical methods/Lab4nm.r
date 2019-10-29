A <- matrix(data = scan('file.txt', sep = ' ', dec = '.', nmax = 16), nrow = 4, ncol = 4)
eps <- 1e-06

PMsolve<-function(A = A, eps = eps){
  y0 <- matrix(data = 1, nrow = nrow(A), ncol = 1)
  l1 <- 999
  l2 <- 0
  q <- l1
  yi <- 0
  
  all.l <- 0
  n.Iter <- 0
  
  while(q >= eps){
    n.Iter <- n.Iter + 1
    yi <- A %*% y0
    l2 <- yi[1]/y0[1]
    q <- abs(l1 - l2)
    l1 <- l2
    y0 <- yi
    all.l <- rbind(all.l, l1)
  }
  
  
  return(list('Власне число' = l1, 'Пройшло ітерацій' = n.Iter, 'Власний вектор' = y0, all.l = all.l))
  
} 