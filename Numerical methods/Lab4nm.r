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
  
  
  return(list('max власне число' = l1, 'Пройшло ітерацій' = n.Iter, 'Власний вектор' = yi/10e+50, all.l = all.l))
  
} 

PMsolve(A, eps)
y <- matrix( data = c(2.8788289,0.7271020,-0.5341074,2.9719815), ncol = 1)
y.Boika <- c((6.579128376600535), (2.2065866774775555), (-0.9516268457298134), (7.413153359811191))
E <- matrix(data = 0, nrow = nrow(A), ncol = ncol(A))
diag(E) <- 1
c <- E * 10.21556
m <- A - c
b <- matrix(data = 0, nrow = 4, ncol = 1)
solve(m,b)
#l = 10.21556