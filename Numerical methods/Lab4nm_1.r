A <- matrix(data = scan('file.txt', sep = ' ', dec = '.', nmax = 9), nrow = 3, ncol = 3)

if(isSymmetric(A)){
  print("ћатриц€ симетрична")
}else{
  print("ƒл€ методу поворот≥в якоб≥ потр≥бна симетрична матриц€! ћожлив≥ нев≥рн≥ розв'€зки")
}

PovorotJacobi <- function(A){
  b <- matrix(data = 0, nrow = nrow(A), ncol = ncol(A))
  L <- lower.tri(A) * A
  diag(L) <- 0 
  #опорний елемент
  a.i.j <- max(L)
  #«находимо ≥ндекси ≥ та j
  i <- which(L == a.i.j, T)[,1]
  j <- which(L == a.i.j, T)[,2]
  
  p <- 2*a.i.j
  q <- A[i,i] - A[j,j]
  d <- sqrt(p^2 + q^2)
  
  if(q != 0){
    r <- abs(q)/2*d
    if (is.nan(sqrt(0.5 + r)) || is.nan(sqrt(0.5 - r))){
      c <- Re(sqrt(as.complex(0.5 + r)))
      s <- sign(p*q) * Re(sqrt(as.complex(0.5 - r)))
    }else{
      c <- sqrt(0.5 + r)
      s <- sign(p*q) * sqrt(0.5 - r)
    }
  }else{
    c <- sqrt(2)/2
    s <- c
  }
  
  
  b[i,i] <- c^2 * A[i,i] + s^2 * A[j,j] + 2*c*s*A[i,j]
  b[j,j] <- s^2 * A[i,i] + c^2 * A[j,j] + 2*c*s*A[i,j]
  
  #b[i,j] = b[j,i] = 0 - перев≥рка
  b[i,j] <- (c^2 - s^2)*A[i,j] + c*s*(A[j,j] - A[i,i])
  b[j,i] <- b[i,j]
  
  for (m in 1:nrow(b)) {
    if (m != i && m != j){
      b[i,m] <- c*A[m,i] + s*A[m,j]
      b[m,i] <- b[i,m]
      b[j,m] <- - (s*A[m,j] + c*A[m,i]) 
      b[m,j] <- b[j,m]
    }
  }
  
  T.i.j <- matrix(data = 0, nrow = nrow(A), ncol = ncol(A))
  diag(T.i.j) <- 1
  T.i.j[i,i] <- c
  T.i.j[i,j] <- -s
  T.i.j[j,i] <- s
  T.i.j[j,j] <- c
  
  print('T matrix')
  print(T.i.j)
  print('b matrix')
  print(b)
  
  return(b)
}

A.returned <- PovorotJacobi(A)

n = 0
while(sum(A.returned) < 10e-6){
  A.returned <- PovorotJacobi(A.returned)
  n = n+1
}

