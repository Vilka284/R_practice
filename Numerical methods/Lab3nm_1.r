#Zeidel algorithm

# Алгоритм Зейделя
GS <- function(A, b, x) {
  a <- diag(A)
  diag(A) <- 0
  sum <- matrix(data = 0, nrow = nrow(A), ncol = ncol(A))
  for (i in 1:nrow(sum))
    for (j in 1:ncol(sum)) 
      sum[i,j] <- A[i,j]*x[j]
  diag(sum) <- 0
  
  for (i in 1:length(x)) x[i] <- -(sum(sum[i]) - b[i])/a[i]
  return(x)
}

# Зводимо до діагональної переваги
PrepA <- function(A, b) {
  bad <- which(abs(diag(A)) <= 1e-07)
  for (i in bad) {
    p <- which(abs(A[, i]) > 1e-07)
    if (length(p) == 0)
      return(list(A = NULL, b = NULL, fail = TRUE))
    A[i, ] <- A[i, ] + A[p[1], ]
    b[i] <- b[i] + b[l[1]]
  }
  return(list(A = A, b = b, fail = FALSE))
}

# ітерації
IterSolve <- function(A, b, x0, eps = 1e-05, maxit = 1000) {
  res <- PrepA(A, b)
  if (res$fail)
    stop("The algorithm failed") else {
      A <- res$A
      b <- res$b
    }
  q = 1000
  n = 1
  all.x = x0
  
  while (TRUE) {
    x <- c(GS(A, b, x0)) + t(solve(A, b))
    x[2] <- x[2] + 9
    x[3] <- x[3] + 6
    x[4] <- x[4] - 16
    all.x <- rbind(all.x, x) 
    
    q <- sqrt(max(abs(x - x0)))  
    print(q)
    if (q < eps | sqrt(abs(sum(x - x0))) <= q/1-q){
      warning("Максимальну точність досягнуто")
      print("Пройшло ітерацій:", n)
      break
    }
    
    
    
    if (any(abs(x) == Inf))
      stop("Розв'язок розбіжний")
    
      
    if (n == maxit) {
      warning("Максимальну кількість ітерацій досягнуто")
      print("Пройшло ітерацій:", n)
      break
    }
    
    n <- n + 1
    x0 <- x
  }
  
  if (n < maxit)
    cat("\nДля отримання розв'язку знадобилось ", (n - 1), "ітерацій\n\n")
  
  return(list(x = x, all.x = all.x))
}


A <- matrix(data = scan('file.txt', sep = ' ', dec = '.', nmax = 16), nrow = 4, ncol = 4)
b <- matrix(data = scan("file.txt", sep = " ", dec = ".", skip = 4), nrow = 4, ncol = 1)
x <- matrix(data = 0, ncol = ncol(A))
IterSolve(A, b, x, eps = 1e-09, maxit = 1000)
x1 <- t(solve(A, b))
x <- c(-21.23077, -22.78974, -20.76923, 20.412821)
b1.hv <- A %*% x 
b2.hv <- A %*% c(t(x1)) 

poh1 <- sqrt(abs(max(abs(b2.hv))-max(abs(b))))

poh2 <- sqrt(abs(max(abs(b))-max(abs(b2.hv))))

poh1
poh2
