# The Gauss-Seiddel algorithm
GS <- function(A, b, x) {
  a <- diag(A)
  diag(A) <- 0
  for (i in 1:length(x)) x[i] <- (b[i] - crossprod(A[i, ], x))/a[i]
  return(x)
}

# The function prepares A so that the diagonals are not zero
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

# The iterations
IterSolve <- function(A, b, x0, eps = 1e-05, maxit = 1000, ...) {
  res <- PrepA(A, b)
  if (res$fail)
    stop("The algorithm failed") else {
      A <- res$A
      b <- res$b
    }
  error = 1000
  n = 1
  all.x = x0
  while (error > eps) {
    x <- c(GS(A, b, x0, ...))
    all.x <- rbind(all.x, x)
    if (any(abs(x) == Inf))
      stop("The algorithm diverges")
    error <- crossprod(x - x0)^0.5
    if (n == maxit) {
      warning("Maxit reached")
      break
    }
    n <- n + 1
    x0 <- x
  }
  if (n < maxit)
    cat("\nConverged after ", (n - 1), "iterations\n")
  return(list(x = x, all.x = all.x))
}