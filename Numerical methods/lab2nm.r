A <- matrix(data = scan("file.txt", sep = " ", dec = ".", nmax = 16),
           nrow = 4, ncol = 4)
b <- matrix(data = scan("file.txt", sep = " ", dec = ".", skip = 4), 
           nrow = 4, ncol = 1)

s <- matrix(data = NA, nrow = nrow(A), ncol = ncol(A))
r <- matrix(data = NA, nrow = nrow(A)+1, ncol = ncol(A))
r[1,] <- A[,1]
s[1,] <-  r[1,] / sqrt(sum(r[1,]^2))
r[2,] <- A[,2] - as.integer(A[,2] %*% s[1,]) * s[1,]
count = 0
for (i in 1:nrow(A)) {
 if(A[i, 1] == A[1, i] & A[i, 2] == A[2, i] & A[i, 3] == A[3, i] & A[i, 4] == A[4, i]){
   count = count + 1
 }
}

if(nrow(A) == count){
  print("symetric!")
}else{
  print("asymetric!!!")
}

#побудова ортонормованої системи s12

counter = 2
for(i in 2:nrow(A)){
  s[i,] <-  r[i,] / sqrt(sum(r[i,]^2))
  summ <- 0 
  for (j in 2:counter) {
    sum = sum + as.integer(A[,i] %*% s[j-1,]) * s[j-1,]
  }
  r[i+1,] <- A[,i] - sum
  counter = counter + 1
}
s
r
#розвязок
x <- list()
for (i in 1:nrow(A)) {
  x[i] = r[n+1, i] / r[n+1, n+1]
}
x
solve(A, b)
