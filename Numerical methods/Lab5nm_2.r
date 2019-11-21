library(Deriv)


eps <- 1e-10


solveNLSystem <- function(){
  n <- as.integer(readline(prompt = "Зі скількох рівнянь складається система: "))
  m <- as.integer(readline(prompt = "Скільки змінних фігурує в системах: "))
  print("Введіть назви змінних (наприклад x1, x2, x3): ")
  var.names <- c()
  for (i in 1:m) {
    var.names[i] <- readline(prompt = cat("Змінна ", i, ":"))
  }
  
  equations <- c()
  x <- c()
  
  for(i in  1:n){
    equations[i] <- readline(prompt = cat("Рівняння ", i, ":"))
  }
  
  cat("Щоб почати роботу програми введіть початкове наближення з ", n, " елементів")
  for(i in 1:n){
    x[i] <- as.double(readline(prompt = cat(i, " елемент для ", i, " рівняння:")))
  }
  
  f <- equations
  J <- matrix(data = 0, nrow = length(equations), ncol = as.integer(n))
  J.pop <- matrix(data = 0, nrow = length(equations), ncol = as.integer(n))
  
  #Наповнюємо якобіан
  # df/dx[i]
  for (i in 1:nrow(J)) {
    for (j in 1:ncol(J)) {
      J[i, j] <- Deriv(f[i], var.names[j])
    }
  }
  
  y <- matrix(data = 0, nrow = length(x), ncol = 1)
  xn <- x*100
  
  while(max(abs(xn - x)) > eps){
    
    F.x <- func(x, f)
    
    J.x <- jac(x, J.pop)
    #Приведення до числа
    for (i in 1:nrow(J)) {
      for (j in 1:ncol(J)) {
        J.pop[i, j] <- as.numeric(J.x[i, j])
      }
    }
    
    y <- solve(J.pop, -F.x)
    xn <- x + y
    x <- xn
  }
  
  return(xn)
}


func <- function(x, equations){
  "
  !
  Доступ до змінних через імена
  !
  "
  N <- vector(mode = "list", length = length(x))
  
  tempNames <- c()
  for (i in 1:length(x)) {
    tempNames[i] <- paste("x", i, sep = '')
  }
  
  names(N) <- tempNames
  
  for (i in 1:length(x)) {
    N[i] <- x[i]
  }
  
  "
  Обчислення рівняння
    UNCOMMENT NECESSARY !
    sorry for shitcode :(
    
    I haven't seen better way to dynamic create values
  "
 x1 <- N$x1
 x2 <- N$x2
 x3 <- N$x3
  # x4 <- N$x4
  # x5 <- N$x5
  # x6 <- N$x6
  # x7 <- N$x7
  
  for (i in 1:length(x)) {
    x[i] <- eval(parse(text = equations[i]))
  }
 
  return(x)
}

jac <- function(x, J){
  N <- vector(mode = "list", length = length(x))
  
  tempNames <- c()
  for (i in 1:length(x)) {
    tempNames[i] <- paste("x", i, sep = '')
  }
  
  names(N) <- tempNames
  
  for (i in 1:length(x)) {
    N[i] <- x[i]
  }
  
  "
  Обчислення рівняння
    UNCOMMENT NECESSARY !
    sorry for shitcode :(
    
    I haven't seen better way to dynamic create values
  "
  x1 <- N$x1
  x2 <- N$x2
  x3 <- N$x3
  # x4 <- N$x4
  # x5 <- N$x5
  # x6 <- N$x6
  # x7 <- N$x7
  
  for (i in 1:nrow(J)) {
    for (j in 1:ncol(J)) {
      J[i,j] <- eval(parse(text = J[i,j]))
    }
  }
  
  return(J)
}

solveNLSystem()
