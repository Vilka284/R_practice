library(Deriv)

equation <- "log10(x) - 7/(2*x + 6)"

check1 <- Deriv(equation)
check2 <- Deriv(Deriv(equation))

# f(x)
eq <- function(x)
  return(log10(x) - 7/(2*x + 6))

# f'(x)
eq.1 <- function(x)
  return(1/(log(10) * x) + 14/(2 * x + 6)^2)

isnegative <- function(){
  # f`(x) * f``(x) > 0
  if (substr(check1, 0, 1) == "-" | substr(check2, 0, 1) == "-"){
    cat("ѕерша пох≥дна: ", check1,"\n")
    cat("ƒруга пох≥дна: ", check2,"\n")

    print("f`(x) * f``(x) < 0")
    print("ЎукаЇмо кор≥нь за методом хорд з л≥вого к≥нц¤, а за методом дотичних з правого")
    return("-")
  }else{
    cat("ѕерша пох≥дна: ", check1, "\n")
    cat("ƒруга пох≥дна: ", check2, "\n")
    
    print("f`(x) * f``(x) < 0")
    print("ЎукаЇмо кор≥нь за методом хорд з правого к≥нц¤, а за методом дотичних з л≥вого")
    return("+")
  }
}

s <- isnegative()

a <- 2
b <- 4
#b <- as.integer(readline(prompt = "¬вед≥ть b: "))
eps <- 1e-10

if (s == "-"){
  a0 <- a
  b0 <- b
}else{
  a0 <- b
  b0 <- a
}

xp1 <- a0 - eq(a0)/eq.1(a0)
xp2 <- b0 - eq(b0)*(b0-a0)/(eq(b0) - eq(a0))

while ((xp2 - xp1) > eps){
  xn1 = xp1 - eq(xp1)*(xp2-xp1)/(eq(xp2) - eq(xp1))
  xn2 = xp2 - eq(xp2)/eq.1(xp2)
  xp1 = xn1
  xp2 = xn2
}

x = (xp1 + xp2)/2
x
