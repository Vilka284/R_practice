library(Deriv)

#Combine method - (chordes and tangent lines)

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
  cat("����� �������: ", check1,"\n")
  cat("����� �������: ", check2,"\n")
  # f`(x) * f``(x) > 0
  if (substr(check1, 0, 1) == "-" | substr(check2, 0, 1) == "-"){

    print("f`(x) * f``(x) < 0")
    print("������ ����� �� ������� ���� � ����� ����, � �� ������� �������� � �������")
    return("-")
  }else{
    
    print("f`(x) * f``(x) < 0")
    print("������ ����� �� ������� ���� � ������� ����, � �� ������� �������� � �����")
    return("+")
  }
}


find_ab <- function(a, b, e){  # ������������ ������
  
  if (a > x | b < x){
    stop("�������� �� ���� �� ����� �������")
  }
  
  if (abs(a - b) > e){
    if (is.nan(log(a))){
      stop("���� ������� ���� �� ������� (0; +Inf)")
    }
    
    if (a == 0 | b == 0){
      print("� �� � ������ ���� ����� ����!")
      stop("���� ������� ���� �� ������� (0; +Inf)")
    }
    
    if (eq(a) * eq(b) > 0){
      c = (a + b) / 2
      find_ab(a, c, e)
      find_ab(c, b, e)
    }else{
      
      cat("�������� ���� �� �������", aLimit, "��", bLimit, "\n")
      cat("��� ������� ������� ����������� ������ ������ ��� ��� [", a, ",", b, "]\n")
    }
  }
  
}


solveC <- function(aLimit, bLimit){
  find_ab(aLimit, bLimit, eps)
  aLimit = abs(aLimit)
  bLimit = abs(bLimit)
  
  
  s <- isnegative()
  if (s == "-"){
    a0 <- aLimit
    b0 <- bLimit
  }else{
    a0 <- bLimit
    b0 <- aLimit
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
  return(x)
}

aLimit <- 5
bLimit <- 10
#b <- as.integer(readline(prompt = "b: "))
#a <- as.integer(readline(prompt = "a: "))
eps <- 1e-10

solveC(aLimit, bLimit)


tochX <- c()
tochY <- c()
for (i in 1:20) {
  tochX[i] <- i/5
  tochY[i] <- eq(i/5)
}
plot(x = tochX, y = tochY, type = "l")
