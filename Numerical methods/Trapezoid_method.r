library(ggplot2)


f1 <- function(x) {
    return((sqrt((0.7*x+6.1)/(0.8*x+3)) * ((x^2)/(6.1+x))))
}

f2 <- function(x) {
    return(((x^2)*sin(0.8*x))/(1+cos(0.8*x)))
}

f3 <- function(x){
    return(exp(x)/x)
}
# Графік ф-ції

plotf <- function(f, a, b, n){
    # Розбиваємо інтервал (а, б) в n підінтервалів
    seg <- seq.int(a, b, length.out = n)
    
    # Пустий вектор для обрахунку підінтервальних значень
    fx <- vector(length = length(seg))
    
    # Для кожного підінтервалу рахуємо значення функції в ньому
    for (i in 1:length(seg)) {
        fx[i] <- f(seg[i])
    }
    
    # Збираємо усі точки для побудови графіку
    df <- data.frame(xend = seg, 
                     y = rep(0, n), 
                     yend = fx, 
                     yend1 = c(fx[2:n], fx[n]), 
                     xend1 = c(seg[2:n], seg[n]))
    
    ggplot(data = df) + 
        stat_function(fun = f, size = 1.05, alpha = 0.75, color='blue') + 
        geom_segment(aes(x=xend, y=y, xend=xend, yend=yend)) + 
        geom_segment(aes(x=xend, y=yend, xend=xend1, yend=yend1)) + 
        geom_ribbon(aes(x=xend, ymin=y, ymax=yend), fill = 'blue', alpha = 0.3) + 
        geom_area(stat = 'function', fun = f, fill = 'black', alpha = 0.3, xlim = c(a, b)) + 
        xlim(c(a, b))
}

trapezoid <- function(f, a, b, n) {
    if (is.function(f) == FALSE) {
        stop('f повинна бути функцією або змінною!')
    }
    
    h <- (b - a) / n
    
    j <- 1:n - 1
    
    xj <- a + j * h
    
    approx <- (h / 2) * (f(a) + 2 * sum(f(xj)) + f(b))
    
    return(approx)
}


plotf(f3, a = 0, b = 2, n = 100)
trapezoid(f3, a = 1, b = 2, n = 100)
