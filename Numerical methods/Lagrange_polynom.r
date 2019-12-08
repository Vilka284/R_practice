n <- 9
x <- c(-10, -7, -6, -5, -4, -3,  -1, 0, 1, 2, 6, 7, 8, 9)
y<- sin(x)







LagrangeInterpol <- function(x, y, n, xk){
    yk <- 0
    for (i in 1:n) {
        p <- 1
        for (j in 1:n) {
            if (j != i)
                p <- p*((xk-x[j])/(x[i]-x[j]))
        }
        yk <- yk + p*y[i]
    }
    return(yk)
}

xs <- x
ys <- y

for (i in min(xs):max(xs)) {
    xk <- i
    yk <- LagrangeInterpol(xs, ys, n, xk)
    append(ys, yk, after = length(xs))
    append(xs, xk, after = length(ys))
}

plot(x, y, type = 'b')

f <- splinefun(x, y, method = "natural")

curve(f(x), -10, 10, col = "darkblue", add = TRUE)
