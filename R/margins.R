dkernel <- function (p, X, h) {
    n <- length(X)
    sum(dnorm((p - X) / h)) / (n * h)
}


fgamma <- function(x, lower, upper) {
    m <- min(x)
    if (m < 0) x <- x + abs(m)
    mean.x <- mean(x)
    var.x <- var(x)
    rate <- mean.x / var.x
    shape <- mean.x^2 / var.x
    list(shape = shape, rate = rate)
}


funif <- function(x, lower, upper) {
    if (is.null(lower) || is.null(upper)) {
        list(min = min(x), max = max(x))
    } else {
        list(min = lower, max = upper)
    }
}


qkantorovic <- function (p, xorder) {
    n <- length(xorder) - 2
    q <- rep(0, length(p))
    for (k in seq(from = 0, to = n)) {
        q <- q + 0.5 * (xorder[k + 1] + xorder[k + 2]) * choose(n, k) * p^k * (1 - p)^(n - k)
    }
    return(q)    
}


fbeta <- function (x, lower, upper) {
    loglik <- function (s) sum(dbeta(x, s[1], s[2], log = TRUE))
    s <- optim(c(1, 1), loglik, control = list(fnscale = -1))$par
    list(shape1 = s[1], shape2 = s[2])
}
