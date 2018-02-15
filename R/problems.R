# Grahl, J. and Bosman, P.A.N. and Rothlauf, F.: The Correlation-Triggered
# Adaptive Variance Scaling IDEA. In GECCO'06, July 8-12, 2006.
# Bosman, P.A.N. and Grahl, J. and Rothlauf, F.: SDR: A Better Trigger for
# Adaptive Variance Scaling in Normal EDAs. In GECCO'07, July 7-11, 2007.

fParabolicRidge <- function (x) {
    - x[1] + 100 * sum(x[2:length(x)]^2)
}

fSharpRidge <- function (x) {
    - x[1] + 100 * sqrt(sum(x[2:length(x)]^2))
}


# Ahn, C.W. and Ramakrishna, R.S. and Goldberg, D.E.: Real-coded Bayesian 
# Optimization Algorithm. In Lozano, J.A. and Larrañaga, P. and Inza, I.
# and Bengoetxea, E. (eds.), Toward a New Evolutionary Computation.
# Advances in the Estimation of Distribution Algorithms, Springer-Verlag, 2006.

fRPD <- function (x) { # x_i \in [0,1]
    f <- function (x, y, alpha, beta, gamma) {
        if (x >= gamma && y >= gamma) {
            return(alpha)
        } else {
            return((beta / gamma) * (gamma - sqrt((x^2 + y^2) / 2)))
        }
    }
    - sum(sapply(seq(length = length(x) / 2), 
                 function (i) f(x[2*i-1], x[2*i], 1.0, 0.8, 0.8)))
}

fRNSP <- function (x) { # x_i \in [-5.12,5.12]
    f <- function (x, y, gamma) {
        if (1 - gamma <= x && 1 - gamma <= y &&
            x <= 1 + gamma && y <= 1 + gamma) {
            return(0)
        } else {
            return(-100*(y - x^2)^2 - (1 - x)^2)
        }
    }
    - sum(sapply(seq(length = length(x) / 2), 
                 function (i) f(x[2*i-1], x[2*i], 0.2)))
}


# The implementation of the following functions is based on the description
# given in http://www.it.lut.fi/ip/evo/functions/node1.html.

fSalomon <- function (x) {
    xnorm <- sqrt(sum(x^2))
    - cos(2 * pi * xnorm) + 0.1 * xnorm + 1
}

fWhitley <- function (x) {
    y <- function (j, k) 100 * (x[k] - x[j]^2)^2 + (1 - x[j])^2
    value <- 0
    for (k in seq(along = x)) {
        for (j in seq(along = x)) {
            value <- value + (y(j,k)^2 / 4000 - cos(y(j,k)) + 1)
        }
    }
    return(value)
}


# The implementation of the following functions is based on the 
# CEC 2005 Optimization Benchmark.

fWeierstrass <- function (x) {
    n <- as.integer(length(x))
    .C("calc_weierstrass", x = as.double(x), nreal = n, res = as.double(0))$res -
            .C("calc_weierstrass", x = as.double(rep(0, n)), nreal = n, res = as.double(0))$res
}

fSchwefel <- function (x) {
    .C("calc_schwefel", x = as.double(x), nreal = as.integer(length(x)), res = as.double(0))$res
}
