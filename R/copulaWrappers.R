dNormalCopulaWrapper <- function (u, copula) {
    eps <- .Machine$double.eps^0.5
    u[u < eps] <- eps
    u[u > 1 - eps] <- 1 - eps
    copula@parameters <- max(min(copula@parameters, 1-eps), -(1-eps))
    copula:::dnormalCopula(u, copula)
}


dMatClaytonWrapper <- function (u, copula, log=FALSE, ...) {
    eps <- .Machine$double.eps^0.5
    u[u < eps] <- eps
    u[u > 1 - eps] <- 1 - eps
    copula:::dMatClayton(u, copula, log)
}
