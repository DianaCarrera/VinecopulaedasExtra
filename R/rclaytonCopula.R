setClass("rclaytonCopula",
         contains = "copula",
         representation = representation(
            clayton = "copula"))


rclaytonCopula <- function (param) {
    new("rclaytonCopula",
        clayton = claytonCopula(1),
        dimension = as.integer(2),
        parameters = param,
        param.names = "param",
        param.lowbnd = -Inf,
        param.upbnd = 0,
        fullname = "Rotated Clayton copula family")
}


rRClaytonCopula <- function(n, copula) {
    copula@clayton@parameters <- -copula@parameters
    u <- rCopula(n, copula@clayton)
    cbind(u[ , 1], 1 - u[ , 2])
}

setMethod("rCopula", signature("numeric", "rclaytonCopula"), rRClaytonCopula)


pRClaytonCopula <- function(u, copula) {
    copula@clayton@parameters <- -copula@parameters
    u[ , 1] - pCopula(cbind(u[ , 1], 1 - u[ , 2]), copula@clayton)
}

setMethod("pCopula", signature("matrix", "rclaytonCopula"), pRClaytonCopula)
setMethod("pCopula", signature("numeric", "rclaytonCopula"), pRClaytonCopula)


dRClaytonCopula <- function(u, copula) {
    copula@clayton@parameters <- -copula@parameters
    dCopula(cbind(u[ , 1], 1 - u[ , 2]), copula@clayton)
}

setMethod("dCopula", signature("matrix", "rclaytonCopula"), dRClaytonCopula)
setMethod("dCopula", signature("numeric", "rclaytonCopula"), dRClaytonCopula)


tauRClaytonCopula <- function(copula) {
    copula@clayton@parameters <- -copula@parameters
    -tau(copula@clayton)
}

setMethod("tau", "rclaytonCopula", tauRClaytonCopula)


iTauRClaytonCopula <- function(copula, tau) {
    -iTau(copula@clayton, -tau)
}

setMethod("iTau", "rclaytonCopula", iTauRClaytonCopula)


hrclaytonCopula <- function (copula, x, v) {
    copula@clayton@parameters <- -copula@parameters
    h(copula@clayton, x, 1 - v)
}

setMethod("h", "rclaytonCopula", hrclaytonCopula)


hinverserclaytonCopula <- function (copula, u, v) {
    copula@clayton@parameters <- -copula@parameters
    hinverse(copula@clayton, u, 1 - v)
}

setMethod("hinverse", "rclaytonCopula", hinverserclaytonCopula)
