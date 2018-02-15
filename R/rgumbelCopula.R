setClass("rgumbelCopula",
         contains = "copula",
         representation = representation(
            gumbel = "copula"))


rgumbelCopula <- function (param) {
    new("rgumbelCopula",
        gumbel = gumbelCopula(1),
        dimension = as.integer(2),
        parameters = param,
        param.names = "param",
        param.lowbnd = -Inf,
        param.upbnd = 0,
        fullname = "Rotated Gumbel copula family")
}


rRGumbelCopula <- function(n, copula) {
    copula@gumbel@parameters <- -copula@parameters
    u <- rCopula(n, copula@gumbel)
    cbind(u[ , 1], 1 - u[ , 2])
}

setMethod("rCopula", signature("numeric", "rgumbelCopula"), rRGumbelCopula)


pRGumbelCopula <- function(u, copula) {
    copula@gumbel@parameters <- -copula@parameters
    u[ , 1] - pCopula(cbind(u[ , 1], 1 - u[ , 2]), copula@gumbel)
}

setMethod("pCopula", signature("matrix", "rgumbelCopula"), pRGumbelCopula)
setMethod("pCopula", signature("numeric", "rgumbelCopula"), pRGumbelCopula)


dRGumbelCopula <- function(u, copula) {
    copula@gumbel@parameters <- -copula@parameters
    dCopula(cbind(u[ , 1], 1 - u[ , 2]), copula@gumbel)
}

setMethod("dCopula", signature("matrix", "rgumbelCopula"), dRGumbelCopula)
setMethod("dCopula", signature("numeric", "rgumbelCopula"), dRGumbelCopula)


tauRGumbelCopula <- function(copula) {
    copula@gumbel@parameters <- -copula@parameters
    -tau(copula@gumbel)
}

setMethod("tau", "rgumbelCopula", tauRGumbelCopula)


iTauRGumbelCopula <- function(copula, tau) {
    -iTau(copula@gumbel, -tau)
}

setMethod("iTau", "rgumbelCopula", iTauRGumbelCopula)


hrgumbelCopula <- function (copula, x, v) {
    copula@gumbel@parameters <- -copula@parameters
    h(copula@gumbel, x, 1 - v)
}

setMethod("h", "rgumbelCopula", hrgumbelCopula)


hinversergumbelCopula <- function (copula, u, v) {
    copula@gumbel@parameters <- -copula@parameters
    hinverse(copula@gumbel, u, 1 - v)
}

setMethod("hinverse", "rgumbelCopula", hinversergumbelCopula)
