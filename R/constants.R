MARGIN_NORMAL <- 1
MARGIN_KERNEL <- 2
MARGIN_GAMMA <- 3
MARGIN_BETA <- 4
MARGIN_UNIF <- 5
MARGIN_USERDIST <- 6

VINE_DVINE <- 1
VINE_RVINE <- 2

COPULA_NONE <- -1
COPULA_PRODUCT <- 1
COPULA_NORMAL <- 2
COPULA_CLAYTON <- 3
COPULA_RCLAYTON <- 4
COPULA_GUMBEL <- 5
COPULA_RGUMBEL <- 6


copula2number <- function (copula) {
    if (is(copula, "indepCopula")) {
        return(COPULA_PRODUCT)
    } else if (is(copula, "normalCopula")) {
        return(COPULA_NORMAL)
    } else if (is(copula, "claytonCopula")) {
        return(COPULA_CLAYTON)
    } else if (is(copula, "rclaytonCopula")) {
        return(COPULA_RCLAYTON)
    } else if (is(copula, "gumbelCopula")) {
        return(COPULA_GUMBEL)
    } else if (is(copula, "rgumbelCopula")) {
        return(COPULA_RGUMBEL)
    } else {
        stop('unexpected copula object')
    }
}

number2copula <- function (number, parameters) {
    if (number == COPULA_PRODUCT) {
        return(indepCopula())
    } else if (number == COPULA_NORMAL) {
        return(normalCopula(parameters))
    } else if (number == COPULA_CLAYTON) {
        return(claytonCopula(parameters))
    } else if (number == COPULA_RCLAYTON) {
        return(rclaytonCopula(parameters))
    } else if (number == COPULA_GUMBEL) {
        return(gumbelCopula(parameters))
    } else if (number == COPULA_RGUMBEL) {
        return(rgumbelCopula(parameters))
    } else {
        stop('unexpected copula number')
    }
}
