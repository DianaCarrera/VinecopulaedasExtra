generateMvdv <- function (lowerVector, upperVector) {
    i <- 1
    
    # number of variables
    if (length(lowerVector) != length(upperVector)) {
        stop("different length of lowerVector and upperVector")
    }
    if (lowerVector[i] == upperVector[i]) {
        modelVector <- lowerVector[i]
    } else {
        stop("different number of variables in lowerVector and upperVector")
    }
    i <- i + 1
    
    # marginal distributions
    n <- 0; N <- modelVector[1]
    while (n != N) {
        if (lowerVector[i] == upperVector[i]) {
            modelVector <- c(modelVector, lowerVector[i])
        } else {
            stop("different margin type in lowerVector and upperVector")
        }
        if (lowerVector[i] %in% c(MARGIN_NORMAL, MARGIN_GAMMA, MARGIN_BETA, MARGIN_UNIF)) {
            modelVector <- c(modelVector, runif(1, lowerVector[i+1], upperVector[i+1]))
            modelVector <- c(modelVector, runif(1, lowerVector[i+2], upperVector[i+2]))
            i <- i + 3
        } else if (lowerVector[i] == MARGIN_KERNEL) {
            stop("kernel margins not yet implemented")
        } else {
            stop("unexpected margin type")
        }
        n <- n + 1
    }
    # vine parameters
    if (lowerVector[i] == upperVector[i]) {
        modelVector <- c(modelVector, lowerVector[i])
    } else {
        stop("different vine type in lowerVector and upperVector")
    }
    if (lowerVector[i+1] == upperVector[i+1]) {
        modelVector <- c(modelVector, lowerVector[i+1])
        trees <- lowerVector[i+1]
    } else {
        stop("different vine trees in lowerVector and upperVector")
    }
    i <- i + 2
    for (j in seq(length = trees)) {
        for (k in seq(length = N - j)) {
            if (lowerVector[i] == upperVector[i]) {
                modelVector <- c(modelVector, lowerVector[i])
            } else {
                stop("different copula type in lowerVector and upperVector")
            }
            if (lowerVector[i] == COPULA_PRODUCT) {
                i <- i + 1
            } else if (lowerVector[i] %in% c(COPULA_NORMAL, COPULA_CLAYTON,
                                             COPULA_RCLAYTON, COPULA_GUMBEL,
                                             COPULA_RGUMBEL)) {
                modelVector <- c(modelVector, runif(1, lowerVector[i+1], upperVector[i+1]))
                i <- i + 2
            }  else {
                stop("unexpected copula type")
            }
        }
    }
    
    model <- vector2model(modelVector)
    mvdv <- mvdv(model$vine, model$margins, model$marginParams)
    return(mvdv)
}
