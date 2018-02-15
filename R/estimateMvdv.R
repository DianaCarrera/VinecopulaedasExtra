estimateMargins <- function (data, model) {
    margins <- model$margins
    for (k in seq(along = margins)) {
        if (!is.null(model$marginParams[[k]])) next
        if (identical(margins[k], "norm")) {
            norm <- fnorm(data[,k], NULL, NULL)
            model$marginParams[[k]] <- norm
        } else if (identical(margins[k], "kernel")) {
            kernel <- fkernel(data[,k], NULL, NULL)
            model$marginParams[[k]] <- kernel
        } else if (identical(margins[k], "gamma")) {
            gamma <- fgamma(data[,k], NULL, NULL)
            model$marginParams[[k]] <- gamma
        } else if (identical(margins[k], "beta")) {
            beta <- fbeta(data[,k], NULL, NULL)
            model$marginParams[[k]] <- beta
        } else if (identical(margins[k], "unif")) {
            unif <- funif(data[,k], NULL, NULL)
            model$marginParams[[k]] <- unif
        } else if (identical(margins[k], "userdist")) {
            # The fuserdist function must return a numeric vector with the
            # parameters of the marginal distribution.
            fuserdist <- if (exists("fuserdist")) fuserdist else function (...) NULL
            model$marginParams[[k]] <- list(params = fuserdist(data[,k]))
        } else {
            stop("unexpected margin type")
        }
    }
    return(model$marginParams)
}

estimateVine <- function (uniformData, model, selectCopulaFunction) {
    VINE <- model$vine
    vineFitSelectCopula <- function (vine, j, i, x, y) {
        data <- cbind(x, y)
        tau <- cor(x, y, method = "kendall")
        copula <- VINE@copulas[[j, i]]
        if (is(copula, "numeric") && copula == COPULA_NONE) {
            selectedCopula <- selectCopulaFunction(data)
        } else if (is(copula, "copula")) {
            selectedCopula <- copula
        } else {
            # The type of the copula was pre-defined. Estimate its parameters.
            if (copula == COPULA_NORMAL) {
                selectedCopula <- fitCopula(copula = normalCopula(0),
                     data = data, method = "itau", estimate.variance = FALSE)@copula
            } else if (copula == COPULA_CLAYTON) {
                theta <- iTau(claytonCopula(1), max(0, min(tau, 0.95)))
                selectedCopula <- claytonCopula(theta)
            } else if (copula == COPULA_RCLAYTON) {
                theta <- iTau(rclaytonCopula(-1), max(0, max(tau, -0.95)))
                selectedCopula <- rclaytonCopula(theta)
            } else if (copula == COPULA_GUMBEL) {
                theta <- iTau(gumbelCopula(1), max(min(tau, 0.95)))
                selectedCopula <- gumbelCopula(theta)
            } else if (copula == COPULA_RGUMBEL) {
                theta <- iTau(rgumbelCopula(-1), max(max(tau, -0.95)))
                selectedCopula <- rgumbelCopula(theta)
            } else {
                stop('unexpected copula type')
            }
        }
        return(selectedCopula)
    }
    fit <- vineFit(class(model$vine), uniformData, selectCopula = vineFitSelectCopula,
                   trees = VINE@trees, truncMethod = "", optimMethod = "")
    return(fit@vine)
}


model2vector <- function (margins, marginParams, vine) {
    v <- length(margins) # 1 -- number of variables
    for (i in seq(along = margins)) { # 2...i -- margin type and its parameters
        if (identical(margins[i], "norm")) { 
            v <- c(v, MARGIN_NORMAL)
            v <- c(v, c(marginParams[[i]]$mean, marginParams[[i]]$sd)) # margin parameters
        } else if (identical(margins[i], "kernel")) { 
            v <- c(v, MARGIN_KERNEL)
            v <- c(v, c(length(marginParams[[i]]$X), marginParams[[i]]$X, marginParams[[i]]$h)) # margin parameters
        } else if (identical(margins[i], "gamma")) { 
            v <- c(v, MARGIN_GAMMA)
            v <- c(v, c(marginParams[[i]]$shape, marginParams[[i]]$rate)) # margin parameters
        } else if (identical(margins[i], "beta")) { 
            v <- c(v, MARGIN_BETA)
            v <- c(v, c(marginParams[[i]]$shape1, marginParams[[i]]$shape2)) # margin parameters
        } else if (identical(margins[i], "unif")) {
            v <- c(v, MARGIN_UNIF)
            v <- c(v, c(marginParams[[i]]$min, marginParams[[i]]$max)) # margin parameters
        } else if (identical(margins[i], "userdist")) {
            v <- c(v, MARGIN_USERDIST)
            v <- c(v, length(c(marginParams[[i]]$params)), c(marginParams[[i]]$params)) # margin parameters
        } else {
            stop("unexpected margin type")
        }
    }
    v <- c(v, if (is(vine, "DVine")) VINE_DVINE else VINE_RVINE)
    v <- c(v, vine@trees) # number of trees
    # copulas tree-by-tree (starting from the first tree)
    for (j in seq(length = vine@trees)) {
        for (i in seq(length = vine@dimension - j)) {
            copula <- vine@copulas[[j, i]]
            if (is(copula, "indepCopula")) {
                v <- c(v, COPULA_PRODUCT)
            } else if (is(copula, "normalCopula")) {
                v <- c(v, COPULA_NORMAL)
                v <- c(v, copula@parameters)
            } else if (is(copula, "claytonCopula")) {
                v <- c(v, COPULA_CLAYTON)
                v <- c(v, copula@parameters)
            } else if (is(copula, "rclaytonCopula")) {
                v <- c(v, COPULA_RCLAYTON)
                v <- c(v, copula@parameters)
            } else if (is(copula, "gumbelCopula")) {
                v <- c(v, COPULA_GUMBEL)
                v <- c(v, copula@parameters)
            } else if (is(copula, "rgumbelCopula")) {
                v <- c(v, COPULA_RGUMBEL)
                v <- c(v, copula@parameters)
            } else {
                stop('unexpected copula type')
            }
        }
    }
    return(v)
}

mvdv2vector <- function (mvdv) {
    return(model2vector(mvdv@margins, mvdv@marginParams, mvdv@vine))
}

vector2model <- function (v) {
    n <- v[1]
    i <- 2
    margins <- character(0)
    marginParams <- list()
    while (length(margins) != n) {
        if (v[i] == MARGIN_NORMAL) {
            margins <- c(margins, "norm")
            if (is.na(v[i+1])) {
                marginParams <- c(marginParams, list(NULL))
                i <- i + 2
            } else {
                marginParams <- c(marginParams, list(list(mean = v[i+1], sd = v[i+2])))
                i <- i + 3
            }
        } else if (v[i] == MARGIN_KERNEL) {
            margins <- c(margins, "kernel")
            if (is.na(v[i+1])) {
                marginParams <- c(marginParams, list(NULL))
                i <- i + 2
            } else {
                X <- v[seq(from = i+2, length = v[i+1])]
                h <- v[i + 1 + 1 + v[i+1]]
                marginParams <- c(marginParams, list(list(X = X, h = h)))
                i <- i + 1 + 1 + v[i+1] + 1
            }
        } else if (v[i] == MARGIN_GAMMA) {
            margins <- c(margins, "gamma")
            if (is.na(v[i+1])) {
                marginParams <- c(marginParams, list(NULL))
                i <- i + 2
            } else {
                marginParams <- c(marginParams, list(list(shape = v[i+1], rate = v[i+2])))
                i <- i + 3
            }
        } else if (v[i] == MARGIN_BETA) {
            margins <- c(margins, "beta")
            if (is.na(v[i+1])) {
                marginParams <- c(marginParams, list(NULL))
                i <- i + 2
            } else {
                marginParams <- c(marginParams, list(list(shape1 = v[i+1], shape2 = v[i+2])))
                i <- i + 3
            }
        } else if (v[i] == MARGIN_UNIF) {
            margins <- c(margins, "unif")
            if (is.na(v[i+1])) {
                marginParams <- c(marginParams, list(NULL))
                i <- i + 2
            } else {
                marginParams <- c(marginParams, list(list(min = v[i+1], max = v[i+2])))
                i <- i + 3
            }
        } else if (v[i] == MARGIN_USERDIST) {
            margins <- c(margins, "userdist")
            if (is.na(v[i+1])) {
                marginParams <- c(marginParams, list(NULL))
                i <- i + 2
            } else {
                params <- v[seq(from = i+2, length = v[i+1])]
                marginParams <- c(marginParams, list(list(params = params)))
                i <- i + 1 + 1 + v[i+1]
            }
        } else {
            stop("unexpected margin type")
        }
    }
    if (v[i] == VINE_DVINE) {
        vine <- CVine(dimension = n, trees = v[i+1],
                      copulas = matrix(list(NULL), n - 1, n - 1))
    } else {
        vine <- DVine(dimension = n, trees = v[i+1],
                      copulas = matrix(list(NULL), n - 1, n - 1))        
    }
    i <- i + 2
    for (j in seq(length = vine@trees)) {
        for (k in seq(length = vine@dimension - j)) {
            if (is.na(v[i])) {
                copula <- COPULA_NONE
                i <- i + 1
            } else {
                if (v[i] == COPULA_PRODUCT) {
                    copula <- indepCopula()
                    i <- i + 1
                } else if (v[i] == COPULA_NORMAL) {
                    if (is.na(v[i+1])) {
                        copula <- COPULA_NORMAL
                        i <- i + 2
                    } else {
                        copula <- normalCopula(v[i+1])
                        i <- i + 2
                    }
                } else if (v[i] == COPULA_CLAYTON) {
                    if (is.na(v[i+1])) {
                        copula <- COPULA_CLAYTON
                        i <- i + 2
                    } else {
                        copula <- claytonCopula(v[i+1])
                        i <- i + 2
                    }
                } else if (v[i] == COPULA_RCLAYTON) {
                    if (is.na(v[i+1])) {
                        copula <- COPULA_RCLAYTON
                        i <- i + 2
                    } else {
                        copula <- rclaytonCopula(v[i+1])
                        i <- i + 2
                    }
                } else if (v[i] == COPULA_GUMBEL) {
                    if (is.na(v[i+1])) {
                        copula <- COPULA_GUMBEL
                        i <- i + 2
                    } else {
                        copula <- gumbelCopula(v[i+1])
                        i <- i + 2
                    }
                } else if (v[i] == COPULA_RGUMBEL) {
                    if (is.na(v[i+1])) {
                        copula <- COPULA_RGUMBEL
                        i <- i + 2
                    } else {
                        copula <- rgumbelCopula(v[i+1])
                        i <- i + 2
                    }
                }  else {
                    stop('unexpected copula type')
                }
            }
            vine@copulas[[j, k]] <- copula
        }
    }

    return(list(margins = margins, marginParams = marginParams, vine = vine))
}

vector2mvdv <- function(vector) {
    model <- vector2model(vector)
    return(mvdv(model$vine, model$margins, model$marginParams))
}

estimateMvdv <- function (data, vector, selectCopulaFunction = selectCopula()) {
    # Estimate the parameters of the marginal distributions.
    model <- vector2model(vector)
    margins <- model$margins
    marginParams <- estimateMargins(data, model)
    # Transform the data into U(0,1) variables.
    p <- match(margins, c("norm", "kernel", "gamma", "beta", "unif", "userdist"))
    puserdist <- if (exists("puserdist")) puserdist else function (...) NULL
    p <- c(pnorm, pkernel, pgamma, pbeta, punif, puserdist)[p]
    uniformData <- sapply(seq(length = ncol(data)),
         function (i) do.call(p[[i]], c(list(data[ , i]), marginParams[[i]])))
    # Estimate the vine.
    vine <- estimateVine(uniformData, model, selectCopulaFunction)
    # Create the vector with the parameters of the margins and the vine.
    return(mvdv(vine, margins, marginParams))
}
