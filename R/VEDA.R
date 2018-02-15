edaLearnVEDAExtra <- function (eda, gen, previousModel, selectedPop,
                               selectedEval, lower, upper) {
    if (is.null(eda@parameters$mvdvVector)) {
        # C interface:
        return(edaLearnVEDAExtraC(eda, gen, previousModel, selectedPop,
                                  selectedEval, lower, upper))
    } else {
        # mvdv interface:
        return(edaLearnVEDAExtraMvdv(eda, gen, previousModel, selectedPop,
                                     selectedEval, lower, upper))
    }
}

edaLearnVEDAExtraC <- function (eda, gen, previousModel, selectedPop,
                                 selectedEval, lower, upper) {
     margin <- eda@parameters$margin
     vineType <- eda@parameters$vineType
     vineWeight <- eda@parameters$vineWeight
     vineTrunc <- eda@parameters$vineTrunc
     indepTest <- eda@parameters$indepTest
     indepTestLevel <- eda@parameters$indepTestLevel
     copulaTypes <- eda@parameters$copulaTypes
     copulaSelect <- eda@parameters$copulaSelect

     if (is.null(margin)) margin <- "norm"
     if (is.null(vineType)) vineType <- "RVine"
     if (is.null(vineWeight)) vineWeight <- "tau"
     if (is.null(vineTrunc)) vineTrunc <- "AIC"
     if (is.null(indepTest)) indepTest <- "cvm"
     if (is.null(indepTestLevel)) indepTestLevel <- 0.01
     if (is.null(copulaTypes)) copulaTypes <- c("normal")
     if (is.null(copulaSelect)) copulaSelect <- "AIC"

     n <- ncol(selectedPop)
     fmargin <- get(paste("f", margin, sep = ""))
     pmargin <- get(paste("p", margin, sep = ""))

     margins <- lapply(seq(length = n),
             function (i) fmargin(selectedPop[ , i], lower[i], upper[i]))
     uniformPop <- sapply(seq(length = n),
             function (i) do.call(pmargin, c(list(selectedPop[ , i]), margins[[i]])))

     vineType <- match(vineType, c("DVine", "RVine")) - 1
     vineWeight <- match(vineWeight, c("tau", "cvm")) - 1
     vineTrunc <- match(vineTrunc, c("", "AIC")) - 1
     indepTest <- match(indepTest, c("", "tau", "cvm")) - 1
     copulaTypes <- match(copulaTypes, c("indep", "normal", "clayton", 
                     "rclayton90", "rclayton180", "rclayton270")) - 1
     copulaSelect <- match(copulaSelect, c("AIC", "cvm")) - 1
     rng <- if (is.null(previousModel)) .Call(C_GSLRngAlloc) else previousModel$rng
     vine <- .Call(C_DMLVineFit, uniformPop, as.integer(vineType),
             as.integer(vineWeight), as.integer(vineTrunc), as.integer(indepTest),
             as.double(indepTestLevel), as.integer(copulaTypes),
             as.integer(copulaSelect), rng)

     return(list(rng = rng, vine = vine, margins = margins))
}

edaLearnVEDAExtraMvdv <- function (eda, gen, previousModel, selectedPop,
                                   selectedEval, lower, upper) {
    mvdvVector <- eda@parameters$mvdvVector
    selectCopulaFunction <- eda@parameters$selectCopulaFunction

    if (is.null(selectCopulaFunction)) selectCopulaFunction <- selectCopula()

    mvdv <- estimateMvdv(selectedPop, mvdvVector, selectCopulaFunction)

    return(mvdv)
}


edaSampleVEDAExtra <- function (eda, gen, model, lower, upper) {
    if (is.null(eda@parameters$mvdvVector)) {
        # C interface:
        return(edaSampleVEDAExtraC(eda, gen, model, lower, upper))
    } else {
        # mvdv interface:
        return(edaSampleVEDAExtraMvdv(eda, gen, model, lower, upper))
    }
}

edaSampleVEDAExtraC <- function (eda, gen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    margin <- eda@parameters$margin

    if (is.null(popSize)) popSize <- 100
    if (is.null(margin)) margin <- "norm"

    qmargin <- get(paste("q", margin, sep = ""))

    uniformPop <- .Call(C_DMLVineRan, model$vine, model$rng, as.integer(popSize))
    pop <- sapply(seq(length = ncol(uniformPop)),
            function (i) do.call(qmargin,
                        c(list(uniformPop[ , i]), model$margins[[i]])))

    return(pop)
} 

edaSampleVEDAExtraMvdv <- function (eda, gen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    pop <- rmvdv(model, popSize)
    return(pop)
}
