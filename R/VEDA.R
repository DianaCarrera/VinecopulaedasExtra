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


edaSampleVEDAExtraMvdv <- function (eda, gen, model, lower, upper) {
    popSize <- eda@parameters$popSize
    pop <- rmvdv(model, popSize)
    return(pop)
}
