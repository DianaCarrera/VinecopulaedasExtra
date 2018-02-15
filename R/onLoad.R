.onLoad <- function (libname, pkgname) {
    setMethod("edaLearn", "VEDA", edaLearnVEDAExtra)
    setMethod("edaSample", "VEDA", edaSampleVEDAExtra)
    setMethod("dCopula", signature("matrix", "normalCopula"), dNormalCopulaWrapper)
    setMethod("dCopula", signature("numeric", "normalCopula"), dNormalCopulaWrapper)
    setMethod("dCopula", signature("matrix", "claytonCopula"), dMatClaytonWrapper)
    setMethod("dCopula", signature("numeric", "claytonCopula"),
              function (u, copula, ...) dMatClaytonWrapper(matrix(u, ncol = dim(copula)), copula, ...))
} 
