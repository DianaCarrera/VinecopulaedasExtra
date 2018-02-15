library("copulaedasExtra")

# Test for the inference and simulation algorithms for vines implemented
# in DML: 1) a multivariate sample with a strong dependence between the
# first two variables is generated; 2) a vine (C-vine, D-vine and R-vine)
# is estimated from the sample and simulated; 3) the correlation matrices
# of the original sample and the simulated sampled are compared.

data <- cbind(rcopula(normalCopula(0.8), 500),
        matrix(runif(3 * 500), ncol = 3))

veda <- VEDA(popSize = 200, margin = "norm", vineWeight = "tau", 
        vineTrunc = "AIC", indepTest = "cvm",
        indepTestLevel = 0.01, copulaSelect = "cvm",
        copulaTypes = c("normal", "clayton", "rclayton90", 
                "rclayton180", "rclayton270"))

for (vineType in c("CVine", "DVine", "RVine")) {
    veda@parameters$vineType <- vineType
    model <- copulaedasExtra::edaLearnVEDAExtra(veda, 1, NULL, data, NULL, NULL, NULL)
    simulated <- copulaedasExtra::edaSampleVEDAExtra(veda, 1, model, NULL, NULL)
    simulatedCor <- cor(simulated, method = "kendall")
    for (i in seq(from = 1, to = ncol(data) - 1)) {
        for (j in seq(from = i + 1, to = ncol(data))) {
            if (i == 1 && j == 2) {
                stopifnot(simulatedCor[i,j] > 0.5)
            } else {
                stopifnot(abs(simulatedCor[i,j]) < 0.5)
            }
        }
    }
}
