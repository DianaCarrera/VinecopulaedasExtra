library("copulaedasExtra")

# Extensive test for the different parameter values of the vine inference
# and simulation algorithms implemented in DML.

veda <- VEDA(popSize = 300, margin = "norm")

for (vineType in c("CVine", "DVine", "RVine")) {
for (vineWeight in c("tau", "cvm")) {
for (vineTrunc in c("", "AIC")) {
for (indepTest in c("", "tau", "cvm")) {
for (indepTestLevel in c(0.01, 0.05, 0.1)) {
for (copulaTypes in list(c("normal"), c("normal", "clayton", "rclayton90", "rclayton180", "rclayton270"))) {
    veda@parameters$vineType <- vineType
    veda@parameters$vineWeight <- vineWeight
    veda@parameters$vineTrunc <- vineTrunc
    veda@parameters$indepTest <- indepTest
    veda@parameters$indepTestLevel <- indepTestLevel
    veda@parameters$copulaSelect <- "cvm"
    veda@parameters$copulaTypes <- copulaTypes

    cat("vineType:", veda@parameters$vineType, "\n")
    cat("vineWeight:", veda@parameters$vineWeight, "\n")
    cat("vineTrunc:", veda@parameters$vineTrunc, "\n")
    cat("indepTest:", veda@parameters$indepTest, "\n")
    cat("indepTestLevel:", veda@parameters$indepTestLevel, "\n")
    cat("copulaTypes:", veda@parameters$copulaTypes, "\n")
    cat("copulaSelect:", veda@parameters$copulaSelect, "\n")

    selectedPop <- matrix(runif(100 * 10, -100, 100), nrow = 100, ncol = 10)
    selectedEval <- runif(100, -100, 100)
    model <- copulaedasExtra::edaLearnVEDAExtra(veda, 1, NULL,
            selectedPop, selectedEval, rep(-100, 10), rep(100, 10))
    pop <- copulaedasExtra::edaSampleVEDAExtra(veda, 1, model,
            rep(-100, 10), rep(100, 10))
}}}}}}
