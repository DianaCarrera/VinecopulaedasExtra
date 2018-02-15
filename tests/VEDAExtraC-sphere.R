library("copulaedasExtra")

# Optimize the Sphere function using the C implementation of VEDA 
# (based on C-vines, D-vines and R-vines).

setMethod("edaReport", "VEDA", edaReportSimple)
setMethod("edaTerminate", "VEDA", 
        edaTerminateCombined(edaTerminateEval, edaTerminateMaxEvals))

veda <- VEDA(popSize = 300, margin = "norm", fEval = 0,
        fEvalTol = 1e-6, maxEvals = 30000,
        vineWeight = "tau", vineTrunc = "AIC", indepTest = "cvm",
        indepTestLevel = 0.01, copulaSelect = "cvm",
        copulaTypes = c("normal", "clayton", "rclayton90",
                "rclayton180", "rclayton270"))

for (vineType in c("CVine", "DVine", "RVine")) {
    veda@parameters$vineType <- vineType
    result <- edaRun(veda, fSphere, rep(-600, 10), rep(600, 10))
    show(result)
    stopifnot(abs(result@bestEval) < 1e-6)
}
