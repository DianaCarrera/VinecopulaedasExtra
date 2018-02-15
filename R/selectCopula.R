selectCopula <- function (useIndepTest = TRUE,
                          candidateCopulas = c(COPULA_NORMAL, COPULA_CLAYTON, COPULA_RCLAYTON, COPULA_GUMBEL, COPULA_RGUMBEL),
                          defaultCopula = COPULA_NONE) {
    f <- function (data) {
        if (useIndepTest) {
        	indepTestStat <- indepTestSim(nrow(data), 2, verbose = FALSE)
        	pvalue <- indepTest(data, indepTestStat)$global.statistic.pvalue
        	if (pvalue > 0.01) {
        		return(indepCopula())
        	}
        }

    	selectedCopula <- NULL
    	selectedStat <- Inf
    	tau <- cor(data[, 1], data[, 2], method = "kendall")
    	for (copula in candidateCopulas) {
    		if (copula == COPULA_NORMAL) {
    			candidateCopula <- fitCopula(copula = normalCopula(0),
                        data = data, method = "itau", estimate.variance = FALSE)@copula
    		} else if (copula == COPULA_CLAYTON) {
    		    if (tau >= 0) {
        			theta <- iTau(claytonCopula(1), min(tau, 0.95))
        			candidateCopula <- claytonCopula(theta)
    		    } else {
                    next
    		    }
    		} else if (copula == COPULA_RCLAYTON) {
    		    if (tau <= 0) {
    		        theta <- iTau(rclaytonCopula(-1), max(tau, -0.95))
    		        candidateCopula <- rclaytonCopula(theta)
    		    } else {
                    next
    		    }
    		} else if (copula == COPULA_GUMBEL) {
    		    if (tau >= 0) {
        			theta <- iTau(gumbelCopula(1), min(tau, 0.95))
        			candidateCopula <- gumbelCopula(theta)
    		    } else {
                    next
    		    }
    		} else if (copula == COPULA_RGUMBEL) {
    		    if (tau <= 0) {
    		        theta <- iTau(rgumbelCopula(-1), max(tau, -0.95))
    		        candidateCopula <- rgumbelCopula(theta)
    		    } else {
    		        next
    		    }            
    		} else {
    			stop("copula type not supported")
    		}
    		candidateStat <- .C("cramer_vonMises", as.integer(nrow(data)),
                    as.integer(ncol(data)), as.double(data), 
                    as.double(pCopula(data, candidateCopula)),
                    stat = double(1.0), PACKAGE = "copula")$stat
    		if (candidateStat < selectedStat) {
    			selectedStat <- candidateStat
    			selectedCopula <- candidateCopula
    		}
    	}

		if (defaultCopula != COPULA_NONE && !is(selectedCopula, "indepCopula")) {
            if (!is.null(selectedCopula)) {
    		    pvalue <- gofCopula(selectedCopula, data, 100, method = "itau",
    		                        simulation = "pb", verbose = FALSE)$pvalue
            } else {
                pvalue <- 0
            }
		   
		}

    	return(selectedCopula)
    }

    return(f)
}
