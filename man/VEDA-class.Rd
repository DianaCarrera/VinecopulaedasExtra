\name{VEDA-class}

\docType{class}

\alias{VEDA-class}
\alias{edaLearn,VEDA-method}
\alias{edaLearnVEDAExtra}
\alias{edaSample,VEDA-method}
\alias{edaSampleVEDAExtra}

\title{Wrappers for the VEDA Estimation and Simulation Functions}

\description{
More flexible specification of the multivariate distribution based 
on vines estimated at each generation of VEDA.
}

\section{Methods}{
  \describe{
    \item{edaLearn}{\code{signature(eda = "VEDA")}}
    \item{edaSample}{\code{signature(eda = "VEDA")}}
  }
}

\details{
The functions \code{edaLearnVEDAExtra} and \code{edaSampleVEDAExtra} replace
the implementation of the learning and sampling steps of VEDA provided by 
the \pkg{copulaedas} package.

\enumerate{
  \item{If the \code{mvdvVector} option is specified, this vector will be used 
    as a template of the multivariate distribution based on vines to be estimated
    at each generation. It is also possible to customize the copula selection 
    strategy through the \code{selectCopulaFunction} option. For more information 
    about the \code{mvdvVector} and \code{selectCopulaFunction} options see the 
    documentation of the \code{\link{estimateMvdv}} function.
  }

}
}

\examples{
setMethod("edaTerminate", "VEDA", 
          edaTerminateCombined(edaTerminateEval, edaTerminateMaxGen))
setMethod("edaReport", "VEDA", edaReportSimple)


# Example that uses the mvdv interface:

mvdvVector <- c(
    3, # cantidad de variables
    MARGIN_NORMAL, # 1st margin (normal)
    NA, # 1st margin (parameters to be estimated by the algorithm)
    MARGIN_NORMAL, # 2nd margin (normal)
    NA, # 2nd margin (parameters to be estimated by the algorithm)
    MARGIN_NORMAL, # 3rd margin (normal)
    NA, # 3rd margin (parameters to be estimated by the algorithm)
    VINE_DVINE,
    2, # number of trees
    COPULA_NORMAL, # 1st copula
    NA, # 1st copula (parameters to be estimated by the algorithm)
    COPULA_PRODUCT, # 2nd copula
    NA # 3rd copula (selected by the algorithm)
)
candidateCopulas <- c(COPULA_NORMAL, COPULA_CLAYTON)
selectCopulaFunction <- selectCopula(useIndepTest = FALSE, candidateCopulas)

veda <- VEDA(mvdvVector = mvdvVector, selectCopulaFunction = selectCopulaFunction,
			 popSize = 300, fEval = 0, fEvalTol = 1e-6, maxGen = 50)

result <- edaRun(veda, fSphere, rep(-300, 3), rep(300, 3))
show(result)


# Examples that uses the C implementation:

veda <- VEDA(vineType = "DVine", vineWeight = "tau", vineTrunc = "AIC",
             indepTest = "cvm", indepTestLevel = 0.01, copulaSelect = "cvm",
             copulaTypes = c("normal", "clayton", "rclayton90",
                             "rclayton180", "rclayton270"),
             popSize = 300, fEval = 0, fEvalTol = 1e-6, maxGen = 50)

result <- edaRun(veda, fSphere, rep(-300, 3), rep(300, 3))
show(result)
}

\seealso{
\code{\linkS4class{mvdv}},
\code{\link{mvdv2vector}},
\code{\link{vector2mvdv}},
\code{\link{estimateMvdv}}.
}

\keyword{classes}
