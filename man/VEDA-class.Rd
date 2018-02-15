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
the \pkg{copulaedas} package. Two alternative interfaces are provided:

\enumerate{
  \item{If the \code{mvdvVector} option is specified, this vector will be used 
    as a template of the multivariate distribution based on vines to be estimated
    at each generation. It is also possible to customize the copula selection 
    strategy through the \code{selectCopulaFunction} option. For more information 
    about the \code{mvdvVector} and \code{selectCopulaFunction} options see the 
    documentation of the \code{\link{estimateMvdv}} function.
  }

  \item{If \code{mvdvVector} is not specified, an implementation in C of the
    estimation and simulation algorithms for C-vines, D-vines and R-vines will
    be used. The following options are recognized by this interface:

	\describe{
	  \item{\code{popSize}}{Population size. Default value: \code{100}.}

	  \item{\code{margin}}{Marginal distributions. If this argument is \code{"xxx"},
	    the algorithm will search for three \R functions named \code{fxxx}.
	    \code{pxxx} and \code{qxxx} to fit each marginal distribution
	    and evaluate the cumulative distribution function and its inverse,
	    respectively. Default value: \code{"norm"}.}

	  \item{\code{vineType}}{Vine type. Supported values are: \code{"CVine"}
	    (Canonical vine) and \code{"DVine"} (D-vine) and \code{"RVine"}. 
	    Default value: \code{"RVine"}.}

	  \item{\code{vineWeight}}{Weights of the edges used for the selection
	    of the structure of the vines. Supported values are:
	    \code{"tau"} (Kendall's tau) and \code{"cvm"} (Cramér-von Mises 
	    statistic of the independence test). Default value: \code{"cvm"}.}

	  \item{\code{vineTrunc}}{Method used to automatically truncate the vine if
	    enough dependence is captured in the first trees. Supported values
	    are: \code{"AIC"} and \code{""} (no truncation). Default
	    value: \code{"AIC"}.}

	  \item{\code{indepTest}}{Independence test performed as the first
	    step of the copula selection procedure. Supported values are:
	    \code{""} (disabled), \code{"tau"} (test based on Kendall's tau)
	    and \code{"cvm"} (distance between the product and empirical
	    copulas measured by a Cramér-von Mises statistic). 
	    Default value: \code{"cvm"}.}

	  \item{\code{indepTestLevel}}{Significance level of the independence
	    test. Default value: \code{0.01}.}

	  \item{\code{copulaTypes}}{A \code{character} vector specifying the candidate
	    copulas. Supported values are: \code{"normal"} (normal copula),
	    \code{"rclayton90"} (Clayton copula rotated 90 degrees), 
	    \code{"rclayton180"} (Clayton copula rotated 180 degrees),
	    and \code{"rclayton270"} (Clayton copula rotated 270 degrees). 
	    Default value: \code{c("normal")}.}

	  \item{\code{copulaSelect}}{Selection strategy for the candidate copulas.
	    Supported values are: \code{"AIC"} \code{"cvm"} (distance between the 
	    candidate and empirical copulas measured by a Cramér-von Mises statistic). 
	    Default value: \code{"AIC"}.}
	}
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
