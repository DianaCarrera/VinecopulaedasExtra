\name{selectCopula}

\alias{selectCopula}

\title{Bivariate Copula Selection}

\description{
This function is intended to be a template to create functions to be
passed as the \code{selectCopulaFunction} argument of the 
\code{\link{estimateMvdv}} function, although it may be used to
select a bivariate copulas in other context. 
}

\usage{
selectCopula(useIndepTest, candidateCopulas, defaultCopula)
}

\arguments{
  \item{useIndepTest}{Indicate whether an independence test should
    be performed first. The default value is \code{TRUE}.}
  \item{candidateCopulas}{A vector of \code{COPULA_*} constants that
    sets the group of candidate copulas from which the selection is 
    performed. The default value is \code{c(COPULA_NORMAL, COPULA_CLAYTON, 
    COPULA_RCLAYTON, COPULA_GUMBEL, COPULA_RGUMBEL)}.}
  \item{defaultCopula}{Indicate whether a default copula should be used
    when no copula among the candidate copulas can represent the dependence
    structure appropriately. The default value is \code{COPULA_NONE}.}
}

\details{
The following procedure is followed for the copula selection:

\itemize{
  \item{If \code{useIndepTest} is set to \code{TRUE} the procedure starts with 
    an independence test. The independence test is performed using the 
    \code{\link{indepTestSim}} function and the indpendence copula is selected
    if there is not enough evidence against the null hypothesis at the 
    0.01 significance level. If \code{useIndepTest} is set to \code{FALSE}
    this step is skipped.}
 
  \item{If the independence copula was not selected, the procedure continues
    by applying copula goodness-of-fit tests for the candidate copulas.
    For each candidate copula, the observed value of the Cramer-von Mises 
    statistic implemented in the \code{\link{gofCopula}} function is calculated.
    If \code{defaultCopula} is set to \code{COPULA_NONE}, the candidate 
    copula with the lowest value of the Cramer-von Mises statistic is selected.
    Otherwise, no copula is selected at this step.}
}
}

\value{
This function returns a function (closure) that expects a matrix with
two columns and one row for each observation of the variables. The returned
function selects and returns the bivariate copula that better fits the
data according to the selection strategy that was configured through the
parameters of the \code{selectCopula} function.
}

\seealso{
\code{\link{indepTestSim}}
\code{\link{gofCopula}}
\code{\link{estimateMvdv}}.
}

\examples{
data <- rCopula(250, normalCopula(0.75))
candidateCopulas <- c(COPULA_NORMAL, COPULA_CLAYTON, COPULA_GUMBEL)
selectCopulaFunction <- selectCopula(TRUE, candidateCopulas, COPULA_NONE)
selectedCopula <- selectCopulaFunction(data)
show(selectedCopula)
}
