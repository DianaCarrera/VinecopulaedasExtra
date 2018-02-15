\name{constants}

\alias{MARGIN_NORMAL}
\alias{MARGIN_KERNEL}
\alias{MARGIN_GAMMA}
\alias{MARGIN_BETA}
\alias{MARGIN_UNIF}
\alias{MARGIN_USERDIST}
\alias{VINE_CVINE}
\alias{VINE_DVINE}
\alias{VINE_RVINE}
\alias{COPULA_NONE}
\alias{COPULA_PRODUCT}
\alias{COPULA_NORMAL}
\alias{COPULA_CLAYTON}
\alias{COPULA_RCLAYTON}
\alias{COPULA_GUMBEL}
\alias{COPULA_RGUMBEL}
\alias{copula2number}
\alias{number2copula}

\title{Constants}

\description{Constants for margin, copula and vine types and 
auxiliary functions.}

\usage{
copula2number(copula)
number2copula(number, parameters)
}

\arguments{
  \item{copula}{Copula object.}
  \item{number}{\code{COPULA_*} constant.}
  \item{parameters}{Vector with copula parameters.}
}

\details{
\code{MARGIN_*} constants specify the types of the marginal distributions
in a \code{mvdv} vector. Supported \code{MARGIN_*} constants are:
\code{MARGIN_NORMAL}, \code{MARGIN_KERNEL}, \code{MARGIN_GAMMA},
\code{MARGIN_BETA}, \code{MARGIN_UNIF}, \code{MARGIN_USERDIST}.

\code{VINE_*} constants specify the vine type in a \code{mvdv} vector. 
Supported \code{VINE_*} constants are: \code{VINE_CVINE},
\code{VINE_DVINE},\code{VINE_RVINE}.

\code{COPULA_*} constants specify the types of the copulas 
in a \code{mvdv} vector. Supported \code{COPULA_*} constants are: 
\code{COPULA_NONE}, \code{COPULA_PRODUCT}, \code{COPULA_NORMAL}, 
\code{COPULA_CLAYTON}, \code{COPULA_RCLAYTON}, \code{COPULA_GUMBEL}, 
\code{COPULA_RGUMBEL}.
}

\value{
\code{copula2number} returns the corresponding \code{COPULA_*} constant
for the given copula. \code{number2copula} builds and returns a copula
object from a given \code{COPULA_*} constant and its parameters.
}

\examples{
copula2number(normalCopula(0.5)) # COPULA_NORMAL
number2copula(COPULA_NORMAL, 0.5) # normalCopula(0.5)
}
