\name{generateMvdv}

\alias{generateMvdv}

\title{Random Multivariate Distributions Constructed from Vines}

\description{
This function generates random multivariate distributions constructed from 
vines, i.e., random objects of class \code{"mvdv"}.
}

\usage{
generateMvdv(lowerVector, upperVector)
}

\arguments{
  \item{lowerVector}{Numeric vector with the structure described in \strong{Details}.}
  \item{upperVector}{Numeric vector with the structure described in \strong{Details}.}
}

\value{
An object of class \code{"mvdv"}.
}

\details{
The \code{lowerVector} and \code{upperVector} vectors must follow the
structure accepted by the \code{\link{vector2mvdv}} function. The value 
of each parameter of the multivariate distribution is generated
randomly in the interval determined by the \code{lowerVector} and
\code{upperVector} vectors. The positions corresponding to the dimension,
type of marginal distributions, type of vine, number of vine trees and
type of copulas must have equal values in both vectors.
}

\seealso{
\code{\linkS4class{mvdv}},
\code{\link{mvdv2vector}},
\code{\link{vector2mvdv}}.
}

\examples{
lowerVector <- c(
  3,
  MARGIN_NORMAL, -10, 0,
  MARGIN_GAMMA, 0, 0,
  MARGIN_NORMAL, -100, 0,
  VINE_DVINE,
  1,
  COPULA_PRODUCT,
  COPULA_NORMAL, -1
)
upperVector <- c(
  3,
  MARGIN_NORMAL, 10, 5,
  MARGIN_GAMMA, 5, 3,
  MARGIN_NORMAL, 100, 50,
  VINE_DVINE,
  1,
  COPULA_PRODUCT,
  COPULA_NORMAL, 1
)

mvdv <- generateMvdv(lowerVector, upperVector)
show(mvdv)
}