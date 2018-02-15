\name{mvdv-class}
\Rdversion{1.1}
\docType{class}

\alias{mvdv-class}
\alias{dmvdv}
\alias{pmvdv}
\alias{rmvdv}

\title{Multivariate Distributions Constructed from Vines}

\description{
Density, distribution function, and random generator for a
multivariate distribution via vines. The class \code{"mvdv"} is 
designed after the class \code{"mvdc"} class from the copula
package.
}

\section{Objects from the Class \code{"mvdv"}}{
Objects can be created by calling the functions
\code{\link{estimateMvdv}}, \code{\link{generateMvdv}}
and \code{\link{vector2mvdv}}.
}

\usage{
dmvdv(mvdv, x)
pmvdv(mvdv, x)
rmvdv(mvdv, n)
}

\arguments{
  \item{mvdv}{Object of class \code{"mvdv"}.}
  \item{x}{A vector of the vine dimension or a matrix with number of 
    columns being the vine dimension, giving the coordinates of the 
    points where the density or distribution function needs to be 
    evaluated.}
  \item{n}{Number of observations to be generated.}
}

\value{
\code{dmvdv} gives the density, \code{pmvdv} gives the cumulative 
distribution function, and \code{rmvdv} generates random variates.
}

\section{\code{"mvdv"} Slots}{
  \describe{
    \item{\code{vine}:}{Object of class \code{"Vine"}.}
    \item{\code{margins}:}{Character vector that determines the marginal
      distributions of the multivariate distribution. Each component
      determines the density, distribution, and quantile function names. 
      For example, \code{"norm"} can be used to specify marginal distribution, 
      because \code{"dnorm"}, \code{"pnorm"}, and \code{"qnorm"} are all available.
      Supported values for each component are: \code{"norm"}, \code{"kernel"}, 
      \code{"gamma"} and \code{"userdist"}. The value \code{"userdist"} supports
      user-defined distribution, i.e., the user can freely specify \code{"duserdist"},
      \code{"puserdist"}, and \code{"quserdist"}.}
    \item{\code{marginParams}:}{Object of class \code{"list"}. Each component of 
      this list is a list with named components which are used to specify the 
      parameters of the marginal distributions.}
  }
}

\seealso{
\code{\link{mvdc}}, 
\code{\link{estimateMvdv}}, 
\code{\link{generateMvdv}},
\code{\link{mvdv2vector}}, 
\code{\link{vector2mvdv}}.
}

\keyword{classes}
