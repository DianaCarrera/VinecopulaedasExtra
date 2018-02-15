\name{mvdv-convert}

\alias{mvdv2vector}
\alias{vector2mvdv}

\title{Convert \code{"mvdv"} Objects}

\description{
Functions to convert a \code{"mvdv"} object to a vector and vice versa.
}

\usage{
mvdv2vector(mvdv)
vector2mvdv(vector)
}

\arguments{
  \item{mvdv}{Object of class \code{"mvdv"}.}
  \item{vector}{Numeric vector with the structure described in \strong{Details}.}
}

\details{
The numeric vector should have the following structure (see the 
\strong{Examples} section for an illustrative example): \itemize{
\item An integer that specifies the number of variables.
\item For each variable, the type of the marginal distribution and its
  parameters with the following structure: \itemize{
    \item One of the \code{MARGIN_*} constants. This parameter is required.
    \item Concatenated parameters of the margin, i.e., \code{0 and 1}
      for a standard Normal marginal distribution. When this vector is
      used as a template model for the \code{\link{estimateMvdv}} function,
      it can be specified as \code{NA} to indicate that the algorithm must
      estimate the parameters of the margin from the data. 
  }
\item One of the One of the \code{VINE_*} constants that specifies the vine type.
\item For each copula in the vine, the type of the copula and its
  parameters with the following structure: \itemize{
    \item One of the \code{COPULA_*} constants. When this vector is
      used as a template model for the \code{\link{estimateMvdv}} function,
      it can be specified as \code{NA} to indicate that the algorithm must
      select the copula type and estimate its parameters. In this case the
	  parameters of the copula are not specified.
	\item Concatenated parameters of the copula, i.e., \code{0.5} for a Normal
	  copula with correlation parameter equal to 0.5. When this vector is
      used as a template model for the \code{\link{estimateMvdv}} function and
      a \code{COPULA_*} constant is given for the copula type, it can be 
      specified as \code{NA} to indicate that the algorithm must estimate
      the parameters of the copula from the data.
  }
}

See the documentation of the \code{\linkS4class{mvdv}} class for a 
description of the available slots.
}

\value{
\code{mvdv2vector} returns a numeric vector and \code{vector2mvdv} returns 
an object of class \code{"mvdv"}.
}

\seealso{
\code{\linkS4class{mvdv}},
\code{\link{MARGIN_NORMAL}},
\code{\link{MARGIN_KERNEL}},
\code{\link{MARGIN_GAMMA}},
\code{\link{MARGIN_BETA}},
\code{\link{MARGIN_UNIF}},
\code{\link{MARGIN_USERDIST}},
\code{\link{VINE_DVINE}},
\code{\link{VINE_RVINE}},
\code{\link{copula2number}},
\code{\link{number2copula}}.
}

\examples{
vector <- c(
  3, # Number of variables.
  MARGIN_NORMAL, # Type of the 1st marginal.
  0, 1, # Parameters of the 1st marginal (mean and std).
  MARGIN_GAMMA, # Type of the 2nd marginal.
  2, 1, # Parameters of the 2nd marginal (shape and scale).
  MARGIN_NORMAL, # Type of the 3rd marginal.
  0, 100, # Parameters of the 3rd marginal (mean and std).
  VINE_DVINE, # Type of vine.
  2, # Number of trees.
  COPULA_NORMAL, # Type of the 1st copula.
  0.5, # Parameter of the 1st copula.
  COPULA_CLAYTON, # Type of the 2nd copula.
  5, # Parameter of the 2nd copula.
  COPULA_NORMAL, # Type of the 3rd copula.
  0.75 # Parameter of the 3rd copula.
)

mvdv <- vector2mvdv(vector)
all.equal(vector, mvdv2vector(mvdv))
}