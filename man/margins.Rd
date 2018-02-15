\name{margins}

\alias{dkernel}
\alias{fgamma}
\alias{fbeta}
\alias{funif}

\title{Marginal Distributions}

\description{
Functions that implement marginal distributions.
}

\usage{
dkernel(p, X, h)

fgamma(x, lower, upper)

fbeta(x, lower, upper)

funif(x, lower, upper)

}

\arguments{
\item{x, q}{Vector of quantiles.}

\item{lower}{Lower bound of the variable.}

\item{upper}{Upper bound of the variable.}

\item{p}{Vector of probabilities.}

\item{X}{Observations of the variable.}

\item{h}{Bandwidth of the kernel.}

\item{xorder}{Order statistics of \code{x}.}
}

\details{
The function \code{dkernel} implements the Gaussian kernel density estimation.
 
The function \code{fgamma} fits the parameters of a Gamma marginal distribution 
to the given sample \code{x}, and \code{funif} does the same for a continuous 
uniform distribution.

See the documentation of the functions that implement the density, 
cumulative distribution and quantile functions (e.g. \code{pgamma}, 
\code{pbeta}, \code{punif}) for information about the parameters 
of each particular distribution.
}
