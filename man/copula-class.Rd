\name{copula-class}

\docType{methods}

\alias{dCopula,matrix,claytonCopula-method}
\alias{dCopula,numeric,claytonCopula-method}
\alias{dCopula,matrix,normalCopula-method}
\alias{dCopula,numeric,normalCopula-method}

\title{Wrappers for the Copula Density Functions}

\description{
The package contains wrappers for the implementation of the \code{dCopula}
generic function for the normal and Clayton copulas that avoid numerical
errors at the boundaries of the \eqn{[0,1]} interval.  
}

\section{Methods}{
  \describe{
    \item{dCopula}{\code{signature(u = "matrix", copula = "claytonCopula")}}
    \item{dCopula}{\code{signature(u = "numeric", copula = "claytonCopula")}}
    \item{dCopula}{\code{signature(u = "matrix", copula = "normalCopula")}}
    \item{dCopula}{\code{signature(u = "numeric", copula = "normalCopula")}}
  }
}