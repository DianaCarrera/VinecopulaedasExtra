\name{rgumbelCopula-class}

\docType{class}

\alias{rgumbelCopula-class}
\alias{dCopula,matrix,rgumbelCopula-method}
\alias{dCopula,numeric,rgumbelCopula-method}
\alias{hinverse,rgumbelCopula-method}
\alias{h,rgumbelCopula-method}
\alias{iTau,rgumbelCopula-method}
\alias{pCopula,matrix,rgumbelCopula-method}
\alias{pCopula,numeric,rgumbelCopula-method}
\alias{rCopula,numeric,rgumbelCopula-method}
\alias{tau,rgumbelCopula-method}

\title{Rotated Gumbel Copula}

\description{
Gumbel copula rotated 90 degrees to support representing negative 
association.
}

\section{Slots}{
  \describe{
    \item{\code{gumbel}:}{Original Gumbel copula.}
  }
}

\section{Methods}{
  \describe{
    \item{dCopula}{\code{signature(u = "matrix", copula = "rgumbelCopula")}}
    \item{dCopula}{\code{signature(u = "numeric", copula = "rgumbelCopula")}}
    \item{hinverse}{\code{signature(copula = "rgumbelCopula")}}
    \item{h}{\code{signature(copula = "rgumbelCopula")}}
    \item{iTau}{\code{signature(copula = "rgumbelCopula")}}
    \item{pCopula}{\code{signature(u = "matrix", copula = "rgumbelCopula")}}
    \item{pCopula}{\code{signature(u = "numeric", copula = "rgumbelCopula")}}
    \item{rCopula}{\code{signature(n = "numeric", copula = "rgumbelCopula")}}
    \item{tau}{\code{signature(copula = "rgumbelCopula")}}
  }
}

\references{
E. C. Brechmann. Truncated and simplified regular vines and their applications.
Diploma thesis, University of Technology, Munich, Germany, October 2010.
}

\keyword{classes}
