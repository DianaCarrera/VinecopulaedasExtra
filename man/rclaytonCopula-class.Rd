\name{rclaytonCopula-class}

\docType{class}

\alias{rclaytonCopula-class}
\alias{dCopula,matrix,rclaytonCopula-method}
\alias{dCopula,numeric,rclaytonCopula-method}
\alias{hinverse,rclaytonCopula-method}
\alias{h,rclaytonCopula-method}
\alias{iTau,rclaytonCopula-method}
\alias{pCopula,matrix,rclaytonCopula-method}
\alias{pCopula,numeric,rclaytonCopula-method}
\alias{rCopula,numeric,rclaytonCopula-method}
\alias{tau,rclaytonCopula-method}

\title{Rotated Clayton Copula}

\description{
Clayton copula rotated 90 degrees to support representing negative 
association.
}

\section{Slots}{
  \describe{
    \item{\code{clayton}:}{Original Clayton copula.}
  }
}

\section{Methods}{
  \describe{
    \item{dCopula}{\code{signature(u = "matrix", copula = "rclaytonCopula")}}
    \item{dCopula}{\code{signature(u = "numeric", copula = "rclaytonCopula")}}
    \item{hinverse}{\code{signature(copula = "rclaytonCopula")}}
    \item{h}{\code{signature(copula = "rclaytonCopula")}}
    \item{iTau}{\code{signature(copula = "rclaytonCopula")}}
    \item{pCopula}{\code{signature(u = "matrix", copula = "rclaytonCopula")}}
    \item{pCopula}{\code{signature(u = "numeric", copula = "rclaytonCopula")}}
    \item{rCopula}{\code{signature(n = "numeric", copula = "rclaytonCopula")}}
    \item{tau}{\code{signature(copula = "rclaytonCopula")}}
  }
}

\references{
E. C. Brechmann. Truncated and simplified regular vines and their applications.
Diploma thesis, University of Technology, Munich, Germany, October 2010.
}

\keyword{classes}
