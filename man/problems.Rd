\name{problems}

\alias{fParabolicRidge}
\alias{fSharpRidge}
\alias{fRPD}
\alias{fRNSP}
\alias{fSalomon}
\alias{fWhitley}
\alias{fWeierstrass}
\alias{fSchwefel}

\title{Benchmark Problems}

\description{
Implementation of a group of well-known benchmark problems usually used
to evaluate the performance of EDAs and other optimization algorithms for
unconstrained global optimization.
}

\usage{
fParabolicRidge(x)
fSharpRidge(x)
fRPD(x)
fRNSP(x)
fSalomon(x)
fWhitley(x)
fWeierstrass(x)
fSchwefel(x)
}

\arguments{
\item{x}{A vector to be evaluated in the function.}
}

\value{
The value of the function for the vector \code{x}
}

\references{
Grahl, J. and Bosman, P.A.N. and Rothlauf, F.: The Correlation-Triggered
Adaptive Variance Scaling IDEA. In GECCO'06, July 8-12, 2006.
Bosman, P.A.N. and Grahl, J. and Rothlauf, F.: SDR: A Better Trigger for
Adaptive Variance Scaling in Normal EDAs. In GECCO'07, July 7-11, 2007.

Ahn, C.W. and Ramakrishna, R.S. and Goldberg, D.E.: Real-coded Bayesian 
Optimization Algorithm. In Lozano, J.A. and Larrañaga, P. and Inza, I.
and Bengoetxea, E. (eds.), Toward a New Evolutionary Computation.
Advances in the Estimation of Distribution Algorithms, Springer-Verlag, 2006.

The implementation of \code{fSalomon} and \code{fWhitley} is based on the 
description given at \url{http://www.it.lut.fi/ip/evo/functions/node1.html}.

The implementation of \code{fWeierstrass} and \code{fSchwefel} was taken 
from the CEC 2005 Optimization Benchmark.
}
