\name{estimateMvdv}

\alias{estimateMvdv}

\title{Estimate a Multivariate Distribution via Vines}

\description{
This function performs the estimation of a multivariate distribution that 
represents the distribution of a sample given by using a vine factorization.
}

\usage{
estimateMvdv(data, vector, selectCopulaFunction = selectCopula())
}

\arguments{
  \item{data}{Data matrix of observations.}
  \item{vector}{Numeric vector with the structure of the multivariate 
    distribution.}
  \item{selectCopulaFunction}{Function that perform the selection 
    of each bivariate copula in the vine.}
}

\details{
The \code{vector} argument describes the structure of the multivariate distribution 
(the marginal distributions and the vine) and it should have the structure described 
in \code{\link{mvdv2vector}}. The \code{selectCopulaFunction} argument must 
be a function that expects a bivariate sample and returns a bivariate copula.
The \code{\link{selectCopula}} function can be used to create functions
with this signature and different copula selection strategies.
}

\value{
An object of class \code{"mvdv"}.
}

\seealso{
\code{\linkS4class{mvdv}},
\code{\link{selectCopula}},
\code{\link{mvdv2vector}},
\code{\link{vector2mvdv}}.
}

\examples{
# Create a sample from a 3-dimensional normal distribution.

U <- rCopula(100, normalCopula(0.8, dim = 3))
data <- matrix(0, nrow = nrow(U), ncol = ncol(U))
data[,1] <- qnorm(U[,1], mean = 0, sd = 1)
data[,2] <- qnorm(U[,2], mean = 0, sd = 10)
data[,3] <- qnorm(U[,3], mean = 0, sd = 100)


# Estimate a D-vine factorization. All the parameters
# of the multivariate distribution are given.

vector <- c(
    3, # number of variables
    MARGIN_NORMAL, 0, 1, # 1st margin (normal, mean and std)
    MARGIN_NORMAL, 0, 10, # 2nd margin (normal, mean and std)
    MARGIN_NORMAL, 0, 100, # 3rd margin (normal, mean and std)
    VINE_DVINE,
    1, # number of vine trees
    COPULA_PRODUCT, # 1st copula
    COPULA_NORMAL, 0.75 # 2nd copula (normal, rho)
)
model <- estimateMvdv(data, vector)
show(model)


# Estimate a R-vine factorization. The model is partially specified,
# i.e., the algorithm must select some of the parameters.

vector <- c(
    3, # number of variables
    MARGIN_NORMAL, # 1st margin (normal)
    NA, # 1st margin (parameters to be estimated by the algorithm)
    MARGIN_NORMAL, 0, 10, # 2nd margin (normal, mean and std)
    MARGIN_NORMAL, 0, 100, # 3rd margin (normal, mean and std)
    VINE_RVINE,
    2, # number of vine trees
    COPULA_NORMAL, # 1st copula
    NA, # 1st copula (parameters to be estimated by the algorithm)
    NA, # 2nd copula (to be selected by the algorithm)
    COPULA_CLAYTON, 10 # 3rd copula (Clayton, theta)
)
model <- estimateMvdv(data, vector)
show(model)


# Estimate a multivariate distribution with margins specified by the user.

# Use a normal distribution as an example.
fuserdist <- function (x) unlist(fnorm(x, NULL, NULL))
puserdist <- function (q, params) pnorm(q, mean = params[1], sd = params[2])
duserdist <- function (x, params) dnorm(x, mean = params[1], sd = params[2])
quserdist <- function (p, params) qnorm(p, mean = params[1], sd = params[2])

U <- rCopula(100, indepCopula(3))
data <- matrix(0, nrow = nrow(U), ncol = ncol(U))
data[,1] <- qnorm(U[,1], mean = 0, sd = 1)
data[,2] <- qnorm(U[,2], mean = 0, sd = 10)
data[,3] <- qnorm(U[,3], mean = 0, sd = 100)

vector <- c(
    3,# number of variables
    MARGIN_USERDIST, # 1st margin
    NA, # 1st margin (parameters to be estimated by the algorithm)
    MARGIN_USERDIST, # 2nd margin (userdist)
    2, 0, 10, # 2nd margin (normal as userdist, number of parameters, mean, std)
    MARGIN_USERDIST, # 3rd margin (userdist)
    NA, # 3rd margin (parameters to be estimated by the algorithm)
    VINE_DVINE,
    2, # number of vine trees
    COPULA_NORMAL, # 1st copula
    NA, # 1st copula (parameters to be estimated by the algorithm)
    NA, # 2nd copula (to be selected by the algorithm)
    COPULA_CLAYTON, 10 # 3rd copula (Clayton, theta)
)
mvdv <- estimateMvdv(data, vector)

dmvdv(mvdv, data)
pmvdv(mvdv, matrix(data[1,], nrow = 1))
rmvdv(mvdv, 10)
}
