setClass("mvdv",
    representation = representation(
        vine = "Vine",
        margins = "character",
        marginParams = "list"))

mvdv <- function (vine, margins, marginParams) {
    new("mvdv", vine = vine, margins = margins, marginParams = marginParams)
}


dmvdv <- function (mvdv, x) {
    if (is.vector(x)) x <- matrix(x, nrow = 1)
    dim <- mvdv@vine@dimension
    k <- match(mvdv@margins, c("norm", "kernel", "gamma", "beta", "unif", "userdist"))
    puserdist <- if (exists("puserdist")) puserdist else function (...) NULL
    duserdist <- if (exists("duserdist")) duserdist else function (...) NULL
    p <- c(pnorm, pkernel, pgamma, pbeta, punif, puserdist)[k]
    d <- c(dnorm, dkernel, dgamma, dbeta, dunif, duserdist)[k]
    u <- sapply(seq(length = dim),
        function (i) do.call(p[[i]], c(list(x[,i]), mvdv@marginParams[[i]])))
    if (is.vector(u)) u <- matrix(u, nrow = 1)
    densmarg <- 0
    for (i in 1:dim) {
        densmarg <- densmarg + log(do.call(d[[i]], c(list(x[ , i]), mvdv@marginParams[[i]])))
    }
    evalCopulaLog <- function (vine, j, i, x, y) {
        log(dCopula(cbind(x, y), vine@copulas[[j, i]]))
    }
    iterResult <- vines:::vineIter(mvdv@vine, u, evalCopula = evalCopulaLog)
    return(apply(matrix(unlist(iterResult$evals), nrow(u)), 1, sum) + densmarg)
}


pmvdv <- function (mvdv, x) {
    if (is.vector(x)) x <- matrix(x, nrow = 1)
    dim <- mvdv@vine@dimension
    k <- match(mvdv@margins, c("norm", "kernel", "gamma", "beta", "unif", "userdist"))
    puserdist <- if (exists("puserdist")) puserdist else function (...) NULL
    p <- c(pnorm, pkernel, pgamma, pbeta, punif, puserdist)[k]
    u <- sapply(seq(length = dim),
        function (i) do.call(p[[i]], c(list(x[ , i]), mvdv@marginParams[[i]])))
    if (is.vector(u)) u <- matrix(u, nrow = 1)
    pvine(mvdv@vine, u)
}


rmvdv <- function (mvdv, n) {
    dim <- mvdv@vine@dimension
    u <- rvine(mvdv@vine, n)
    k <- match(mvdv@margins, c("norm", "kernel", "gamma", "beta", "unif", "userdist"))
    quserdist <- if (exists("quserdist")) quserdist else function (...) NULL
    q <- c(qnorm, qkernel, qgamma, qbeta, qunif, quserdist)[k]
    x <- sapply(seq(length = dim),
        function (i) do.call(q[[i]], c(list(u[ , i]), mvdv@marginParams[[i]])))
    return(x)
}
