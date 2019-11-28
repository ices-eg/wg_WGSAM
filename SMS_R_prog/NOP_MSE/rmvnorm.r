library(mvtnorm)


rmvnorm<- function (n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)), 
          method = c("eigen", "svd", "chol"), pre0.9_9994 = FALSE,seed=NULL) 
{
  if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps), 
                   check.attributes = FALSE)) {
    stop("sigma must be a symmetric matrix")
  }
  if (length(mean) != nrow(sigma)) 
    stop("mean and sigma have non-conforming size")
  method <- match.arg(method)
  R <- if (method == "eigen") {
    ev <- eigen(sigma, symmetric = TRUE)
    if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))) {
      warning("sigma is numerically not positive semidefinite")
    }
    t(ev$vectors %*% (t(ev$vectors) * sqrt(pmax(ev$values, 
                                                0))))
  }
  else if (method == "svd") {
    s. <- svd(sigma)
    if (!all(s.$d >= -sqrt(.Machine$double.eps) * abs(s.$d[1]))) {
      warning("sigma is numerically not positive semidefinite")
    }
    t(s.$v %*% (t(s.$u) * sqrt(pmax(s.$d, 0))))
  }
  else if (method == "chol") {
    R <- chol(sigma, pivot = TRUE)
    R[, order(attr(R, "pivot"))]
  }
  if (is.null(seed)) set.seed(seed)
  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = !pre0.9_9994) %*% R
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}

n=100000
use<-dim(COV)[[1]]
# use=11
a1<-rmvnorm(n=n,mean=est[1:use],sigma=COV[1:use,1:use],method="eigen",seed=1)
a2<-rmvnorm(n=n,mean=est[1:use],sigma=COV[1:use,1:use],method="svd",seed=1)
a3<-rmvnorm(n=n,mean=est[1:use],sigma=COV[1:use,1:use],method="chol",seed=1)
round(cov(a1),5)
round(cov(a2),5)
round(cov(a3),5)
round(cov(a1)/cov(a2),5) # no diffrence
round(cov(a1)/cov(a3),5) # small diffrence
