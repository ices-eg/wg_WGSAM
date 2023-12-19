library (Compositional)
?Compositional::diri.est


x <- rdiri( 10000, c(5, 7, 1, 3, 10, 2, 4) )
a<-diri.est(x)
a
sum(a$param) #phi
a$param/sum(a$param)

b<-diri.est(x, type = "prec")
b


diriEst<-function (x,iterlim = 1000,type = "mle" )  {
    n <- dim(x)[1]
    z <- log(x)
    loglik <- function(param, z, n) {
      param <- exp(param)
      -n*lgamma(sum(param))+n*sum(lgamma(param))-sum(z %*% (param - 1))
    }
    diriphi <- function(param, z, n) {
      phi <- exp(param[1])
      b <- c(1, exp(param[-1]))
      b <- b/sum(b)
      -n * lgamma(phi)+n*sum(lgamma(phi*b))-sum(z %*%(phi * b - 1))
    }
    if (type == "mle") {
      suppressWarnings({
        da <- nlm(loglik, Rfast::colmeans(x) * 10, z = z,
                  n = n, iterlim =  iterlim)
        da <- optim(da$estimate, loglik, z = z, n = n, control = list(maxit = 5000),
                    hessian = TRUE)
      })
      result <- list(loglik = -da$value, param = exp(da$par),
                     std = sqrt(diag(solve(da$hessian))))
    }
    if (type == "prec") {
      suppressWarnings({
        da <- nlm(diriphi, c(10, Rfast::colmeans(x)[-1]),
                  z = z, n = n, iterlim = iterlim)
        da <- optim(da$estimate, diriphi, z = z, n = n, control = list(maxit = 5000),
                    hessian = TRUE)
      })
      phi <- exp(da$par[1])
      a <- c(1, exp(da$par[-1]))
      a <- a/sum(a)
      result <- list(loglik = -da$value, phi = phi, mu = a,
                     param = phi * a)
    }
    return(result)
}


diriEst(x,type = "mle")
diriEst(x,type = "prec")

