library(DirichletReg)

head(ArcticLake)
AL <- DR_data(ArcticLake[, 1:3])
plot(AL, cex = 0.5, a2d = list(colored = FALSE, c.grid = FALSE))


stom<-DR_data(data.frame(Other=0.3,Sprat=0.5,Hering=0.2))
stom
plot(stom, cex = 0.5, a2d = list(colored = F, c.grid = T),cex=2)



lake1 <- DirichReg(AL ~ depth, ArcticLake)
lake1

DirichletReg::residuals.DirichletRegModel

residuals(lake1, "standardized")
object=lake1

residuals2<-function (object, type = c("standardized", "composite", "raw"), ...) 
{
  Y <- object$Y
  fitted.vals <- fitted(object, mu = TRUE, alpha = TRUE, phi = TRUE)
  M <- fitted.vals[["mu"]]
  A <- fitted.vals[["alpha"]]
  f <- fitted.vals[["phi"]]
  raw.res <- Y - M
  type <- match.arg(type)
  wghts <- object$weights
  V <- apply(M, 2, function(m) (m * (1 - m))/(1 + f))
  switch(type, standardized = {
    res <- raw.res/sqrt(V)
  }, composite = {
    res <- rowSums((raw.res/sqrt(V))^2)
  }, raw = {
    res <- raw.res
  })
  return(res)
}

residuals2(lake1, "standardized")
residuals2(lake1, "composite")
residuals2(lake1, "raw")

object=lake1



require(stats)
## Extends the example for 'switch'
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
