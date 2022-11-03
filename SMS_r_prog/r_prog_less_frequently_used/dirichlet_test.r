library(DirichletReg)


x<-c(0.1,1,2,3,4,5,10)
x<-x/sum(x)

n<-10
pSum<-17
alfa<-x*pSum
xx<-rdirichlet(n,alfa)

round(x,3)
round(xx,3)
apply(xx,1,sum)

ddirichlet(xx, x*pSum, log = T, sum.up = FALSE)
ddirichlet(xx, x*pSum, log = T, sum.up = T)

one<-xx[1,]
sum(one)

logSumP<-lgamma(sum(alfa))
loggamma<-sum(lgamma(one))
pPowStom<-sum((alfa-1)*log(one))

logLike<-logSumP-loggamma+pPowStom

logLike

-logLike