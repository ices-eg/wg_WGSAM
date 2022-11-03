sd<-1
x<-rlnorm(10000,sd=sd,meanlog=2)
hist(x)
abline(v=median(x),col='blue')
abline(v=mean(x),col='red')

median(x)
mean(x)
exp((sd^2)/2)*median(x)


