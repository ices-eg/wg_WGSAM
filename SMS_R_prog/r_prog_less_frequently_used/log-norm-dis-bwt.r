n<-100000

cleanup()
par(mfrow=c(2,1))
meanlog<-3
sdlog<-0.8
m<-rlnorm(n, meanlog = meanlog, sdlog = sdlog)
hist(m,freq=F,breaks=100,main=paste("input mean:",round(exp(meanlog),2),"mean:",round(mean(m),2),"median:",round(median(m),3),
               "\n","exp(mu+0.5sd^2):",round(exp(meanlog+0.5*sdlog^2),2)))
abline(v=mean(m),col="blue",lwd=2)
abline(v=median(m),col="red",lwd=2)


m<-rnorm(n, mean =meanlog, sd = sdlog)
m<-exp(m)

hist(m,freq=F,breaks=100,main=paste("input mean:",round(exp(meanlog),2),"mean:",round(mean(m),2),"median:",round(median(m),3),
               "\n","exp(mu+0.5sd^2):",round(exp(meanlog+0.5*sdlog^2),2)))
abline(v=mean(m),col="blue",lwd=2)
abline(v=median(m),col="red",lwd=2)


# the SMS way with truncated variance
X11()
par(mfrow=c(2,1))
#mind<- -100; maxd<- 100
mind<- -2; maxd<- 2
a<-data.frame(rn=rnorm(n*2, mean = 0, sd = 1))
a<-subset(a,rn> mind & rn<maxd)
noise<-a[1:n,'rn']
hist(noise,freq=F,breaks=100)
m<-exp(meanlog)*exp(sdlog^2*noise)
hist(m,freq=F,breaks=100,main=paste("input mean:",round(exp(meanlog),2),"mean:",round(mean(m),2),"median:",round(median(m),3),
               "\n","exp(mu+0.5sd^2):",round(exp(meanlog+0.5*sdlog^2),2)))
abline(v=mean(m),col="blue",lwd=2)
abline(v=median(m),col="red",lwd=2)

#X11()
#sd<-seq(0,1,0.05)
#plot(sd, sd-sqrt(exp(sd^2) - 1))

