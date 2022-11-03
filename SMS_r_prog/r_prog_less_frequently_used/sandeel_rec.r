 dat<-Read.summary.data()
 dat<-droplevels(subset(dat,Species=='Sandeel'& Year>=1975))
 ssb<-subset(dat,Quarter==1)
 bio<-aggregate(list(TSB=ssb$BIO/1000,SSB=ssb$SSB/1000),list(Year=ssb$Year),sum)

 rec<-subset(dat,Quarter==3 & Age==0, select=c(Year,N))
 a<-merge(bio,rec)
 a$N<-a$N/1000000
cleanup()
X11()
 par(mfcol=c(2,2))
plot(a$Year,a$N,type='b')

plot(a$SSB,a$N,type='p')
lines(smooth.spline(a$SSB,a$N,df=4),col='red')

plot(a$TSB,a$N,type='p')
lines(smooth.spline(a$TSB,a$N,df=4),col='red')

plot(a$TSB-a$SSB,a$N,type='p')
lines(smooth.spline(a$TSB-a$SSB,a$N,df=4),col='red')

a$age1<-a$TSB-a$SSB
library(mgcv)

X11(); par(mfcol=c(2,2))
aa<-gam(N~s(SSB),data=a)
summary(aa)
plot(aa)
plot(a$N,predict.gam(aa))
abline(a=0,b=1)

X11(); par(mfcol=c(2,2))
bb<-gam(N~s(SSB)+s(age1),data=a)
summary(bb)
plot(bb)

anova(aa,bb,test='F')

X11(); par(mfcol=c(2,2))
plot(a$N,predict.gam(bb))
abline(a=0,b=1)
