#############################################################################
# confidence interval of M2  calculated from mean and standard deviations

tmp<-Read.SMS.std()
tst<-subset(tmp,name=='M2_sd2')
summary(tst)

if (SMS.control@no.species==3)  tmp<-subset(tmp,select=c(-species,-prey,-predator))

# plot the result
X11()
plotConfM2<-function(M2age=0) {
  var.name<-c("M2_sd0","M2_sd1","M2_sd2")
  a<-subset(tmp,name %in% var.name & age==M2age)
  a$Species=sp.names[a$species]
  a$minus<-a$value-2*a$std
  a$plus<-a$value+2*a$std
  print(xyplot(minus+ value+plus~year|Species, type='l', ylab='Predation mortality, M2', xlab=NULL,
    lwd=c(2,2.5,2),col=c(4,1,4),lty=c(2,1,2),scales = "free",   data=a))
}

 #plotConfM2(0)
plotConfM2(1)

plotConfM2(2)
