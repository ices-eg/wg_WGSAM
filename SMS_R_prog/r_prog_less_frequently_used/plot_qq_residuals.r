#control<-read.FLSMS.control()
#indices<-SMS2FLIndices(control)

##############################################################
# QQplot of catch and surveys

cleanup() 
dev<-'Screen'
nox<-4; noy<-4;

n.other<-2  # BUG
a<-Read.catch.survey.residuals()

# survey residuals by fleet
fleet.names<-Read.fleet.names()

newplot(dev,nox,noy); i<-0
aa<-subset(a,data=='survey')
by(aa,list(aa$fleet,aa$Species.n),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   qqnorm(x$residual,main=' ',col=x$Age+1)
   qqline(x$residual)
   title(main=paste(fleet.names[x[1,]$Species.n-n.other,x[1,]$fleet]))
     i<<-i+1
   hist(x$residual,main=paste(x[1,]$Species,x[1,]$fleet),xlab='Residual')
})



by(a,list(a$Species,a$data),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   qqnorm(x$residual,main=' ')
   qqline(x$residual)
   title(main=paste(x[1,]$Species,x[1,]$data))
     i<<-i+1
   hist(x$residual,main=paste(x[1,]$Species,x[1,]$data),xlab='Residual')

})


by(a,list(a$Species,a$data),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   qqnorm(x$residual,main=' ')
   qqline(x$residual)
   title(main=paste(x[1,]$Species,x[1,]$data))
     i<<-i+1
   hist(x$residual,main=paste(x[1,]$Species,x[1,]$data),xlab='Residual')

})

by(a,list(a$Species),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   qqnorm(x$residual,main=' ',col=x$col)
   qqline(x$residual)
   title(main=paste(x[1,]$Species,' all'))
     i<<-i+1
})

hist(a$residual,main='all')
