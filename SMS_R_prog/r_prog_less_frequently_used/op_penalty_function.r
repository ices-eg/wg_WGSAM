Blim<-25

penal<-function(Blim=25,newP=T,xlim=c(1,100),use.log=F){
  #S1<-L50*log(3.0)/(L75-L50)
  
  L50<-Blim*0.7
  L75<-L50*1.1
  penalty_S1<-L50*log(3.0)/(L75-L50)
  
  SSB<-seq(1,3*Blim,1)
  penalty_factor<-1
  
  penalty_limit<-L50
  penal<- penalty_factor*(1.0-(1.0/(1+exp(penalty_S1-penalty_S1/penalty_limit*SSB))))

  if (use.log) {
    SSB<-log(SSB)
    xlim<-log(xlim)
    Blim<-log(Blim)
  }
  if (newP) plot(SSB,penal,ylab='Penalty',type='l',xlim=xlim,col=cols,lwd=2) else lines(SSB,penal,col=cols,lwd=2)
  abline(v=Blim,lty=2,col=cols,lwd=2)
}
cleanup()
cols<-1
penal(Blim=100,newP=T,xlim=c(0,1200))
for (x in seq(100,1000,200)) {cols<-cols+1; penal(Blim=x,newP=F)}
