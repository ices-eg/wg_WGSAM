
p<-Read.OP.par()

# find max SSB
a<-Read.summary.data()
a<-subset(a,select=c(Species.n,Year,SSB),Quarter==1)
a<-aggregate(SSB~Species.n+Year,data=a,sum)
a<-aggregate(SSB~Species.n,data=a,max)

a<-merge(a,p)

cleanup()
nox<-4; noy<-4;
noxy<-nox*noy

newplot(dev='screen',nox,noy)
par(mar=c(4,4,3,2))  #bottom, left, top, right

for (sp in (first.VPA:nsp)){
 sp.name<-sp.names[sp]
 b<-subset(a,Species.n==sp)
 do.plot<-F
 if (b[1,'HCR']==1) {
   SSB<-c(0,b[1,'SSB'])/1000
   Fi<-rep(b[1,'Ftarget'],2)
   do.plot<-T
 }
 else if (b[1,'HCR']==2) {
   SSB<-c(0,b[1,'T1'],b[1,'T2'],max(b[1,'SSB'],b[1,'T2']))/1000
   Fi<-c(0,0,b[1,'Ftarget'],b[1,'Ftarget'])
   do.plot<-T
 }
 else if (b[1,'HCR']==3) {
   maxSSB<-max(b[1,'SSB'],b[1,'T2'])
   SSB<-c(0,b[1,'T1'],b[1,'T2'],maxSSB)/1000
   Fi<-c(0,0,b[1,'Ftarget'],b[1,'Ftarget']+(maxSSB-b[1,'T2'])/b[1,'T2']*b[1,'Fslope'])
   do.plot<-T
 }
 else if (b[1,'HCR']==4) {
   maxSSB<-max(b[1,'SSB'],b[1,'T2'])
   SSB<-seq(0,maxSSB,maxSSB/100)/1000
   Fi<-b[1,'Ftarget']/(1+exp(b[1,'S1'] - b[1,'S1']/b[1,'SSB50']*SSB))
   do.plot<-T
 }

 if (do.plot) plot(SSB,Fi,xlab='SSB (1000 t)',ylab='F',type='l',lwd=2,main=sp.name)
 if (b[1,'HCR']==4) abline(v=b[1,"SSB50"],lty=2,lwd=2)
}


