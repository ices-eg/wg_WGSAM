########################
## Histogram of residuals from OP run

SSBparm<-"out"   # read ssb-R parameters from SSB_R.in (="in") or SSB_R.out (="out")
dev<-"screen"
dev<-"png"
Portrait <- T

#scenario.dir<-"C:/users/movi/sms/NS_63-10-Nov-2013/opti_s2b_HCR_2_0_Rec3_penBlim_atAgeW___lim_2"
output.dir<-scenario.dir
op.dir<-scenario.dir

##########################################################################
## init section used by other sections
#


cleanup()
if (nsp==1) { nox<-1; noy<-1; } else { nox<-1; noy<-1; }

noxy<-nox*noy
par(ask=TRUE)



#read SSB/R parameters
if (SSBparm=="in") p<-Read.SSB.Rec.data.in() else p<-Read.SSB.Rec.data()

model.name<-c('Ricker','Bev. & Holt','Geom. mean','Hockey stick')


SSB_R<-function(x) {
    delta<-0.5
    y<-x  # copy structure
    #print(c(alfa,beta,exp(alfa+beta)))
    if (model==1) y<-alfa*x*exp(-beta*x)
    else if (model==51 | model==52) y<-alfa*x*exp(-beta*x+info1*info2)
    else if (model==2) y<-alfa*x/(1+x*beta)
    else if (model==3) y<-rep(exp(alfa),length(x))
    else if (model==4) {for (ii in (1:length(x))) y[i]<-exp(alfa)*min(x[i],exp(beta))}
    else if (model==5) {for (ii in (1:length(x))) {
        if (x[ii]<=(exp(beta)*(1-delta))) y[ii]<-exp(alfa)*x[ii]
        else if (x[ii]< exp(beta)*(1+delta)) y[ii]=exp(alfa)*(x[ii]-(((x[ii]-exp(beta)*(1+delta))^2)/(4*delta*exp(beta))))
        else if (x[ii]>=exp(beta)*(1+delta)) y[ii]<=exp(alfa+beta)
        else x[i]<-NA
        }}
    else if (model==100) {for (ii in (1:length(x))) y[ii]<-exp(alfa)*min(x[ii],beta)}
    return(y)
}

##########################  SSB - Recruit plot Obsevations  #############

 dat<-Read.OP.condensed(dir=op.dir)
 ssb<-tapply(dat$SSB,list(dat$Species.n,dat$Year),sum)/1000
 rec<-tapply(dat$recruit,list(dat$Species.n,dat$Year),sum)/1000

 s.index<-0

if (dev=="print") cleanup()

nox<-3; noy<-4;
noxy<-nox*noy
newplot(dev,filename='SSB_R_OP1',nox,noy,Portrait,dir=op.dir);
i<-0

ss<-as.character(seq(first.VPA,nsp))

for (sp in ss) {
  s.index<-s.index+1
  model<-p[s.index,'model']
  alfa<-p[s.index,'alfa']
  beta<-p[s.index,'beta']
  info1<-p[s.index,'info1']
  info2<-p[s.index,'info2']
  CV<- sqrt(exp(p[s.index,'std']^2) - 1)
  modelName<-model.name[min(model,4)]
  if (model==51 | model==52) modelName<-paste("Ricker, Temp=",info2,sep='')

  if (i==noxy) {newplot(dev,filename=paste('SSB_R_OP1',s.index,sep=''),nox,noy,Portrait,dir=op.dir); i<-0 }
  x<-ssb[sp,]*1000
  y<- 1000*rec[sp,] /SSB_R(x)   # residuals: observed / predicted

  x<-seq(-3,3,by=0.01)
  hist(log(y),freq=F,main=paste(sp.names[as.numeric(sp)],': ',modelName," ",sep=''),xlab='Residual (log(obs/est))')

  SD<-p[s.index,'std']
  curve(dnorm(x,mean=0,sd=SD), col = 2, lty = 2, lwd = 3, add = TRUE)
  i<-i+1
}


## range in noise function
s.index<-0

if (dev=="print") cleanup()

newplot(dev,filename='SSB_R_OP2',nox,noy,Portrait,dir=op.dir);
i<-0

for (sp in ss) {
  s.index<-s.index+1
  model<-p[s.index,'model']
  alfa<-p[s.index,'alfa']
  beta<-p[s.index,'beta']
  info1<-p[s.index,'info1']
  info2<-p[s.index,'info2']
  CV<-p[s.index,'std']
  modelName<-model.name[min(model,4)]
  if (model==51 | model==52) modelName<-paste("Ricker, Temp=",info2,sep='')
  if (i==noxy) {newplot(dev,filename=paste('SSB_R_OP2',s.index,sep=''),nox,noy,Portrait,dir=op.dir);; i<-0 }
  x<-ssb[sp,]*1000
  y<- 1000*rec[sp,] /SSB_R(x)   # residuals: observed / predicted

  x<-seq(-3,3,by=0.01)
  hist(log(y)/CV,freq=F,main=paste(sp.names[as.numeric(sp)],': ',modelName," ",sep=''),xlab='Noise')

  CV<-p[s.index,'std']
  curve(dnorm(x,mean=0,sd=1), col = 2, lty = 2, lwd = 3, add = TRUE)
  i<-i+1
}
if (dev=='png') cleanup()


