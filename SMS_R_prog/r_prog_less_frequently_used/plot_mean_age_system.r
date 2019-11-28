
first.year<- 1963                #first year on plot, negative value means value defined by data
last.year<- 2100               #last year on plot

##########################################################################


my.dev<-'screen'   # output device:  'screen', 'wmf', 'png', 'pdf'

cleanup()

#dat1<-Read.summary.data(read.init.function=F)
dat<-dat1
dat<-droplevels(subset(dat,Year %in% c(first.year:last.year) & Quarter==1  & M2> -1))
dat$Nage<-dat$N*dat$Age
dat$Nwest<-dat$N*dat$west

dat<-aggregate(cbind(N,Nwest,Nage)~Species+Year+Species.n,sum,na.rm=T,data=dat)
dat$Mwest<-dat$Nwest/dat$N
dat$Mage<-dat$Nage/dat$N

nox<-4; noy<-3;
noxy<-nox*noy



dev<-'screen'
plot_Mage<-function(x,header='Species') {
  cleanup();newplot(dev,nox,noy); i<-0
  for (sp in seq(first.VPA,nsp)) {
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
     a<-droplevels(subset(x,sp==Species.n))
     plot(x=a$Year,y=a$Mage,main=a[1,header],xlab=NULL,ylab='Mean age')
    i<-i+1
  }
}

plot_Mage(dat)

by(dat,dat$Species.n,

dat$group1<-'all'
dat[dat$Species %in% c('Cod','Whiting','Haddock','Saithe'),'group1']<-'large gadoids'
dat[dat$Species %in% c('Plaice','Sole'),'group1']<-'flatfish'
dat[dat$Species %in% c('Sandeel','Nor. pout','Sprat'),'group1']<-'industrial'

dat<-aggregate(cbind(N,Nwest,Nage)~Year+group1,sum,na.rm=T,data=dat)
dat$Mwest<-dat$Nwest/dat$N
dat$Mage<-dat$Nage/dat$N

plot_Mage(dat,header='group1')

  