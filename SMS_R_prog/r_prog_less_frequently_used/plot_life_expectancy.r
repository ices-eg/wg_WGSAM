
first.year<- 1963                #first year on plot, negative value means value defined by data
last.year<- 2100               #last year on plot


first_q_a<-"1_1"    # first quarter and age
#first_q_a<-"3_0"    # first quarter and age
##########################################################################


my.dev<-'screen'   # output device:  'screen', 'wmf', 'png', 'pdf'

cleanup()

dat1<-Read.summary.data(read.init.function=T)
dat<-dat1
dat<-droplevels(subset(dat,Year %in% c(first.year:last.year)  & M2> -1 ,select=c(Species.n, Species, Year,Quarter, Age, Z, F) ))

if (first_q_a=="3_0") {
  dat$first<-(dat$Quarter==3 & dat$Age==0)
  ini.age<-0.5
} 
if (first_q_a=="1_1") {
 dat<-subset(dat,Age>0);
 dat$first<-(dat$Quarter==1 & dat$Age==1)
 ini.age<-1
}

dat<-dat[order(dat$Species.n,dat$Year,dat$Age,dat$Quarter),]
min.Year<-min(dat$Year)

dat$sumZ<-dat$Z

for (x in (1:dim(dat)[1])) {
 if (!dat[x,'first']) dat[x,'sumZ']<-dat[x,'sumZ']+(dat[x-1,'sumZ'])
}
dat$p<-exp(-dat$sumZ)


a<-aggregate(p~Species+Year+Species.n,sum,na.rm=T,data=dat)
a$Expectancy<-a$p/4 + ini.age+0.25/2
a<-merge(a,b)

b<-aggregate(sumZ~Species+Year+Species.n,max,na.rm=T,data=dat)
bb<-aggregate(F~Species+Year+Species.n,sum,na.rm=T,data=dat)
a<-merge(a,bb)

nox<-3; noy<-4;
noxy<-nox*noy



dev<-'screen'
plot_Mage<-function(x,header='Species') {
  cleanup();newplot(dev,nox,noy); i<-0
    par(mar=c(3,4,3,2))
  for (sp in seq(first.VPA,nsp)) {
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
     a<-droplevels(subset(x,sp==Species.n))
     plot(x=a$Year,y=a$Expectancy,main=a[1,header],type='b',xlab='',ylab='Life expectancy (year)')
    i<-i+1
  }
}

plot_Mage(a)


plot_sumZ<-function(x,header='Species',addF=F) {
  cleanup();newplot(dev,nox,noy); i<-0
    par(mar=c(3,4,3,2))
  for (sp in seq(first.VPA,nsp)) {
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
     a<-droplevels(subset(x,sp==Species.n))
     if (addF) ylim<-c(min(a$F),max(a$sumZ)) else ylim<-NULL
     plot(x=a$Year,y=a$sumZ,main=a[1,header],ylim=ylim,type='b',xlab='',ylab='Sum of Z')
     if (addF)  lines(x=a$Year,y=a$F,type='b')
    i<-i+1
  }
}

plot_sumZ(a)
#plot_sumZ(a,addF=T)


plot_sumZF<-function(x,header='Species') {
  cleanup();newplot(dev,nox,noy); i<-0
    par(mar=c(3,4,3,2))
  for (sp in seq(first.VPA,nsp)) {
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
     a<-droplevels(subset(x,sp==Species.n))
     plot(x=a$Year,y=a$F/a$sumZ,main=a[1,header],type='b',xlab='',ylab='Sum of F/ sum of Z')
    i<-i+1
  }
}

plot_sumZF(a)


dev<-'screen'
plot_sumZExpec<-function(x,header='Species') {
  cleanup();newplot(dev,nox,noy); i<-0
    par(mar=c(3,4,3,2))
  for (sp in seq(first.VPA,nsp)) {
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
     a<-droplevels(subset(x,sp==Species.n))
     plot(x=a$Year,y=a$Expectancy/a$sumZ,main=a[1,header],type='b',xlab='',ylab='life expec/Sum of Z')
    i<-i+1
  }
}

plot_sumZExpec(a)
    