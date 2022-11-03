
paper<-F                   # output on paper (=TRUE) or screen (=FALSE)
file.name<-'meanWeights'             # graphical output file if paper<-TRUE
Portrait<-F                 # graphical output orientation

first.year<- 1963                #first year on plot, negative value means value defined by data
last.year<- 2011                 #last year on plot
plotVar<-'weca'                  # plot wsea or weca
plotVar<-'west'                  # plot wsea or weca

##########################################################################

cleanup()
 
if (paper) dev<-"wmf" else dev<-"screen"
nox<-1; noy<-1;
noxy<-nox*noy


dat<-Read.summary.data()

dat<-subset(dat,Year<=last.year,select=c(Year,Quarter,Species.n,Age,west,weca,C.obs))

if (first.year>0) dat<-subset(dat,Year>=first.year )
if (plotVar=="weca") {
  dat$w<-dat$west 
  dat<-subset(dat,Quarter==1,select=c(Year,Species.n,Age,w))
} else {
  dat$CW<-dat$weca*dat$C.obs
  dat<-aggregate(list(cw=dat$CW,C=dat$C.obs),list(Year=dat$Year,Species.n=dat$Species.n,Age=dat$Age),sum)
  dat$w<-dat$cw/dat$C
  dat<-subset(dat,!is.na(w),select=c(Year,Species.n,Age,w))
}

for (sp in (first.VPA:nsp)){
    newplot(dev,nox,noy,filename=paste(file.name,'_',sp.names[sp],sep=''),Portrait=Portrait)
    par(mar=c(3,4,3,2))
    s<-subset(dat,Species.n==sp)
    minA<-min(s$Age)
    aa<-subset(s,Age==minA)
    plot(aa$Year,aa$w,ylim=c(0,max(s$w)),t='b',pch=minA,ylab='Weight (kg)',main=sp.names[sp])
    for (a in ((minA+1):SMS.control@species.info[sp,1])) {
       aa<-subset(s,Age==a)
       lines(aa$Year,aa$w,t='b',pch=a)
    }     
}

