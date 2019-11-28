
dev<-"screen"
nox<-2; noy<-2;

clean.between<-FALSE
first.year.on.plot<-1970
last.year.on.plot<-2020

include.mean<-TRUE
include.std1<-TRUE     #include 1*std band
include.std2<-TRUE     #include 2*std band

include.obs<-TRUE	#include observations

cleanup()
newplot(dev,nox,noy);
i<-0

if (nsp>1) sp.title<-TRUE else sp.title<-FALSE;

plot_forcast<-function(vari='SSB') {

if (include.obs) {
  a<-Read.MCMC.data()
  a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
}

b<-Read.MCMC.mean.data();
b<-subset(b,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)

#########################################

if (include.obs) {
 if (vari=="SSB") a1<-tapply(a$SSB,list(a$Species.n,a$Repetion,a$Iteration,a$Year),sum)/1000
 if (vari=="Yield") a1<-tapply(a$Yield,list(a$Species.n,a$Repetion,a$Iteration,a$Year),sum)/1000
 if (vari=="Recruit") a1<-tapply(a$recruit,list(a$Species.n,a$Repetion,a$Iteration,a$Year),sum)/1000
 if (vari=="F") a1<-tapply(a$mean.F,list(a$Species.n,a$Repetion,a$Iteration,a$Year),sum)

 repe<-as.numeric(dimnames(a1)[[2]])
 iter<-as.numeric(dimnames(a1)[[3]])
}

if (vari=="SSB") {
  b1<-tapply(b$SSB,list(b$Species.n,b$Year),sum)/1000
  b1.std<-tapply(b$SSB.std,list(b$Species.n,b$Year),sum)/1000
  y.lab<-"1000t"
  titl<-'SSB'
}
if (vari=="Yield") {
  b1<-tapply(b$Yield,list(b$Species.n,b$Year),sum)/1000
  b1.std<-tapply(b$Yield.std,list(b$Species.n,b$Year),sum)/1000
  y.lab<-"000t"
  titl<-'SOP'
}
 print(vari)
if (vari=="Recruit") {
  b1<-tapply(b$recruit,list(b$Species.n,b$Year),sum)/1000000
  b1.std<-tapply(b$recruit.std,list(b$Species.n,b$Year),sum)/1000000
  y.lab<-"10^6"
  titl<-"Recruits"
}
if (vari=="F") {
  b1<-tapply(b$mean.F,list(b$Species.n,b$Year),sum)
  b1.std<-tapply(b$mean.F.std,list(b$Species.n,b$Year),sum)
  y.lab<-" "
  titl<-expression(bar(F))
}
species.n<-dimnames(b1)[[1]]
years<-as.numeric(dimnames(b1)[[2]])
if (clean.between) {
  cleanup()
  newplot(dev,nox,noy);
  i<-0
}


for (sp in species.n) {
  if (i==noxy) {newplot(dev,nox,noy); i<-0 }
  if (sp.title) titl<-paste(name[as.numeric(sp)+1],titl)
  if (include.obs) {
     plot(years,a1[sp,1,1,],main=titl,type='l',ylab=y.lab,xlab='',ylim=(c(0,max(a1[sp,,,]))))
     for (r in repe) for (ii in iter) lines(years,a1[sp,r,ii,],col=ii) 
     i<-i+1
  }
  else if (include.mean) {
     if (include.std2) max.y<-max(b1[sp,]+2*b1.std[sp,]) else if (include.std1) max.y<-max(b1[sp,]+b1.std[sp,]) else max.y<-max(b1[sp,])
     
     plot(years,b1[sp,],main=titl,type='l',ylab=y.lab,xlab='',ylim=c(0,max.y))
     if (include.std1) {
       lines(years,b1[sp,]-b1.std[sp,],col=2,lty=2) 
       lines(years,b1[sp,]+b1.std[sp,],col=2,lty=2) 
     }
     if (include.std2) {
       lines(years,b1[sp,]-2*b1.std[sp,],col=3,lty=3) 
       lines(years,b1[sp,]+2*b1.std[sp,],col=3,lty=3) 
     }
     i<-i+1
  }
}

} 


plot_forcast(vari="Yield")
plot_forcast(vari="F")
plot_forcast(vari="SSB")
plot_forcast(vari="Recruit")

