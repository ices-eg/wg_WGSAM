# Function to plot

dev<-"screen"
nox<-2; noy<-2;

first.year.on.plot<-2000
last.year.on.plot<-2030
age.incl<-c(1,2,3,4,5)
age.incl<-c(0,1,2)
include.mean<-TRUE      
include.std1<-FALSE      #include 1*std band
include.std2<-FALSE      #include 2*std band

include.obs<-FALSE

plot_forcast<-function(var='M2') {

a<-Read.MCMC.detailed.data()
a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & Age %in% age.incl ) ,drop=T)

#prepare annual data
comb<-list(a$Species.n,a$Repetion,a$Iteration,a$Age,a$Year)
if (var=="N") {a<-subset(a,Quarter==1,,drop=T);
                  comb<-list(a$Species.n,a$Repetion,a$Iteration,a$Age,a$Year)
                 a1<-tapply(a$N,comb,sum)/1000; y.lab<-"N 10^6)";
              }
if (var=="M2"){a1<-tapply(a$M2,comb,sum); y.lab<-"M2"; }
if (var=="F") {a1<-tapply(a$F,comb,sum); y.lab<-"F";}

if (include.mean) {
  b1<-apply(a1,c(1,4,5),mean)
  if (include.std1 || include.std2) b1.std<-apply(a1,c(1,4,5),sd)
}
  
species.n<-dimnames(a1)[[1]]
years<-as.numeric(dimnames(a1)[[5]])
repe<-as.numeric(dimnames(a1)[[2]])
iter<-as.numeric(dimnames(a1)[[3]])
age<-dimnames(a1)[[4]]

cleanup()
for (a in age) {
  newplot(dev,nox,noy);
  i<-0

  for (sp in species.n) {
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
    if (include.obs) {
       plot(years,a1[sp,1,1,a,],main=paste(name[as.numeric(sp)+1],a),type='l',ylab=y.lab,
           xlab='',ylim=(c(0,max(a1[sp,,,,]))))
       for (r in repe) for (ii in iter) lines(years,a1[sp,r,ii,a,],col=ii)
       i<-i+1
    }
    else if (include.mean) {
     if (include.std2) max.y<-max(b1[sp,a,]+2*b1.std[sp,a,]) else if (include.std1) max.y<-max(b1[sp,a,]+b1.std[sp,a,]) else max.y<-max(b1[sp,a,])
     
     plot(years,b1[sp,a,],main=paste(name[as.numeric(sp)+1],a),type='l',ylab=y.lab,xlab='',ylim=c(0,max.y))
     if (include.std1) {
       lines(years,b1[sp,a,]-b1.std[sp,a,],col=2,lty=2)
       lines(years,b1[sp,a,]+b1.std[sp,a,],col=2,lty=2)
     }
     if (include.std2) {
       lines(years,b1[sp,a,]-2*b1.std[sp,a,],col=3,lty=3)
       lines(years,b1[sp,a,]+2*b1.std[sp,a,],col=3,lty=3)
     }
     i<-i+1
    }
  }
}

} 

#plot_forcast(var="M2")
#plot_forcast(var="F")
plot_forcast(var="N")
