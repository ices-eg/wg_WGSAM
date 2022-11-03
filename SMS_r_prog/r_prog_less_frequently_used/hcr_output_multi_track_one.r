#scenario<-'aa'
#my.data.path<-file.path(data.path,scenario)
my.data.path<-data.path  #default
plot.recruit<-T

nox<-3; noy<-2; Portrait<-F
if (!plot.recruit) nox<-2
paper<-FALSE        # graphics on paper=file (TRUE) or on screen (FALSE)
if ("scenario" %in% ls()) run.ID<-scenario else run.ID<-"b"         # file id used for paper output

cleanup()

species.in.title<-T
first.year.on.plot<-1975
last.year.on.plot<-2100

include.assess.forcast.line<-TRUE
include.F.reference.points<-TRUE
include.SSB.reference.points<-TRUE
Use.SSB.rec.param<-"out"               #Read SSB recruitment parameters from in=SSB_R.in or out=SSB_R.out

Reps<-c(1,26)  # Select MCMC repetion
Reps<-seq(1,1)
Iter<-c(1)  # Select Iterations within a MCMC Repetion
Iter<-seq(1,5)  # Select Iterations within a MCMC Repetion

incl.sp<-seq(1,3)  # species number to be included

max.color<-5
incl.sp<-seq(15,21)
incl.sp<-seq(1,3)
incl.sp<-1
incl.sp<-"all"
#sp.plot<-incl.sp

do.plot.forecast1<-function(sp.plot=c(1),incl.prob=FALSE,incl.closure=FALSE,incl.M2=FALSE,plot.residuals=F,dir=my.data.path) {
#########################################
    control<-read.FLSMS.control(file=file.path(dir,'SMS.dat'))
    last.year.model<-control@last.year.model
    HCR<-read.FLSMS.predict.control(control=control,file=file.path(dir,'HCR_options.dat'))

    sp.names<<-control@species.names
    years<-c(control@first.year,control@last.year.model)

    lReps<-length(Reps)
    
    make.assess.forcast.line<-function() if (include.assess.forcast.line) abline(v=last.year.model,col='red',lwd=2)
    
    if (incl.closure) {
      closure <-Read.MCMC.closure.data(dir=dir)
      closure<-subset(closure,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)
    }

    if (incl.M2) {
      M2<-Read.MCMC.detailed.M2(dir=dir,del.zero=F)
      M2<-droplevels(subset(M2,Year>=first.year.on.plot & Year<=last.year.on.plot & M2>0 & Age<=2 & Repetion %in% Reps  & Iteration %in% Iter))
      M2<-tapply(M2$M2,list(M2$Species.n,M2$Year,M2$Age,M2$Repetion,M2$Iteration),sum)
    }
    
   ref.points<-Read.reference.points()

    ssb.r<-Read.MCMC.SSB.rec.data(dir=dir)
    ssb.r<-subset(ssb.r,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
    
    F.yield<-Read.MCMC.F.yield.data(dir=dir)
    F.yield<-subset(F.yield,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)

    if (paper) dev<-"wmf" else dev<-"screen"

    if (is.character(incl.sp)) if (incl.sp=="all") {
       info<-control@species.info
       n.pred<-0; n.oth<-0
       for (s in (1:control@no.species)) {
         if(info[s,'predator']>=1) n.pred<-n.pred+1;
         if(info[s,'predator']==2) n.oth<-n.oth+1;
       }
       first.VPA<-n.oth+1
       n.VPA<-control@no.species-first.VPA+1

       sp.plot<-seq(first.VPA,length(unique(ssb.r$Species))+n.oth)
    }
    
for (sp in (sp.plot)) {
  newplot(dev,nox,noy,filename=paste("trend_",run.ID,'_',sp.names[sp],sep=''),Portrait=Portrait);
  par(mar=c(3,5,3,2)) 
  loc.name<-sp.names

  if (!species.in.title) loc.name[]<-"" 
   
  a<-subset(ssb.r,Species.n==sp & Repetion %in% Reps & Iteration %in% Iter)
  b<-tapply(a$SSB,list(a$Year,a$Repetion ,a$Iteration), sum)/1000
  bSSB<-b*1000
  y<-as.numeric(dimnames(b)[[1]])
  plot(y,b[,1,1],main=paste(loc.name[sp], "SSB"),xlab="",ylab='SSB (1000t)',type='l',lwd=2,ylim=c(0,max(b)))
    for (i in (1:dim(b)[2])) for (j in (1:dim(b)[3])) lines(y,b[,i,j],lty=min(i,max.color),col=min(i,max.color),lwd=2)
  if (include.SSB.reference.points) {
    if (ref.points[sp,3] >0) abline(h=ref.points[sp,3]/1000,lty=2)
    if (ref.points[sp,4] >0) abline(h=ref.points[sp,4]/1000,lty=3)
  }
  make.assess.forcast.line()
   
  a<-subset(ssb.r,Species.n==sp & Repetion %in% Reps  & Iteration %in% Iter)
  b<-tapply(a$rec,list(a$Year,a$ Repetion ,a$Iteration), sum)/1000
  y<-as.numeric(dimnames(b)[[1]])
  plot(y,b[,1,1],main=paste(loc.name[sp], "Recruits"),xlab="",ylab='Recruits (1000)',
        type='l',lwd=2,ylim=c(0,max(b)))
   for (i in (1:dim(b)[2])) for (j in (1:dim(b)[3]))  lines(y,b[,i,j],lty=min(i,max.color),col=min(i,max.color),lwd=2)
  make.assess.forcast.line()

  #read SSB/R parameters
  if (Use.SSB.rec.param=='out') p<-Read.SSB.Rec.data() else p<-Read.SSB.Rec.data.in()

  model.name<-c('Ricker','Bev. & Holt','Geom. mean','Hockey stick')
  spp<-sp-first.VPA+1
  model<-p[spp,'model']
  alfa<-p[spp,'alfa']
  beta<-p[spp,'beta']
  CV<-p[spp,'std']
  info1<-p[spp,'info1']
  info2<-p[spp,'info2']

  ssbR<-function(x) {
    if (model==1 & Use.SSB.rec.param=='in') y<-alfa*x*exp(-beta/control@beta.cor[sp]*x)
    else if (model==1 & Use.SSB.rec.param=='out') y<-alfa*x*exp(-beta*x)
    else if (model==51 | model==52) y<-alfa*x*exp(-beta*x+info1*info2)
    else if (model==2) y<-alfa*x/(1+x*beta)
    else if (model==3) y<-rep(exp(alfa),length(x))
    else if (model==4) {for (i in (1:length(x))) y[i]<-exp(alfa)*min(x[i],exp(beta))}

    else if (model==100) {for (i in (1:length(x))) y[i]<-exp(alfa)*min(x[i],beta)}
    y
  }
 
  if (plot.recruit) {  # recruitment plots
  ssb<-seq(0,max(bSSB),max(bSSB)/100)
  rec<- ssbR(ssb)

  rec<-rec/1000;
  ssb<-ssb/1000
  firstVPA<-control@no.species-length(control@SSB.R.year.first)+1
   spidx<-sp-firstVPA+1
  if (control@SSB.R.year.first[spidx] != -1) fyrc<-control@SSB.R.year.first[spidx] else fyrc<-control@first.year
  if (control@SSB.R.year.last[spidx] != -1) lyrc<-control@SSB.R.year.last[spidx] else lyrc<-control@last.year.model

  a<-subset(ssb.r,Species.n==sp & Repetion %in% Reps & Iteration %in% Iter)
  a$recruit[a$Year<fyrc | (a$Year>lyrc & a$Year<control@last.year.model)]<-NA
  plot(ssb,rec,main="SSB recruit",type='l',ylab='Recruits (1000)',ylim=c(0,max(a$recruit,na.rm=T)/1000))
  fa<-control@first.age
  b<-data.frame(Year=a$Year-control@first.age,recruit=a$recruit,Repetion=a$Repetion,Iteration=a$Iteration)
  c<-data.frame(Year=a$Year,SSB=a$SSB,Repetion=a$Repetion,Iteration=a$Iteration)

  d<-merge(b,c)
  d$recruits[d$Year<fyrc | (d$Year>lyrc & d$Year<control@last.year.model)]<-NA
  d$color<-ifelse(d$Year<=last.year.model,1,3)
  lines(d$SSB/1000,d$recruit/1000,type='p',col=d$color)
  
  
  # residuals
  if (plot.residuals) {
    d<-data.frame(d, pred.rec=ssbR(as.vector(d$SSB)))
    e<-transform(d,resid=log(recruit)-log(pred.rec))
    
    e$resid[e$Year<fyrc | (e$Year>lyrc & e$Year<control@last.year.model)]<-NA
    plot(e$Year,e$resid,type='p',main='Stock recruitment residuals',ylab='log(obs)-log(predicted)',,col=d$color)
    abline(h=0,lty=3)
  }
 }

  if (F) {
    aa<-acf(e$resid,xlab='Lag(years)',main='Autcorrelation',lag.max=5)
    print(loc.name[sp])
    print(aa)

    a<-as.vector(e$resid)
    n<-length(a)
    sum<-0
    for (i in (2:n)) sum<-sum+(a[i]-a[i-1])^2
    d<-sum/sum(a^2)
    # Durbin-Watson d statistics. The traditional test for the presence of first-order autocorrelation
    # Its value always lies between 0 and 4.
    # A value of 2 indicates there appears to be no autocorrelation.
    # If the Durbin-Watson statistic is substantially less than 2, there is evidence of positive serial correlation.
    # As a rough rule of thumb, if Durbin-Watson is less than 1.5, there may be cause for alarm.
    # Small values of d indicate successive error terms are, on average, close in value to one another, or positively correlated.
    # Large values of d indicate successive error terms are, on average, much different in value to one another, or negatively correlated.

    cat(paste("\n# Durbin-Watson d statistics:",round(d,2),"\n"))
  }

  #plot(e$ssb,e$rec)


  a<-subset(F.yield,Species.n==sp & Repetion %in% Reps & Iteration %in% Iter)

  b<-tapply(a$mean.F,list(a$Year,a$ Repetion ,a$Iteration), sum)
  y<-as.numeric(dimnames(b)[[1]])

  plot(y,b[,1,1],main=paste(loc.name[sp], 'Mean F'),xlab="",ylab='Mean F',
        type='l',lwd=2,ylim=c(0,max(b)))
  for (i in (1:dim(b)[2])) for (j in (1:dim(b)[3])) lines(y,b[,i,j],lty=min(i,max.color),col=min(i,max.color),lwd=2)
   make.assess.forcast.line()
  
  b<-tapply(a$Yield,list(a$Year,a$ Repetion ,a$Iteration), sum)/1000
  y<-as.numeric(dimnames(b)[[1]])
  plot(y,b[,1,1],main=paste(loc.name[sp], "Yield"),xlab="",ylab='Yield (1000 t)',
        type='l',lwd=2,ylim=c(0,max(b)))
  for (i in (1:dim(b)[2])) for (j in (1:dim(b)[3])) lines(y,b[,i,j],lty=min(i,max.color),col=min(i,max.color),lwd=2)
   make.assess.forcast.line()


  if (incl.M2) {
      print(dim(M2))
      print(dimnames(M2)) 
      if (as.character(sp) %in% dimnames(M2)[[1]]) {
        b<-M2[as.character(sp),,,,]
        print(dimnames(b))
        
        if (sum(b,na.rm=T)>=0.01) {
          y<-as.numeric(dimnames(b)[[1]])
          plot(y,b[,1,1],main=paste("M2 at age"),xlab="",ylab='M2',
                  type='l',lwd=1.5,ylim=c(0,max(b,na.rm=T)))
          for (a in (2:(dim(b)[2]))) for (i in (2:(dim(b)[3]))) if(max(b[,a,i],na.rm=T)>0.001) lines(y,b[,a,i],lty=a,col=a,lwd=2)
        }
      }
     }
     

  if (incl.closure) {
    a<-subset(closure,Species.n==sp & Repetion %in% Reps  & Iteration %in% Iter)
    b<-tapply(a$closure,list(a$Year,a$ Repetion ,a$Iteration), sum)
   y<-as.numeric(dimnames(b)[[1]])
   
   plot(y,b[,1,1],main=paste(loc.name[sp], "Sub model"),xlab="",ylab='sub model',
        type='s',lwd=2,ylim=c(0,max(b)))
   for (i in (2:dim(b)[2])) for (j in (1:dim(b)[3]))  lines(y,b[,i,j]+i/lReps/3,lty=min(i,max.color),col=min(i,max.color),lwd=2,type='s')
  }

  if (paper) cleanup()

}
  
}

#do.plot.forecast1(sp.plot=incl.sp,incl.prob=TRUE,incl.closure=T,dir="TAC_stable")
#do.plot.forecast1(sp.plot=incl.sp,incl.prob=TRUE,incl.closure=F,dir="constantF")
do.plot.forecast1(sp.plot=incl.sp,incl.prob=TRUE,incl.M2=T)
