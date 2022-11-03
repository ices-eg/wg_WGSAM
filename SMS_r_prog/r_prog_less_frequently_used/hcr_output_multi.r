#scenario<-'T1-T2_stoc_rec_assess_run01'
#scenario<-'T1-T2_stoc_rec_assess_run01_0pctConstraints'
scenario.dir<-file.path(data.path,scenario)
scenario.dir<-data.path


my.dev<-'png'   # output device:  'screen', 'wmf', 'png', 'pdf'
nox<-2; noy<-2;

if ("scenario" %in% ls()) run.ID<-scenario else run.ID<-"ass_uncer"         # file id used for paper output
cleanup()


species.in.title<-T
first.year.on.plot<-2000
#first.year.on.plot<-2008
last.year.on.plot<-2050

#burn.in.delete<-5
include.assess.forcast.line<-T
include.F.reference.points<-T
include.SSB.reference.points<-TRUE

#percentile<-c(0.50,0.25,0.75,0.025,0.975)   #first value must be 50 and last value the highest
#percentile<-c(0.50,0.25,0.75)   #first value must be 50 and last value the highest
percentile<-c(0.50,0.25,0.75,0.05,0.95)   #first value must be 50 and last value the highest
#percentile<-c(0.50)   #first value must be 50 and last value the highest

incl.sp<-c(1,2,3)  # species number to be included
incl.sp<-"all"

my.lwd.median<-2.5
my.lwd.uncertainty<-2


cleanup()
plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=8, height=8, pointsize=12)
  if (dev=='wmf') win.metafile(filename = file.path(scenario.dir,paste(out,'.wmf',sep='')), width=8, height=10, pointsize=12)
  if (dev=='png') png(filename =file.path(scenario.dir,paste(out,'.png',sep='')), width = 1200, height = 1600,units = "px", pointsize = 30, bg = "white")
  if (dev=='pdf') pdf(file =file.path(scenario.dir,paste(out,'.pdf',sep='')), width = 8, height = 10,pointsize = 12,onefile=FALSE)
}


do.plot.forecast1<-function(sp.plot=c(1),incl.prob=FALSE,incl.closure=FALSE,incl.closure2=FALSE,
                             incl.constraints=TRUE,incl.0.group.M2=FALSE,incl.eaten=T,
                            plot.yield=T,myDir=data.path,minus.year=0,incl.percieved.F=F) {
#########################################
    control<-read.FLSMS.control(file=file.path(myDir,'SMS.dat'))
    sp.names<<-control@species.names
    HCR<-read.FLSMS.predict.control(control=control,file=file.path(myDir,'HCR_options.dat'))
    years<-c(control@first.year,control@last.year.model)
    ssb.r<-Read.MCMC.SSB.rec.data(dir=myDir)
    ssb.r<-subset(ssb.r,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
    
    F.yield<-Read.MCMC.F.yield.data(dir=myDir)
    print('F.yield is done')
    F.yield<-subset(F.yield,Year>=first.year.on.plot & Year<=last.year.on.plot-minus.year,drop=T)  
    if (HCR@age.group.output[1,"M2"] >0) {
      eaten.M2<-Read.MCMC.eaten_M2(dir=myDir)
      print('Read.MCMC.eaten_M2 is done')
      eaten.M2<-subset(eaten.M2,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
      ms.incl<-sum(eaten.M2$eaten.M2)>1
      #ms.incl<-F
    } else ms.incl<-F;
    if (include.F.reference.points | include.SSB.reference.points) ref.points<-Read.reference.points()
    
    if (incl.closure) {
      closure <-Read.MCMC.closure.data(dir=myDir)
      print('Read.MCMC.closure.data is done')
      closure<-subset(closure,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)
    }
    
    if (incl.closure2 | incl.constraints) {
      closure.constraint <-Read.MCMC.closure.constraints.data(dir=myDir)
      closure.constraint<-subset(closure.constraint,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)
    }
   
    if (ms.incl) {
      M2<-Read.MCMC.detailed.M2(dir=myDir)
      print('Read.MCMC.detailed.M2 is done')
      M2<-subset(M2,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
    }

    len.per<-length(percentile)

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

  plotfile(dev=my.dev,out=file.path(paste(run.ID,'_',sp.names[sp],sep='')));
  par(mar=c(3,5,3,2)) 
  par(mfcol=c(nox,noy))
  loc.name<-sp.names[sp]

  if (!species.in.title) loc.name[]<-"" 
   
  a<-subset(ssb.r,Species.n==sp)
  b<-tapply(a$SSB,list(a$Year), function(x) quantile(x,probs = percentile))
  res.ssb<-b
  
  y<-as.numeric(names(b))
  q<-matrix(unlist(b),nrow=len.per)/1000
  plot(y,q[1,],main=paste(loc.name, "SSB"),xlab="",ylab='SSB (1000t)',
        type='l',lwd=my.lwd.median,ylim=c(0,max(q[len.per,])))
  for (i in (1:len.per)) lines(y,q[i,],lty=1+i%/%2,col=1+i%/%2,lwd=my.lwd.uncertainty)

  if (include.assess.forcast.line) abline(v=years[2])
  if (include.SSB.reference.points) {
    if (ref.points[sp,3] >0) abline(h=ref.points[sp,3]/1000,lty=2)
    if (ref.points[sp,4] >0) abline(h=ref.points[sp,4]/1000,lty=3)
  }

  if (incl.prob & ref.points[sp,3] >0 & ref.points[sp,4] >0) {
    a<-subset(ssb.r,Species.n==sp)
    q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
    #all.res<-cbind(all.res,q)
    T1<-ref.points[sp,3]
    T2<-ref.points[sp,4]
    
    q[q>T2]<-0
    q[q>0]<-1
    q<-apply(q,c(1),sum)/(dim(q)[2]*dim(q)[3])

    y<-as.numeric(names(q))
    plot(y,q,main=paste("Prob., SSB below", T2/1000,"&", T1/1000),xlab="",ylab='probability',
            type='l',lwd=my.lwd.median,ylim=c(0,0.2))
    q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
    q[q>T1]<-0
    q[q>0]<-1
    q<-apply(q,c(1),sum)/(dim(q)[2]*dim(q)[3])

    y<-as.numeric(names(q))
    lines(y,q,lty=2)
    abline(h=0.05,lty=3)
    if (include.assess.forcast.line) abline(v=years[2])
  } 

  a<-subset(F.yield,Species.n==sp)
  b<-tapply(a$mean.F,list(a$Year), function(x) quantile(x,probs =percentile,na.rm=TRUE))
  res.F<-b

  y<-as.numeric(names(b))
  q<-matrix(unlist(b),nrow=len.per)
  plot(y,q[1,],main=paste("Mean F"),xlab="",ylab='',
        type='l',lwd=my.lwd.median,ylim=c(0,max(q[len.per,])))
  for (i in (1:len.per)) lines(y,q[i,],lty=1+i%/%2,col=1+i%/%2,lwd=my.lwd.uncertainty)
  if (include.assess.forcast.line) abline(v=years[2])
  if (include.F.reference.points) {
    if (ref.points[sp,1] >0) abline(h=ref.points[sp,1],lty=2)
    if (ref.points[sp,2] >0) abline(h=ref.points[sp,2],lty=3)
  }
  if (incl.percieved.F) {
    b<-tapply(a$mean_F_percieved,list(a$Year), function(x) quantile(x,probs =0.50,na.rm=TRUE))
    lines(y,b,lty=1,col=1,lwd=3)
  }

  #a<-subset(F.yield,Species.n==sp)
  b<-tapply(a$Yield,list(a$Year), function(x) quantile(x,probs =percentile,na.rm=TRUE))
  res.yield<-b
  y<-as.numeric(names(b))
  q<-matrix(unlist(b),nrow=len.per)/1000
  if (plot.yield) {
  plot(y,q[1,],main=paste("Yield"),xlab="",ylab='Yield (1000 t)',
        type='l',lwd=my.lwd.median,ylim=c(0,max(q[len.per,])))
  for (i in (1:len.per)) lines(y,q[i,],lty=1+i%/%2,col=1+i%/%2,lwd=my.lwd.uncertainty)
  if (include.assess.forcast.line) abline(v=years[2])
  #grid()
  }

  a<-subset(ssb.r,Species.n==sp & Year>=first.year.on.plot & Year<=last.year.on.plot-minus.year)
  b<-tapply(a$rec,list(a$Year), function(x) quantile(x,probs = percentile,na.rm=TRUE))
  res.recruit<-b
  y<-as.numeric(names(b))
  q<-matrix(unlist(b),nrow=len.per)/1000
  plot(y,q[1,],main=paste("Recruits"),xlab="",ylab='Recruits (millions)',
        type='l',lwd=my.lwd.median,ylim=c(0,max(q[len.per,])))
  for (i in (1:len.per)) lines(y,q[i,],lty=1+i%/%2,col=1+i%/%2,lwd=my.lwd.uncertainty)
  if (include.assess.forcast.line) abline(v=years[2])


 if (ms.incl) {
  a<-subset(eaten.M2,Species.n==sp) 
  b<-tapply(a$eaten.M2,list(a$Year), function(x) quantile(x,probs =percentile,na.rm=TRUE))
  y<-as.numeric(names(b))
 
  q<-matrix(unlist(b),nrow=len.per)/1000
  if (sum(q[1,])>=0.1 & incl.eaten) {
    plot(y,q[1,],main=paste("Eaten biomass"),xlab="",ylab='Eaten biomass(1000 t)',
            type='l',lwd=my.lwd.median,ylim=c(0,max(q[len.per,])))
    for (i in (1:len.per)) lines(y,q[i,],lty=1+i%/%2,col=1+i%/%2,lwd=my.lwd.uncertainty)
    if (include.assess.forcast.line) abline(v=years[2])
  }
  
  a<-subset(M2,Species.n==sp)

  if (incl.0.group.M2) {
    a1<-subset(a,Age<=1)
    b<-tapply(a1$M2,list(a1$Year,a1$Age,a1$Repetion,a1$Iteration),sum)
    b1<-apply(b,c(1,2), function(x) quantile(x,probs =percentile,na.rm=TRUE))

    if (sum(b1)>=0.1) {
        y<-as.numeric(dimnames(b1)[[2]])
        plot(y,b1[1,,1],main=paste("M2 at age 0"),xlab="",ylab='M2',
                type='l',lwd=my.lwd.median,col=1,ylim=c(0,max(b1)))
        for (i in (1:len.per)) lines(y,b1[i,,1],lty=1+i%/%2,col=1+i%/%2,lwd=my.lwd.uncertainty)
        if (include.assess.forcast.line) abline(v=years[2])
        
        if (F) {
          plot(y,b1[1,,2],main=paste("M2 at age 1"),xlab="",ylab='M2',
            type='l',lwd=my.lwd.median,col=1,ylim=c(0,max(b1[,,2])))
          for (i in (1:len.per)) lines(y,b1[i,,2],lty=1+i%/%2,col=1+i%/%2,lwd=my.lwd.uncertainty)
          if (include.assess.forcast.line) abline(v=years[2])
        }

    }
    

  }

  if (T) {
    b<-tapply(a$M2,list(a$Year,a$Age,a$Repetion,a$Iteration),sum)
    b<-apply(b,c(1,2),median)

    if (sum(b)>=0.1) {
      y<-as.numeric(dimnames(b)[[1]])
      plot(y,b[,1],main=paste("median M2 at age"),xlab="",ylab='M2',
              type='l',lwd=my.lwd.uncertainty,ylim=c(0,max(b)))
      for (a in (2:(dim(b)[2]))) if(max(b[,a]>0.001)) lines(y,b[,a],lty=a,col=a,lwd=my.lwd.uncertainty)
      if (include.assess.forcast.line) abline(v=years[2])
    }
  }
 }  #ms.incl
 
  if (incl.closure) {
    a<-subset(closure,Species.n==sp)
    max.closure<-max(a$closure)
    if (max.closure<=1) {
       b<-tapply(a$closure,list(a$Year), mean)
       y<-as.numeric(names(b))
      plot(y,b,main=paste("Probability of closure"),
            xlab="",ylab='probability',
            type='s',lwd=my.lwd.median,ylim=c(0,max(b)),xlim=c(first.year.on.plot,max(y)))
      if (include.assess.forcast.line) abline(v=years[2])
    } else {
      a<-data.frame(Year=a$Year,closure=a$closure,closure1=a$closure%/%2,closure2=a$closure%%2)
      b<-tapply(a$closure1,list(a$Year), mean)
      y<-as.numeric(names(b))
      plot(y,b,main=paste("Probability of closure"),
            xlab="",ylab='probability',
            type='s',lwd=my.lwd.median,ylim=c(0,max(b)),xlim=c(first.year.on.plot,max(y)))
     b<-tapply(a$closure2,list(a$Year), mean)
     lines(y,b,type='s',lwd=1.5,col=2,lty=4)
     if (include.assess.forcast.line) abline(v=years[2])
    }
  }
   colors=c("green","yellow","red")
   if (incl.closure2) {
       a<-subset(closure.constraint,Species.n==sp & closure>0)
       lab<-data.frame(closure=c(1,2,3),
             label=c("a) F<Fmsy & SSB>Bpa","b) Fmsy<F<Fpa & SSB>Bpa","c) F>Fpa |SSB<Bpa"))

       a<-merge(a,lab)
       b<-tapply(a$closure,list(a$label,a$Year), length)
       b[is.na(b)]<-0
       #print(b)
       #barplot(b,main="TAC Decision Rule",ylab='frequency')
       c<-b/apply(b,2,sum)
               barplot(c,main="TAC Decision Rule",ylab='probability',col=colors)
               legend(,x="left", legend=rownames(b),fill=colors,bg="white")
    }

   colors=c(0,2,3)
   if (incl.constraints) {
      a<-subset(closure.constraint,Species.n==sp & closure>0)

       b<-tapply(a$constraints,list(a$constraint,a$Year), length)
       combi<-data.frame(constraints=unique(a$constraints))
       lab<-data.frame(constraints=c(1,3,5),label=c('no constraints','lower','upper'))
       lab<-merge(lab,combi)
       lab<-(lab$label)
       b[is.na(b)]<-0
       c<-b/apply(b,2,sum)
               barplot(c,main="TAC constraints",ylab='probability',col=colors)
               legend(,x="left", legend=lab,fill=colors,bg="white")

    }
  all.res.glo<<-list(SSB=res.ssb,FI=res.F,Recruit=res.recruit,Yield=res.yield)
  if (my.dev!='screen') cleanup()

} #sp
  
}


do.plot.forecast1(sp.plot=incl.sp,incl.prob=F,incl.closure=F,incl.closure2=F,incl.constraints=F, incl.eaten=T,
               incl.0.group.M2=F,plot.yield=T,minus.year=1,myDir=scenario.dir)

