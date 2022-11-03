#scenario<-'T1-T2_stoc_rec_assess_run01'
#scenario<-'T1-T2_stoc_rec_assess_run01_0pctConstraints'
#scenario<-'T1-T2_stoc_rec_assess_run01-Icelandic'
#scenario<-'T1-T2_stoc_rec_assess_run01_15pctConstraints'

scenario.dir<-file.path(data.path,scenario)
#scenario.dir<-data.path


my.dev<-'wmf'   # output device:  'screen', 'wmf', 'png', 'pdf'


run.ID<-scenario
species.in.title<-F
first.year.on.plot<-2018      # Year range for data included in the plot
last.year.on.plot<-2027
do.change.from.year.to.year<-T

max.cl<-10         # max closure years
min.yield<-2000   # yield used as minimum yield in the variation plot

logAxis<-FALSE

incl.sp<-seq(1)  # species number to be included
incl.sp<-"all"

new_plot<-T           # start with a blank output screen
nox<-3; noy<-2;

do.plot.forecast2<-function(sp.plot=c(1),dir=data.path,do.closure.plot=FALSE) {
#########################################
control<-read.FLSMS.control(file=file.path(dir,'SMS.dat'))
HCR<-read.FLSMS.predict.control(control=control,file=file.path(dir,'HCR_options.dat'))

ref.points<-Read.reference.points(dir=dir)
ssb.r<-Read.MCMC.SSB.rec.data(dir=dir)
ssb.r<-subset(ssb.r,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)  


F.yield<-Read.MCMC.F.yield.data(dir=dir)
F.yield<-subset(F.yield,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)

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

if (any(HCR@HCR>=20) & any(HCR@HCR<40) & do.closure.plot) {
    cl <-Read.MCMC.closure.data(dir=dir)
    cl1<-tapply(cl$closure,list(cl$Year),mean)
    
    cl2<-subset(cl,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)
    cl2<-mean(cl2$closure)
}

if (do.closure.plot) {

  closure <-Read.MCMC.closure.data(dir=dir)
  cl<-subset(closure,Year>=first.year.on.plot & Year<=last.year.on.plot,drop=T)

  cl$closure[cl$closure==1]<-0
  cl$closure[cl$closure>=2]<-1
  mean.cl<-tapply(cl$closure,list(cl$Species.n),mean)

  a<-by(cl,list(cl$Species.n,cl$Repetion,cl$Iteration),function(x) {
   key<-rank(x$Year)
   x<-x[key,]
   res<-rep(0,max.cl)
   ii<-dim(x)[1]
   closed<-FALSE
   n<-0
   for (i in (1:ii)) {
     if (x[i,"closure"]==1) {
       if (closed) n<-n+1 else {closed<-TRUE; n<-1;}
     } else {
       if (closed) {res[min(n,max.cl)]<-res[min(n,max.cl)]+1; closed<-FALSE; }
     }
   }
   if (closed) res[min(n,max.cl)]<-res[min(n,max.cl)]+1
  res
  })

  a<-matrix(unlist(a),ncol=max.cl,byrow=T)
  closure<-apply(a,2,sum)
}


if (last.year.on.plot>first.year.on.plot) {
   
    # change in SSB from year to year
    a<-tapply(ssb.r$SSB,list(ssb.r$Species.n,ssb.r$Repetion,ssb.r$Iteration,ssb.r$Year),sum)
    SSB.change<-a
    SSB.change[,,,]<-0
    for (i in (2:dim(a)[4])) SSB.change[,,,i]<-a[,,,i]/a[,,,i-1]

    # change in yield from year to year
    a<-tapply(F.yield$Yield,list(F.yield$Species.n,F.yield$Repetion,F.yield$Iteration,F.yield$Year),sum)
    a[a<min.yield]<-min.yield
    yield.change<-a
    yield.change[,,,]<-0
    for (i in (2:dim(a)[4])) yield.change[,,,i]<-a[,,,i]/a[,,,i-1]

    # change in mean F from year to year
    a<-tapply(F.yield$mean.F,list(F.yield$Species.n,F.yield$Repetion,F.yield$Iteration,F.yield$Year),sum)
    F.change<-a
    F.change[,,,]<--1
    for (i in (2:dim(a)[4])) F.change[,,,i]<-a[,,,i]/a[,,,i-1]

}

loc.name<-control@species.names
if (!species.in.title) loc.name[]<-'';

cleanup()
  plotfile<-function(dev='screen',out) {
    print(out)
    if (dev=='screen') X11(width=8, height=8, pointsize=12)
    if (dev=='wmf') win.metafile(filename = paste(out,'.wmf',sep=''), width=6, height=8, pointsize=12)
    if (dev=='png') png(filename = paste(out,'.png',sep=''), width = 1200, height = 1000,units = "px", pointsize = 25, bg = "white")
    if (dev=='pdf')  pdf(file =paste(out,'.wmf',sep=''), width = 8, height = 6,pointsize = 12,onefile=FALSE)
   }


# do the plots

for (sp in (sp.plot)) {
    print(sp)
    #if (new_plot) newplot(dev,noy,nox,filename=paste("Histo_",run.ID,'_',loc.name[sp],sep=''),Portrait=TRUE);
    if (new_plot) plotfile(dev=my.dev,out=paste("Histo_",run.ID,'_',Name,'_',loc.name[sp],sep=''));
    par(mar=c(3,5,3,2))
    par(mfcol=c(nox,noy))

    blim<-ref.points[sp,3]
    bpa<-ref.points[sp,4]


    x.lab<-"SSB (1000 t)"
    par(mar=c(5,4,4,5)+.1)
 
    a<-subset(ssb.r,Species.n==sp) 
    min.y<-min(a$SSB)/1000
    max.y<-max(a$SSB)/1000
    med<-trunc(median(a$SSB/1000))
 
    hist(a$SSB/1000,main=paste(loc.name[sp], "SSB, (median=",med,")"),xlim=c(min.y,max.y),xlab=x.lab,axes=FALSE,ylab='frequency')
    axis(1)
    axis(2)

    par(new=T)  
    x<-sort(a$SSB/1000)
    y<-seq(1,length(x))
    y<-y/length(y)
    plot(x,y,col=1,lwd=2,type='l',axes=FALSE,xlab=x.lab,ylab='')
    axis(side=4)
    mtext(side=4,line=3.0,"probability",cex=0.75)
    abline(h=0.05,col="red")
    par(xaxs="r")

    x.lab<-"Yield (1000 t)"
    par(mar=c(5,4,4,5)+.1)   
    a<-subset(F.yield,Species.n==sp)
    min.y<-min(a$Yield)/1000
    max.y<-max(a$Yield)/1000
    med<-trunc(median(a$Yield/1000))
    print(paste("mean TAC:",mean(a$Yield/1000)))
    hist(a$Yield/1000,main=paste("Yield, (median=",med,")"),xlim=c(min.y,max.y),xlab=x.lab,axes=F,ylab='frequency') #,nclass=50)
    axis(1)
    axis(2)
    #print(sort(a$Yield/1000))
    par(new=T)
    x<-sort(a$Yield/1000)
    y<-seq(1,length(x))
    y<-y/length(y)
    plot(x,y,col=1,lwd=2,type='l',axes=FALSE,xlab=x.lab,ylab='')
    axis(side=4)
    mtext(side=4,line=3.0,"probability",cex=0.75)
    par(xaxs="r")

    
    x.lab<-"mean F"
    par(mar=c(5,4,4,5)+.1)   
    a<-subset(F.yield,Species.n==sp)
    
    min.y<-min(a$mean.F)
    max.y<-max(a$mean.F)
    
    med<-round(median(a$mean.F),digits=2) 
    hist(a$mean.F,main=paste("F (median=",med,")"),axes=F,xlim=c(min.y,max.y),xlab=x.lab,ylab='frequency') #,nclass=50)
    axis(1)
    axis(2)
    par(new=T)
   if (T) {
    x<-sort(a$mean.F)
    y<-seq(1,length(x))
    y<-y/length(y)
    plot(x,y,col=1,lwd=2,type='l',axes=FALSE,xlab=x.lab,ylab='')
    axis(side=4)
    mtext(side=4,line=3.0,"probability",cex=0.75)
    par(xaxs="r")
   }

    a<-subset(ssb.r,Species.n==sp)
    q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
  
    T1<-blim    # snyd
    T2<-bpa
  
    q[q>T2]<-0
    q[q>0]<-1
    q<-apply(q,c(1),sum)/(dim(q)[2]*dim(q)[3])
    
    
 if (F) {
  y<-as.numeric(names(q))
  plot(y,q,main=paste("Probability, SSB below", T2/1000,"&", T1/1000),xlab="",ylab='probability',
        type='l',lwd=2,ylim=c(0,1))
   q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
  q[q>T1]<-0
  q[q>0]<-1
  q<-apply(q,c(1),sum)/(dim(q)[2]*dim(q)[3])
  y<-as.numeric(names(q))
  lines(y,q,lty=2) 
 }    
     
 if (last.year.on.plot>first.year.on.plot & do.change.from.year.to.year ) {
    # Change in SSB
    x.lab<-"factor"
    par(mar=c(5,4,4,5)+.1)   
    a<-SSB.change[sp,,,]
    a<-a[,-1]  # exclude first column =0
    med<-round(median(a),digits=2)
    if (logAxis) a<-log(a)
    h<-hist(a,plot=FALSE)
    hist(a,main=paste("SSB change(median=",med,")"),xlim=c(min(a),max(a)),xlab=x.lab,axes=FALSE,ylab='frequency')
    if (logAxis) axis(1,h$mids, round(exp(h$mids),digits=2)) else axis(1,h$mids, round(h$mids,digits=2))
    axis(2)
    par(new=T)  
    x<-sort(a)
    y<-seq(1,length(x))
    y<-y/length(y)
    plot(x,y,col=1,lwd=2,type='l',axes=FALSE,xlab=x.lab,ylab='')
    axis(side=4)
    mtext(side=4,line=3.0,"probability",cex=0.75)
    par(xaxs="r")
  
    # Change in yield
    x.lab<-"factor"
    par(mar=c(5,4,4,5)+.1)   
    a<-yield.change[sp,,,]
    a<-a[,-1]  # exclude first column =0
    #print(loc.name[sp])
    #print(a[a>1.15])
    
    med<-round(median(a),digits=2) 
     if (logAxis) a<-log(a)
    h<-hist(a,plot=FALSE)
    hist(a,main=paste("Yield change(median=",med,")"),xlim=c(min(a),max(a)),xlab=x.lab,axes=FALSE,ylab='frequency')
    if (logAxis) axis(1,h$mids, round(exp(h$mids),digits=2)) else axis(1,h$mids, round(h$mids,digits=2))
    axis(2)
    par(new=T)  
    x<-sort(a)
    y<-seq(1,length(x))
    y<-y/length(y)
    plot(x,y,col=1,lwd=2,type='l',axes=FALSE,xlab=x.lab,ylab='')
    axis(side=4)
    mtext(side=4,line=3.0,"probability",cex=0.75)
    par(xaxs="r")
    
    # Change in F
    if (!do.closure.plot) {
      x.lab<-"factor"
      par(mar=c(5,4,4,5)+.1)
      a<-F.change[sp,,,]
      a<-a[,-1]  # exclude first column =0

      med<-round(median(a),digits=2)
      if (logAxis) a<-log(a)
      
      h<-hist(a,plot=FALSE)

      hist(a,main=paste("F change(median=",med,")"),xlim=c(min(a),max(a)),xlab=x.lab,axes=FALSE,ylab='frequency')
    if (logAxis) axis(1,h$mids, round(exp(h$mids),digits=2)) else axis(1,h$mids, round(h$mids,digits=2))
      axis(2)
      par(new=T)
      x<-sort(a)
      y<-seq(1,length(x))
      y<-y/length(y)
      plot(x,y,col=1,lwd=2,type='l',axes=FALSE,xlab=x.lab,ylab='')
      axis(side=4)
      mtext(side=4,line=3.0,"probability",cex=0.75)
      par(xaxs="r")
    }
    if (do.closure.plot) {
      x.lab<-"years"
      par(mar=c(5,4,4,5)+.1)
      space<-0.1
      barplot(closure,names=seq(1,max.cl),
      main=paste("Number of years in a closure\nprob. closure:",round(mean.cl[[sp]],2)),
      space=space,xlab=x.lab,axes=FALSE,ylab='frequency',col=0)

      #axis(1)
      axis(2)
      par(new=T)

      x<-seq(1,max.cl)+0.5
      y<-cumsum(closure)/sum(closure)
      plot(x,y,col=1,lwd=2,type='b',axes=FALSE,ylim=c(0,1),xlim=c(1,max.cl*(1+space)),xlab=x.lab,ylab='')
      axis(side=4)
      mtext(side=4,line=3.0,"probability",cex=0.75)
      par(xaxs="r")
    }
    
  }

if (my.dev!='screen') cleanup()

}  
}
 
 #do.plot.forecast2(sp.plot=incl.sp,dir="TAC_stable",do.closure.plot=TRUE)
 do.plot.forecast2(sp.plot=incl.sp,do.closure.plot=FALSE,dir=scenario.dir)

