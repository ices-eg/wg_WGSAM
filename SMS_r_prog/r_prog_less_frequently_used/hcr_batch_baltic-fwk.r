# user options
nox<-2; noy<-3;
paper<-F   # if paper==T output on file, else screen
cleanup()

#SMS.option<-"-ind run_ms3.dat"
 SMS.option<-' '
 
 
first.year.in.mean<-2015    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2015

do.simulation<-F     # do the simulations and create data files for plots
do.plots<-F            # do the plots
do.plots2<-T            # do the plots

 include.probability<-T
######### end user options #########
do.HCR.batch<-function() {

#Read refence points from file reference_points.in
ref<-Read.reference.points()
blim<-ref[,"Blim"]
bpa<-ref[,"Bpa"]
T1<-blim
T2<-bpa

#  files for output
ssb.out<-'HCR_SSB.dat'
yield.out<-'HCR_yield.dat'
F.out<-'HCR_F.dat'
prob.out<-'HCR_prob.dat'

if (do.simulation) {

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat.gem')

  sp.name<-control@species.names
 
  percentiles<-c(0.025,0.05,0.10,0.25,0.50,0.75,0.95,0.975)

  # headers in output files
  cat(paste('codF cluF Species.n '),file=ssb.out)
  cat(paste("SSB",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=ssb.out,append=T); cat('\n',file=ssb.out,append=T) 
  cat(paste('codF cluF Species.n '),file=yield.out)
  cat(paste("y",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=yield.out,append=T); cat('\n',file=yield.out,append=T) 
  cat(paste('codF cluF Species.n '),file=F.out)
  cat(paste("F",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=F.out,append=T); cat('\n',file=F.out,append=T)    
  cat('codF cluF Species.n p.T1 p.T2 \n',file=prob.out)


  iter<-0
  
  HCR@no.MCMC.iterations<-20   #no of iterations
  HCR@last.prediction.year<-2016 
  BaseF<-HCR@constant.F
  print (BaseF)
  codF<-seq(0.3,1.0,0.05)
  ClupoidF<-1
  
  for (CODF in (codF)) for (CLUF in (ClupoidF)) {
      iter<-iter+1
      
      HCR@constant.F[1]<-CODF
 
      #HCR@constant.F[1]<-BaseF[1]*CODF
      #HCR@constant.F[2]<-BaseF[2]*CLUF*0.5
      #HCR@constant.F[3]<-BaseF[3]*CLUF
       
      write.FLSMS.predict.control(HCR,SMS=control,file='HCR_options.dat')

      #run SMS
      shell(paste( file.path(root,"program","sms.exe"),"-mceval",SMS.option,sep=" "), invisible = TRUE)

      a<-Read.MCMC.SSB.rec.data()
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)

      b<-tapply(a$SSB,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(CODF,' ',CLUF,' ',i,' '),file=ssb.out,append=TRUE)
         cat(b[[i]],file=ssb.out,append=TRUE)
         cat('\n',file=ssb.out,append=TRUE)
      }
     
      dummy<-by(a,list(a$Species.n),function(x) {
        q<-tapply(x$SSB,list(x$Year,x$Repetion,x$Iteration),sum)
        sp<-x[1,"Species.n"]
        q[q>T2[sp]]<-0
        q[q>0]<-1
        p.T2<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])

        q<-tapply(x$SSB,list(x$Year,x$Repetion,x$Iteration),sum)      
        q[q>T1[sp]]<-0
        q[q>0]<-1
        p.T1<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])
        cat(paste(CODF,' ',CLUF,sp,p.T1,p.T2,'\n'),file=prob.out,append=TRUE)
      })

      a<-Read.MCMC.F.yield.data(dir=data.path)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      b<-tapply(a$Yield,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(CODF,' ',CLUF,' ',i,' '),file=yield.out,append=TRUE)
         cat(b[[i]],file=yield.out,append=TRUE)
         cat('\n',file=yield.out,append=TRUE)
      }

      b<-tapply(a$mean.F,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(CODF,' ',CLUF,' ',i,' '),file=F.out,append=TRUE)
         cat(b[[i]],file=F.out,append=TRUE)
         cat('\n',file=F.out,append=TRUE)
      }
      
 
  }
}   # end do.simulations

if (do.plots) {

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names

  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)

  a<-merge(ssb,yield)
  a<-merge(a,fi)
  b<-subset(a,select=c(Species.n,codF,cluF,SSB500,y500))

  make.plots<-function(variable='SSB') {
  i<<-0
  by(b,list(b$Species.n),function(x) {
    xx<-sort(unique(x$codF))
    y<-sort(unique(x$cluF))
    if (variable=="SSB") z<-tapply(x$SSB500/1000,list(x$codF,x$cluF),mean)
    if (variable=="Yield") z<-tapply(x$y500/1000,list(x$codF,x$cluF),mean)

    sp<-sp.name[x[1,"Species.n"]]
     if ((i %% (nox*noy))==0) {
       if (paper) dev<-"wmf" else dev<-"screen"
       newplot(dev,nox,noy,dir=data.path,filename=paste("Batch_",sep=''),Portrait=T);
       par(mar=c(4,5,3,2))
     }
    i<<-i+1
    contour(xx,y,z,xlab="Cod F",ylab="Clupeid F",nlevels = 10)
    title(main=paste(sp,variable))
  })
  }
  
  i<<-0
  make.plots(variable="SSB")
  #make.plots(variable="Yield")

  if (paper) cleanup()
  
} #end do.plots

if (do.plots2) {
 # user options
 nox<-1; noy<-1;
 if (paper) dev<-"wmf" else dev<-"screen"
 newplot(dev,nox,noy,dir=data.path,filename=paste("Batch2_",sep=''),Portrait=T);
  xlab.title<-'Cod F'

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names
 
  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  prob<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)

  a<-merge(ssb,yield)
  a<-merge(a,prob)
  a<-merge(a,fi)
  
  by(a,list(a$Species.n), function(a){
    if (paper=="Y") dev<-"wmf" else dev<-"screen"
    sp<-a[1,"Species.n"]
    newplot(dev,nox,noy,dir=outdir,filename=paste("HCR_",sp,sep=''),Portrait=F);
  
  
    par(mar=c(5,4,4,5)+.1)
    s<-a$SSB500/1000
    y<-a$y500/1000
    x<-a$codF
    x.lab<-xlab.title
  
    plot(x,s,ylab='SSB & Yield (1000 t)',xlab=x.lab ,ylim=c(min(s,y,0),max(s,y)),lty=1,type='l',lwd=2,col=1,main=sp.name[sp])
    if (include.probability) {    
      legend("topright",
         c('SSB','Yield', paste('p(SSB<',T1[sp],'t)'), paste('p(SSB<',T2[sp],'t)') ),
         pch="  12",lty=c(1,2,3,4),lwd=rep(2,4),col=c(1,2,3,4))
    } else {
      legend(min(x),0.85*max(s,y),
         c('SSB','Yield','F'),
         pch="   ",lty=c(1,2,5),lwd=rep(2,3))
    }
    lines(x,y,lty=2,lwd=2,col=2)
  
    par(new=T)
    plot(x,a$F500,axes=F,xlab=x.lab, ylab=' ',lty=5,lwd=2,,ylim=c(0,1),type='n')
    if (include.probability) {
      lines(x,a$p.T1,lty=3,lwd=2,type='b',pch="1",col=3)
      lines(x,a$p.T2,lty=4,lwd=2,type='b',pch="2",col=4)
      abline(h=0.05)
    }
    axis(side=4)
  
    #mtext(side=4,line=3.0,"Probability & F")
    mtext(side=4,line=3.0,"Probability")
    par(xaxs="r")
  
    if (paper=='Y') cleanup()
  })
} #end do.plots2

}
do.HCR.batch()


