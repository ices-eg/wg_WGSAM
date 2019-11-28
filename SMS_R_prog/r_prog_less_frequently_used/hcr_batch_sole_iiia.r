# user options
nox<-1; noy<-1;
paper<-F   # if output on paper (paper=T) or screen (paper=F)
cleanup()


first.year.in.mean<-2020    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2040


# Scenarios, defined further later on
scenario<-"ConstantF"
do.simulation<-T       # do the simulations and create data files for plots
do.plots<-T            # do the plots
plot.F.line<-F
include.probability<-T

legend.pos<-"topright"
max.y.axis<- 4   # maximum value for yield and SS, use -1 for value defined by data

######### end user options #########
do.HCR.batch<-function() {

#Read refence points from file reference_points.in
ref<-Read.reference.points()
blim<-ref[1,"Blim"]
bpa<-ref[1,"Bpa"]
T1<-blim
T1<-1500
T2<-bpa
T2<-2000

outdir<-file.path(data.path,scenario)

#  files for output
ssb.out<-'HCR_SSB.dat'
yield.out<-'HCR_yield.dat'
F.out<-'HCR_F.dat'
prob.out<-'HCR_prob.dat'

if (do.simulation) {

  setwd(data.path)
  bio.interact<-FALSE

  #  runs are made in a separate dirictory
  copy.SMS.scenario.files(directory= scenario)
 
  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.names<-control@species.names

  if (scenario=="ConstantF") {
    HCR@HCR[1]<-1
   # HCR@real.time[,1]<--1
   # HCR@survey[,1]<--1
   # HCR@assessment[,1]<--1
   # HCR@implementation[,1]<- -1
   # HCR@real.time[,1]<--1

    min<-0.05
    max<-0.75
    step=0.10
    xlab.title<<-'F'
    HCR.vari<-"constant.F"    # slot name to be changed for each iteration
  
     }
  
  setwd(outdir)

  write.FLSMS.control(control,file='SMS.dat')

  iterations<-seq(min,max,step)

  # headers in output files
  cat('option SSB025 SSB250 SSB500 SSB750 SSB975 \n',file=ssb.out)
  cat('option y025 y050 y500 y750 y975 \n',file=yield.out)
  cat('option F025 F050 F500 F750 F975 \n',file=F.out)
  cat('option p.T1 p.T2 \n',file=prob.out)

  iter<-0

  for (option in (iterations)) {
      iter<-iter+1

      print(paste("Iteration:",iter))

      if (HCR.vari !="") {
        slot(HCR,HCR.vari)[1,1]<-option
      }
      
     # if (scenario=="ConstantF) { }
      
           
      write.FLSMS.predict.control(HCR,file='HCR_options.dat')

      #run SMS
      shell(paste('"',file.path(root,"program","sms.exe"),'"'," -mceval",sep=""), invisible = TRUE)


      a<-Read.MCMC.SSB.rec.data(dir=outdir)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      #print(a)
      b<-tapply(a$SSB,list(a$Species.n), function(x) quantile(x,probs = c(0.025,0.25,0.50,0.75,0.975)))
      cat(paste(option,' '),file=ssb.out,append=TRUE)
      cat(b[[1]],file=ssb.out,append=TRUE)
      cat('\n',file=ssb.out,append=TRUE)

      q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
      q[q>T2]<-0
      q[q>0]<-1                                                                   
      p.T2<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])

      q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
      q[q>T1]<-0
      q[q>0]<-1
      p.T1<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])
      cat(paste(option,p.T1,p.T2,'\n'),file=prob.out,append=TRUE)

      a<-Read.MCMC.F.yield.data(dir=outdir)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      b<-tapply(a$Yield,list(a$Species.n), function(x) quantile(x,probs = c(0.025,0.25,0.50,0.75,0.975)))
      cat(paste(option,' '),file=yield.out,append=TRUE)
      cat(b[[1]],file=yield.out,append=TRUE)
      cat('\n',file=yield.out,append=TRUE)

      b<-tapply(a$mean.F,list(a$Species.n), function(x) quantile(x,probs = c(0.025,0.25,0.50,0.75,0.975)))
      cat(paste(option,' '),file=F.out,append=TRUE)
      cat(b[[1]],file=F.out,append=TRUE)
      cat('\n',file=F.out,append=TRUE)
  }
}   # end do.simulations

if (do.plots) {

  setwd(outdir)
   # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.names<-control@species.names


  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  prob<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)

  a<-merge(ssb,yield)
  a<-merge(a,prob)
  a<-merge(a,fi)
 
  if (paper) dev<-"wmf" else dev<-"screen"
  newplot(dev,nox,noy,dir=outdir,filename=paste("HCR_",sp.names,sep=''),Portrait=T);

  par(mar=c(5,4,4,5)+.1)
  s<-a$SSB500/1000
  y<-a$y500/1000
  x<-a$option
  x.lab<-xlab.title
  if (max.y.axis>0) max.y<-max.y.axis 
  else max.y<-max(s,y)
  
  plot(x,s,ylab='SSB & Yield (1000 t)',xlab=x.lab ,ylim=c(min(s,y,0),max.y),lty=1,type='l',lwd=2)

  l.string<-c('SSB','Yield')
  l.lty<-c(1,2)
  l.pch<-"  "
  if (plot.F.line) {
    l.string<-c(l.string,'F')
    l.lty<-c(l.lty,5)
    l.pch<-paste(l.pch," ",sep='')
  }
  if (include.probability) {
    l.string<-c(l.string,paste('p(SSB<',T1,'t)'), paste('p(SSB<',T2,'t)'))
    l.lty<-c(l.lty,3,4)
    l.pch<-paste(l.pch,"12",sep="")
  }
  legend(legend.pos,l.string,pch=l.pch,lty=l.lty,lwd=rep(2,5))
  if (include.probability) lines(x,y,lty=2,lwd=2)

  par(new=T)
  if (plot.F.line) F.col<-1
  else F.col<-0
  
  plot(x,a$F500,axes=F,xlab=x.lab, ylab=' ',lty=5,lwd=2,,ylim=c(0,1),type='l',col=F.col)
  if (include.probability) {
    lines(x,a$p.T1,lty=3,type='b',pch="1")
    lines(x,a$p.T2,lty=4,type='b',pch="2")
    abline(h=0.05)
  }
  axis(side=4)

  mtext(side=4,line=3.0,"Probability & F")
  par(xaxs="r")

  if (paper) cleanup()
  
} #end do.plots

setwd(data.path)
}
do.HCR.batch()