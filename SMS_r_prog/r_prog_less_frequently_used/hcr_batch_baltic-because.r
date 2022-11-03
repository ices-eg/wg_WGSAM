# user options
nox<-2; noy<-3;
paper<-F   # if paper==T output on file, else screen
cleanup()

first.year.in.mean<-2049    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2050

do.simulation<-F     # do the simulations and create data files for plots
do.plots<-T            # do the plots


######### end user options #########
do.HCR.batch<-function() {

#  files for output
ssb.out<-'HCR_SSB.dat'
yield.out<-'HCR_yield.dat'
F.out<-'HCR_F.dat'

if (do.simulation) {

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat.gem')

  sp.name<-control@species.names

   xlab.title<-'Implentation bias'

  # headers in output files
  cat('codF cluF Species.n SSB025 SSB250 SSB500 SSB750 SSB975 \n',file=ssb.out)
  cat('codF cluF Species.n y025 y050 y500 y750 y975 \n',file=yield.out)
  cat('codF cluF Species.n F025 F050 F500 F750 F975 \n',file=F.out)


  iter<-0

  BaseF<-HCR@constant.F
  print (BaseF)
  codF<-seq(0.01,1,0.1)
  ClupoidF<-seq(0.01,0.8,0.1)
  
  percentiles<-c(0.025,0.25,0.50,0.75,0.975)

  for (CODF in (codF)) for (CLUF in (ClupoidF)) {
      iter<-iter+1

      HCR@constant.F[1]<-BaseF[1]*CODF
      HCR@constant.F[2]<-BaseF[2]*CLUF*0.5
      HCR@constant.F[3]<-BaseF[3]*CLUF
       
      write.FLSMS.predict.control(HCR,file='HCR_options.dat')

      #run SMS
      shell(paste( file.path(root,"program","sms.exe"),"-mceval",sep=" "), invisible = TRUE)

      a<-Read.MCMC.SSB.rec.data()
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)

      b<-tapply(a$SSB,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(CODF,' ',CLUF,' ',i,' '),file=ssb.out,append=TRUE)
         cat(b[[i]],file=ssb.out,append=TRUE)
         cat('\n',file=ssb.out,append=TRUE)
      }

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
  make.plots(variable="Yield")

  if (paper) cleanup()
  
} #end do.plots

}
do.HCR.batch()


