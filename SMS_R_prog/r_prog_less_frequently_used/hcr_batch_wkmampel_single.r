# user options
nox<-1; noy<-1;
paper<-'N'   # if paper=="Y" output on file, else screen
cleanup()


first.year.in.mean<-2030    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2040

n.iterations<-10

include.probability<-TRUE

if (first.year.in.mean!=last.year.in.mean) myTitle<-paste(first.year.in.mean,'-',last.year.in.mean,sep='') else myTitle<-paste(first.year.in.mean)
# Scenarios, defined further later on

if (F) {
    scenario<-"TAC_constraints"
    fTarget<-0.4    # the prefered target F

    min<-0.05
    max<-0.25
    step<-0.05
    xlab.title<-'TAC constraints +/-'
}

if (T) {
    scenario<-"WKMAMPEL-Ftarget"
    min<-0.1       # min F
    max<-1.0
    step<-0.1
    xlab.title<-"Target-F"  
}


do.simulation<-T          # do the simulations and create data files for plots
do.plots<-TRUE            # do the plots

T1<- 50000      # SSB that gives F=0
T2<-100000      # SSB that gives F=Ftarget


percentiles<-c(0.025,0.05,0.10,0.25,0.50,0.75,0.95,0.975)

######### end user options #########
do.HCR.batch<-function() {


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
  scenario.dir<-scenario

  dir.create(scenario.dir,showWarnings = FALSE)

  SMS.files.single<-c("natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                      "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                      "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                      "proportion_M_and_F_before_spawning.in",
                      "Exploitation_pattern.in","covariance_N.in","HCR_options.dat",
                      "SMS.dat","SMS.exe")

  for (from.file in SMS.files.single) {
    to.file<-file.path(data.path,scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names
 
  setwd(outdir)

  write.FLSMS.control(control,file='SMS.dat')

  iterations<-seq(min,max,step)

  
  # headers in output files
  heading<-"option "
  cat(paste(heading),file=ssb.out)
  cat(paste("SSB",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=ssb.out,append=T); cat('\n',file=ssb.out,append=T) 
  cat(paste(heading),file=yield.out)
  cat(paste("y",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=yield.out,append=T); cat('\n',file=yield.out,append=T) 
  cat(paste(heading),file=F.out)
  cat(paste("F",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=F.out,append=T); cat('\n',file=F.out,append=T)    
  cat(paste(heading," p.T1 \n"),file=prob.out)

  iter<-0

  for (option in (iterations)) {
      iter<-iter+1

      print(paste("Iteration:",iter))

      #if (HCR.vari !="") {
      #  slot(HCR,HCR.vari)[1,1]<-option
      #}
       HCR@no.MCMC.iterations <-n.iterations
       HCR@last.prediction.year <- last.year.in.mean
       
       smallF<-0.001
      if (scenario=="WKMAMPEL-Ftarget") {
            HCR@HCR[1,1]<-10
            HCR@Trigger["T1",1]<-T1
            HCR@Trigger["T2",1]<-T2 # just a very high number
            HCR@Trigger.a.b["FT1a",1]<- smallF
            HCR@Trigger.a.b["FT1b",1]<- 0
            HCR@Trigger.a.b["FT12a",1]<- smallF           # intercept                     
            HCR@Trigger.a.b["FT12b",1]<- option/(T2-T1)                      # slope   
            HCR@Trigger.a.b["FT2a",1]<-option
            HCR@Trigger.a.b["FT2b",1]<-0                      
      }

      if (scenario=="TAC_constraints") {
           HCR@implementation["std",1]<-option
           HCR@TAC.constraints['min',1]<-1-option
           HCR@TAC.constraints['max',1]<-1+option
           
            HCR@HCR[1,1]<-10
            HCR@Trigger["T1",1]<-T1
            HCR@Trigger["T2",1]<-T2 # just a very high number
            HCR@Trigger.a.b["FT1a",1]<- smallF
            HCR@Trigger.a.b["FT1b",1]<- 0
            HCR@Trigger.a.b["FT12a",1]<- smallF           # intercept                     
            HCR@Trigger.a.b["FT12b",1]<- targetF/(T2-T1)                      # slope   
            HCR@Trigger.a.b["FT2a",1]<-targetF
            HCR@Trigger.a.b["FT2b",1]<-0                      

      }
        
        
      write.FLSMS.predict.control(HCR,SMS=control,file='HCR_options.dat')

      #run SMS
      shell(paste("sms.exe","-mceval",sep=" "), invisible = TRUE)


      a<-Read.MCMC.SSB.rec.data(dir=outdir)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      #print(a)
      b<-tapply(a$SSB,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      cat(paste(option,' '),file=ssb.out,append=TRUE)
      cat(b[[1]],file=ssb.out,append=TRUE)
      cat('\n',file=ssb.out,append=TRUE)

      q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
      q[q>T1]<-0
      q[q>0]<-1
      p.T1<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])
      cat(paste(option,p.T1,'\n'),file=prob.out,append=TRUE)

      a<-Read.MCMC.F.yield.data(dir=outdir)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      b<-tapply(a$Yield,list(a$Species.n), function(x) quantile(x,probs =percentiles))
      cat(paste(option,' '),file=yield.out,append=TRUE)
      cat(b[[1]],file=yield.out,append=TRUE)
      cat('\n',file=yield.out,append=TRUE)

      b<-tapply(a$mean.F,list(a$Species.n), function(x) quantile(x,probs = percentiles))
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

  sp.name<-control@species.names


  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  prob<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)

  a<-merge(ssb,yield)
  a<-merge(a,prob)
  a<-merge(a,fi)


  if (paper=="Y") dev<-"wmf" else dev<-"screen"
  newplot(dev,nox,noy,dir=outdir,filename=paste("HCR_",sp.name,sep=''),Portrait=F);

  par(mar=c(5,4,4,5)+.1)
  s<-a$SSB500/1000
  y<-a$y500/1000
  x<-a$option
  x.lab<-xlab.title

  plot(x,s,ylab='SSB & Yield (1000 t)',xlab=x.lab ,ylim=c(min(s,y,0),max(s,y)),lty=1,type='l',lwd=2,col=1,main=myTitle)
  if (include.probability) {
    legend("topright",
       c('SSB','Yield','F', paste('p(SSB<',round(T1),'t)')),
       pch="   1",lty=c(1,2,5,3),col=c(1,2,3,4), lwd=rep(2,4))
  } else {
    legend(min(x),0.85*max(s,y),
       c('SSB','Yield','F'),
       pch="   ",lty=c(1,2,5),lwd=rep(2,3),col=c(1,2,3))
  }
  lines(x,y,lty=2,lwd=2,col=2)

  par(new=T)
  plot(x,a$F500,axes=F,xlab=x.lab, ylab=' ',lty=5,lwd=2,ylim=c(0,1),type='l',col=3)
  if (include.probability) {
    lines(x,a$p.T1,lty=3,type='b',pch="1",col=4)
    abline(h=0.05)
  }
  axis(side=4)

  mtext(side=4,line=3.0,"Probability & F")
  par(xaxs="r")

  if (paper=='Y') cleanup()
  
} #end do.plots

setwd(data.path)
}
do.HCR.batch()