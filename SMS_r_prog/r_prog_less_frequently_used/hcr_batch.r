# user options
nox<-1; noy<-1;
paper<-'N'   # if paper=="Y" output on file, else screen
cleanup()


first.year.in.mean<-2014    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2016
include.probability<-TRUE


# Scenarios, defined further later on
scenario<-"ConstantF"
scenario<-"Esc_Recruit"
scenario<-"Esc_CapF"

scenario<-"Esc_AssUncer"
scenario<-"Esc_Q3"
scenario<-"Esc_Recruit_F0"
 scenario<-"ConstantF_noise"
scenario<-"TAC_stable"
scenario<-"TAC_constraints"
scenario<-"Assess_noise"

do.simulation<-T      # do the simulations and create data files for plots
do.plots<-TRUE            # do the plots



######### end user options #########
do.HCR.batch<-function() {

#Read refence points from file reference_points.in
ref<-Read.reference.points()
blim<-ref[1,"Blim"]
bpa<-ref[1,"Bpa"]
T1<-blim
T2<-bpa

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
                      "Exploitation_pattern.in")

  for (from.file in SMS.files.single) {
    to.file<-file.path(data.path,scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names

 if (scenario=="Assess_noise") {
    min<-0.01
    max<-0.26
    step<-0.05
    xlab.title<-'Assessment noise'
    HCR.vari<-""
    HCR@assessment["dist",1]<-1 # log normal distributed error
    HCR@assessment["bias",1]<-1 # no bias
    HCR@assessment["std",1]<-0.01 # CV of error
    HCR@assessment["same",1]<-1
   }
  
  if (scenario=="TAC_constraints") {
    min<-0.05
    max<-0.25
    step<-0.05
    xlab.title<-'TAC constraints +/-'
    HCR.vari<-""
  }
  
  if (scenario=="ConstantF") {
    HCR@HCR[1]<-1
    HCR@real.time[,1]<--1
    HCR@survey[,1]<--1
    HCR@assessment[,1]<--1
    HCR@real.time[,1]<--1

    min<-0.05
    max<-0.75
    step=0.05
    xlab.title<-'F'
    HCR.vari<-"constant.F"    # slot name to be changed for each iteration
  }
   if (scenario=="ConstantF_noise") {
    HCR@HCR[1]<-1
    HCR@real.time[,1]<--1
    HCR@survey[,1]<--1
    HCR@assessment[,1]<--1
    HCR@real.time[,1]<--1
    HCR@F.cap[,1]<-0
    HCR@HCR.F.TAC[,1]<-0
    
    HCR@implementation["dist",1]<-1 # log normal distributed error
    HCR@implementation["bias",1]<-1 # no bias
    HCR@implementation["std",1]<-0.20 # CV of implentation error
    HCR@implementation["same",1]<-1
    
    HCR@constant.F[,1]<-0.35

    min<-0.00001
    max<-0.601
    step=0.1
    xlab.title<-'Implementation noise'
    HCR.vari<-""
  }

  if (scenario=="Esc_CapF") {
    HCR@HCR[1]<-98

    min<-0.2
    max<-2.0
    step=0.1
    xlab.title<-'Cap F'
    HCR.vari<-"F.cap"    # slot name to be changed for each iteration
  }
   if (scenario=="Esc_AssUncer") {
    HCR@HCR[1]<-98
    min<-0.1
    max<-0.8
    step=0.1
    xlab.title<-'Assessment uncertanty'
    HCR.vari<-''
  }
   if (scenario=="Esc_Recruit") {
    HCR@HCR[1]<-98
    min<-0.5
    max<-1.0
    step=0.05
    xlab.title<-'Recruitment factor'
    HCR.vari<-''
  }
  if (scenario=="TAC_stable") {
    HCR@HCR[1]<-15
    HCR@no.MCMC.iterations<-200
    HCR@read.F[1,"second"]<-2006
    HCR@HCR.F.TAC[,1]<-2
    
    min<-0
    max<-50
    step=10
    xlab.title<-'percent variation'
    HCR.vari<-''
  }

   if (scenario=="Esc_Recruit_F0") {
    HCR@HCR[1]<-98
    min<-0.5
    max<-1.0
    step=0.05
    xlab.title<-'Recruitment factor'
    HCR.vari<-''
  }

   if (scenario=="Esc_Q3") {
    HCR@HCR[1]<-98
    min<-100
    max<-151000
    step=25000
    xlab.title<-'Maximum 1st half year TAC'
    HCR.vari<-''
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
      
     if (scenario=="Assess_noise") {
        HCR@assessment["std",1]<-option # CV of error
     }

      
      
      if (scenario=="TAC_constraints") {
           HCR@implementation["std",1]<-option
           HCR@TAC.constraints['min',1]<-1-option
           HCR@TAC.constraints['max',1]<-1+option

     }
        
      if (scenario=="ConstantF_noise") {
        HCR@implementation["std",1]<-option
      }
      
      if (scenario=="Esc_AssUncer"){
        HCR@real.time["std",1]<-option
        HCR@assessment["std",1]<-option
      }

      if (scenario=="TAC_stable"){
      if (option==0) {
        HCR@TAC.constraints["min",1]<-0
        HCR@TAC.constraints["max",1]<-0
      } else {
         HCR@TAC.constraints["min",1]<-(100-option)/100
        HCR@TAC.constraints["max",1]<-(100+option)/100
      }
      }

      if (scenario=="Esc_Recruit") {
         a<-scan(file=file.path(data.path,"SSB_R.in"),comment.char = "#")
         if (option>0) a[2]<-a[2]+log(option)
         cat(file="SSB_R.in",a,"\n")
      }

      if (scenario=="Esc_Recruit_F0") {
         a<-scan(file=file.path(data.path,"SSB_R.in"),comment.char = "#")
         if (option>0) a[2]<-a[2]+log(option)
         cat(file="SSB_R.in",a,"\n")
         
         HCR@Trigger.a.b["FT12b",1]<-0
         HCR@Trigger.a.b["FT2a",1]<-0

         HCR@target.SSB[,1]<-10E10    #very high = closure
      }

      if (scenario=="Esc_Q3") {
       HCR@Trigger.a.b["FT12b",1]<-option/140E6
       HCR@Trigger.a.b["FT2a",1]<-option
      }
      
      
      write.FLSMS.predict.control(HCR,file='HCR_options.dat')

      #run SMS
      shell(paste( file.path(root,"program","sms.exe"),"-mceval",sep=" "), invisible = TRUE)


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

  plot(x,s,ylab='SSB & Yield (1000 t)',xlab=x.lab ,ylim=c(min(s,y,0),max(s,y)),lty=1,type='l',lwd=2)
  if (include.probability) {
    legend("right",
       c('SSB','Yield','F', paste('p(SSB<',round(T1),'t)'), paste('p(SSB<',round(T2),'t)') ),
       pch="   12",lty=c(1,2,5,3,4),lwd=rep(2,5))
  } else {
    legend(min(x),0.85*max(s,y),
       c('SSB','Yield','F'),
       pch="   ",lty=c(1,2,5),lwd=rep(2,3))
  }
  lines(x,y,lty=2,lwd=2)

  par(new=T)
  plot(x,a$F500,axes=F,xlab=x.lab, ylab=' ',lty=5,lwd=2,,ylim=c(0,1),type='l')
  if (include.probability) {
    lines(x,a$p.T1,lty=3,type='b',pch="1")
    lines(x,a$p.T2,lty=4,type='b',pch="2")
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