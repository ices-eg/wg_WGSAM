# user options


my.dev<-'wmf'   # output device:  'screen', 'wmf', 'png', 'pdf'
cleanup()

old.risk<-F
first.year.in.mean<-2018    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2027

new.risk<-T
year.short<-c(2014,2017)
year.long<-c(2018,2027)

short.lived.risk<-T
R1<-T  # Risk type 1
R2<-F  # Risk type 2
R3<-T  # Risk type 3
short.lived.risk.years<-c(3,4,5,6,7) #years after the last assessment year

include.probability<-TRUE

number.stochastic.recruitment<- 10000  # number of iterations in case of stochastic recruitment


 
NOP<-function(scenario="Scen_112",run='minTAC',doHisto=F,xpos=1,ypos=0.8,do.simulation=TRUE,do.plots=TRUE,do.plot.box.ssb=T,riskTab=T,stochastic.recruitment=T,plotFval=0.5) {

  plotfile<-function(dev='screen',out) {
    out<-file.path(data.path,paste(out,run,sep='_'))
    cat(out,'\n')
    if (dev=='screen') X11(width=8, height=8, pointsize=12)
    if (dev=='wmf') win.metafile(filename = paste(out,'.wmf',sep=''), width=8, height=6, pointsize=12)
    if (dev=='png') png(filename = paste(out,'.png',sep=''), width = 1200, height = 1000,units = "px", pointsize = 25, bg = "white")
    if (dev=='pdf')  pdf(file =paste(out,'.wmf',sep=''), width = 8, height = 6,pointsize = 12,onefile=FALSE)
   }


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
  source(file.path(prog.path,"make_psv.R"))
  bio.interact<-FALSE

  #  runs are made in a separate dirictory
  scenario.dir<-scenario

  if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
  dir.create(scenario.dir,showWarnings = FALSE)

  SMS.files.single<-c("area_names.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                      "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                      "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                      "proportion_M_and_F_before_spawning.in","proportion_landed.in",
                      "zero_catch_year_season.in","zero_catch_season_ages.in",
                      "Exploitation_pattern.in",
                      "covariance_N.in","assessment_CV_age.in",
                      "HCR_options.dat","SMS.exe","SMS.dat","cp.bat")

  for (from.file in SMS.files.single) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }
 
 
  
# Make HCR control object
SMS.control<-read.FLSMS.control()
 
sp.name<-SMS.control@species.names



# make HCR_option.dat object
HCR<-FLSMS.predict.control(
    first.prediction.year=2012, 
    last.prediction.year=last.year.in.mean+1,
    no.species=1,
    species.names=sp.names
)

HCR@read.stock.N[]<-c(2012,2012)
     
HCR@years.weca["first-year",]<-   1983

if (stochastic.recruitment) {
   HCR@rec.noise["lower",]<- -2
   HCR@rec.noise["upper",]<-  2
} else {
   HCR@rec.noise["lower",]<-  0
   HCR@rec.noise["upper",]<-  0
}
HCR@target.SSB[]<-T2
 
HCR@obs.noise["lower",]<- -2
HCR@obs.noise["upper",]<-  2

HCR@read.rec.SSB.parm<-0             # 0=use estimated SSB-R parameters, 1= read in


HCR@HCR.F.TAC[]<-0              # Use F
HCR@inter.year[,]<-1            # No. of intermediate years

#  Assessment uncertanties September assessment 
HCR@assessment[,]<- c(1,1,0.20,1)     # log normal, same noise on all ages
#HCR@assessment[,]<- c(2,1,-1,-1)     # noise from co-variance matrix 
#HCR@assessment[,]<- c(4,1,-1,-1)     # Log-normal, age dependent noise (input from file) 


HCR@intermediate.F[,]<-c( -1, -1) # F two first years
HCR@intermediate.TAC[]<- c( 27083, 200000) # TAC the first year (2012) and TAC Q1-Q3 the second Year (2013)  
HCR@recruit.adjust.CV[]<-0                   # adjust recruit by half of the variance 
HCR@read.F[]<-c(2012,2012) 
HCR@use.read.F<-1

HCR@age.group.output[,]<-0   # no detailed age output
SMS.control@test.output<-0

if (scenario=="Scen_112") {
  HCR@HCR[1,]<-112

  HCR@growth.model[2,1]<-0.28  #  Cap F for Q4 (CapFQ4 =0.28)               
  HCR@growth.model[3,1]<-0.32  #  Cap F for Q1-Q3 (CapFQ1-Q3 =0.32)
  HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC                
  HCR@growth.model[5,1]<-200000  #  Maximum (observed) TAC   
  

  HCR@real.time[,]<-c(-1,0,0,2)   # forth value=upper F (observed) used for setting the TAC from the escapement strategy
    
  if (run=='test')   {
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'test'
    min<-50000
    max<-50000
    step=10000
    number.stochastic.recruitment<-1
    
    HCR@last.prediction.year<-2014
    SMS.control@test.output<-53
    
    HCR@age.group.output[,'F']<-1  
    HCR@age.group.output[,'N']<-1
    HCR@age.group.output[,'C']<-1      
  } 
  

  if (run=='minTAC_noise1')   {
    HCR@assessment[,]<- c(1,1,0.20,1)     # log normal, same noise on all ages
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<-0000     # begin at 0
    max<-50000      # 500000
    step=10000     # step 10000
  }
  
  if (run=='minTAC_noise2')   {
    HCR@assessment[,]<- c(2,1,-1,-1)     # noise from co-variance matrix 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<-0000     # begin at 0
    max<-50000      # 500000
    step=10000     # step 10000
  }
  
  if (run=='minTAC_noise4')   {
    HCR@assessment[,]<- c(4,1,-1,-1)     # Log-normal, age dependent noise (input from file) 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<-0000     # begin at 0
    max<-50000      # 500000
    step=10000     # step 10000
  }
  
   if (run=='minMaxTAC')   {
    HCR@growth.model[5,1]<-150000  #  Maximum (observed) TAC   
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<-0000     # begin at 0
    max<-50000      # 500000
    step=10000     # step 10000
  }
 
  
  if (run=='maxTAC')   {
    HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<-  50000
    max<- 250000
    step<- 25000
  }
  if (run=='final100')   {
    HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-100000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<- 100000
    max<- 100000
    step<-100000
  }
    if (run=='final200')   {
    HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-200000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<- 200000
    max<- 200000
    step<-200000
  }
  if (run=='finalSensi100')   {
    HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-100000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  if (run=='finalSensi200')   {
    HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-200000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  if (run=='sensiFtac100')   {
  HCR@growth.model[2,1]<-0.28*2  #  Cap F for Q4 (CapFQ4 =0.28)               
  HCR@growth.model[3,1]<-0.32*2  #  Cap F for Q1-Q3 (CapFQ1-Q3 =0.32)
  HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC                
  HCR@growth.model[5,1]<-100000  #  Maximum (observed) TAC   
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum F used to set the TAC'
    min<- 0.4
    max<- 0.8
    step<-0.1
  }
  if (run=='sensiFtac200')   {
  HCR@growth.model[2,1]<-0.28*2  #  Cap F for Q4 (CapFQ4 =0.28)               
  HCR@growth.model[3,1]<-0.32*2  #  Cap F for Q1-Q3 (CapFQ1-Q3 =0.32)
  HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC                
  HCR@growth.model[5,1]<-200000  #  Maximum (observed) TAC   
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum F used to set the TAC'
    min<- 0.4
    max<- 0.8
    step<-0.1
  }

}

  if (stochastic.recruitment) HCR@no.MCMC.iterations<-number.stochastic.recruitment else   HCR@no.MCMC.iterations<-1 #no of iterations

  setwd(outdir)
  write.FLSMS.control(SMS.control,write.multi=F)
  iterations<-seq(min,max,step)

  # headers in output files
  cat('option SSB025 SSB250 SSB500 SSB750 SSB975 \n',file=ssb.out)
  cat('option y025 y050 y500 y750 y975 \n',file=yield.out)
  cat('option F025 F050 F500 F750 F975 \n',file=F.out)
  #cat('option p.T1 p.T2 \n',file=prob.out)
  cat('option p.T1 p.T2 p.r1.short p.r2.short p.r3.short p.r1.long p.r2.long p.r3.long', paste("short.",short.lived.risk.years,sep=''), '\n',file=prob.out)

  iter<-0

  for (option in (iterations)) {
      iter<-iter+1

      print(paste("Iteration:",iter))

      if (HCR.vari !="") {
        slot(HCR,HCR.vari)[1,1]<-option
      }
      else if (scenario=="Scen_112") {
         if (run=='minTAC' | run=='minTAC_noise1' | run=='minTAC_noise2' | run=='minTAC_noise4' |run=='minMaxTAC' ) HCR@growth.model[4,1]<-option
         if (run=='maxTAC' | run=='final100' | run=='final2500') HCR@growth.model[5,1]<-option
         if (run=='finalSensi' |run=='finalSensi100' |run=='finalSensi200') {
           HCR@growth.model[2,1]<-0.28*option  #  Cap F for the Q4           
           HCR@growth.model[3,1]<-0.32*option  #  Cap F for the Q1-Q3
         } 
         if (run=='sensiFtac100' | run=='sensiFtac200')  HCR@real.time[4,1]<-option     
      }
 
            
      write.FLSMS.predict.control(HCR,SMS.control,file='HCR_options.dat')

      #run SMS
      shell(paste( file.path(data.path,scenario.dir,"sms.exe"),"-mceval  ",sep=" "), invisible = TRUE)
       
      aa<-Read.MCMC.SSB.rec.data(dir=outdir)
      if (do.plot.box.ssb) {
        a<-subset(aa,Year>=2013 & Year<=last.year.in.mean)
        b<-aggregate(SSB~ Year, data=a, function(x) quantile(x,probs = c(0.05,0.50,0.95))) 
        plotfile(dev=my.dev,out=paste("Risk",scenario,option,sep='_'));
        print(matplot(b$Year,b$SSB/1000,ylab='SSB(1000 t)',xlab='Year',type='b',ylim=c(0,max(b$SSB)/1000) ))
        abline(h=T1/1000,lty=2)
        if (my.dev!='screen') cleanup()
        print(b)
      }

      a<-subset(aa,Year>=first.year.in.mean & Year<=last.year.in.mean)
      
       
      #print(a)
      b<-tapply(a$SSB,list(a$Species.n), function(x) quantile(x,probs = c(0.025,0.25,0.50,0.75,0.975)))
      cat(paste(option,' '),file=ssb.out,append=TRUE)
      cat(b[[1]],file=ssb.out,append=TRUE)
      cat('\n',file=ssb.out,append=TRUE)

      q.save<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
       q<-q.save
      q[q>T2]<-0
      q[q>0]<-1
      p.T2<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])

      q<-q.save
      q[q>T1]<-0
      q[q>0]<-1
      p.T1<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])
      
      # Risks
      calc.Risk<-function(years){
        a<-subset(aa,Year>=years[1] & Year<=years[2] )
        q.save<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
        
        #print(ftable(round(q.save,0)))
         
        # risk 1
        q<-q.save
        q[q>T1]<-0
        q[q>0]<-1
        #cat("Risk 1:\n")
        #print(ftable(round(q,0)))
        p.r1<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])
        #print(p.r1 )
        #print(tapply(a$SSB,list(a$Year,a$Iteration),sum))
        #print((q[,1,]))
        
        # risk 2
        b<-apply(q,c(2,3),sum)
        #cat("risk 2:\n"); print(b)
        b[b>0]<-1
       # print(b)
        p.r2<-sum(b)/length(b)
        #print(p.r2)
        
        
        # risk 3
        #cat("Risk 3:\n")
        b<-apply(q,c(1,2),sum)
        #print(b)
        b<-b/dim(q)[3]
        #print(b)
        p.r3<-max(b)
        #print(p.r3)
        return( c(p.r1,p.r2,p.r3))
      }
      
      if (riskTab) {
        fy.risk<-2014
        ny.risk<-4
        
        if (iter==1) {
          tabfile<-file.path(data.path,paste('Risk_tab',run,'.out',sep='_'))
          cat("option fy ly p1 p2 p3\n",file=tabfile)
        }
        for (i in (1:3)) {
          y1<-fy.risk+(i-1)*ny.risk
          y2<-fy.risk+i*ny.risk-1
          pr<-calc.Risk(c(y1,y2)) 
          cat(option,y1,y2,pr,'\n',file=tabfile,append=T)
        }
      }
                    
      if (new.risk) {
        short<-calc.Risk(year.short)
        #cat("\nshort risk:",short,'\n')
        long<-calc.Risk(year.long)
        #cat('\nlong:',long,'\n')
      }
      
      if (short.lived.risk) {
        a<-droplevels(subset(aa,Year  %in% (SMS.control@last.year.model +short.lived.risk.years)))
        q<-tapply(a$SSB,list(a$Year,a$Repetion,a$Iteration),sum)
        q[q>T1]<-0
        q[q>0]<-1
        b<-apply(q,1,sum)/dim(q)[[2]]/dim(q)[[3]]
      }
      
      cat(paste(c(option,p.T1,p.T2, short, long, b )),'\n',file=prob.out,append=TRUE)

 
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
  if (!do.simulation) xlab.title<-'Factor, Annual Cap F=0.6'
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

  # print(a)
   
  cleanup()

   plotfile(dev=my.dev,out=paste("HCR_",scenario,sep=''));

  par(mar=c(5,4,4,5)+.1)
  s<-a$SSB500/1000
  y<-a$y500/1000
  x<-a$option
  x.lab<-xlab.title

  plot(x,s,ylab='SSB & Yield (1000 t)',xlab=x.lab ,ylim=c(min(s,y,0),max(s,y)),lty=1,type='l',lwd=2,col=1)
  xleg=x[1]*xpos; yleg=s[3]*ypos

  if (include.probability) {
   if (old.risk) { 
      #legend("topright",
      legend(x=xleg,y=yleg,
         c('SSB','Yield','F', paste('p(SSB<',T1,'t)'), paste('p(SSB<',T2,'t)') ),
         pch="   12",lty=c(1,2,5,3,4),col=c(1,2,3,4,5),lwd=rep(2,5))
   } else  if (new.risk) {
       legend(x=xleg,y=yleg, ncol=2,
       c('SSB','Yield','F', 'Prob 3 (2018-27)','Prob(2014)','Prob(2015)','Prob(2016)','Prob(2017)'),
     #  c('SSB','Yield','F', 'Short term risk', 'Long term risk'),
        pch="   L4567",lty=c(1,2,5,3,4,4,4,4),col=c(1,2,3,4,6,6,6,6),lwd=rep(2,8))
   }
  } else {
    legend(min(x),0.85*max(s,y),
       c('SSB','Yield','F'),
       pch="   ",lty=c(1,2,5),lwd=rep(2,3),col=c(1,2,3))
  }
  lines(x,y,lty=2,lwd=2,col=2)

  par(new=T)
  plot(x,a$F500,axes=F,xlab=x.lab, ylab=' ',lty=5,lwd=2,ylim=c(0,plotFval),type='l',col=3)
  if (include.probability) {
    if (old.risk) {
      lines(x,a$p.T1,lty=3,type='b',pch="1",col=4)
      lines(x,a$p.T2,lty=4,type='b',pch="2",col=5)
    }
    
    if (new.risk & F) {
     if (R1) lines(x,a$p.r1.short,lty=3,type='b',pch="1",col=4)
     if (R2) lines(x,a$p.r2.short,lty=3,type='b',pch="2",col=4)
     if (R3) lines(x,a$p.r3.short,lty=3,type='b',pch="3",col=4)
     
     if (R1) lines(x,a$p.r1.long,lty=4,type='b',pch="1",col=5)
     if (R2) lines(x,a$p.r2.long,lty=4,type='b',pch="2",col=5)
     if (R3) lines(x,a$p.r3.long,lty=4,type='b',pch="3",col=5)
    }
     if (new.risk) {
      # if (R1) lines(x,a$p.r1.long,lty=4,type='b',pch="a",col=4)
       if (R3) lines(x,a$p.r3.long,lty=3,type='b',pch="L",col=4)
    }
   
    if (short.lived.risk) { 
       lines(x,a$short.4,lty=4,type='b',pch="4",col=6)      #2014
       lines(x,a$short.5,lty=4,type='b',pch="5",col=6)       #15
       lines(x,a$short.6,lty=4,type='b',pch="6",col=6)      #16
       lines(x,a$short.7,lty=4,type='b',pch="7",col=6)      #17
       #lines(x,a$short.8,lty=4,type='b',pch="8",col=6)      #18

    }
    abline(h=0.05,lty=2)
  }
  axis(side=4)

  mtext(side=4,line=3.0,"Probability & F")
  par(xaxs="r")

  if (my.dev!='screen') cleanup()
  
} #end do.plots

 setwd(data.path)
 
 if (doHisto) {
   scenario<<-scenario
   Name<<-run
   source(file.path(prog.path,"HCR_output_histo.R"))
   setwd(data.path)
 }
}

if (F) {
  NOP(scenario="Scen_112",run='minTAC_noise1',doHisto=F,do.simulation=T,xpos=1,ypos=0.45) 
  NOP(scenario="Scen_112",run='maxTAC',doHisto=F,do.simulation=T,xpos=1,ypos=0.88) 
  NOP(scenario="Scen_112",run='minMaxTAC',doHisto=F,do.simulation=T,xpos=1,ypos=0.45) 
  NOP(scenario="Scen_112",run='final100',doHisto=T,xpos=1,ypos=0.83) 
  NOP(scenario="Scen_112",run='final200',doHisto=T,xpos=1,ypos=0.6,plotFval=0.5) 
  NOP(scenario="Scen_112",run='finalSensi100',doHisto=F,xpos=1,ypos=0.83) 
  NOP(scenario="Scen_112",run='finalSensi200',doHisto=F,xpos=1.5,ypos=0.89) 
  NOP(scenario="Scen_112",run='minTAC_noise2',doHisto=F,do.simulation=T,xpos=1,ypos=0.45) 
  NOP(scenario="Scen_112",run='minTAC_noise4',doHisto=F,do.simulation=T,xpos=1,ypos=0.45) 
}

 NOP(scenario="Scen_112",run='sensiFtac100',doHisto=F,do.simulation=T,xpos=1,ypos=0.96) 
 NOP(scenario="Scen_112",run='sensiFtac200',doHisto=F,do.simulation=T,xpos=1.4,ypos=0.83) 


if (F) {
 a<- Read.MCMC.detailed.data(dir=file.path(data.path,"Scen_112"))
 N<-tapply(a$N,list(Year=a$Year,Q=a$Quarter,Age=a$Age),sum)
 ftable(round(N))
 
 FI<-tapply(a$F,list(Year=a$Year,Q=a$Quarter,Age=a$Age),sum)
 ftable(round(FI,3))
}

if (F) {
      a<-Read.MCMC.F.yield.data(dir=file.path(data.path,"Scen_112"))
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      tapply(a$Yield,list(a$Species.n), function(x) quantile(x,probs = c(0.025,0.25,0.50,0.75,0.975)))
      tapply(a$Yield,list(a$Species.n), mean)
}