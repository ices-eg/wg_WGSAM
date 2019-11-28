# user options


my.dev<-'wmf'   # output device:  'screen', 'wmf', 'png', 'pdf'
cleanup()

old.risk<-F
first.year.in.mean<-2017    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2026

new.risk<-T
year.short<-c(2013,2016)
year.long<-c(2017,2026)

short.lived.risk<-T
R1<-T  # Risk type 1
R2<-F  # Risk type 2
R3<-T  # Risk type 3
short.lived.risk.years<-c(1,2,3,4,5) #years after the last assessment year

include.probability<-TRUE

number.stochastic.recruitment<- 1000  # number of iterations in case of stochastic recruitment


NOP<-function(scenario="Scen_101",run='minTAC',doHisto=F,xpos=1,ypos=0.8,do.simulation=TRUE,do.plots=TRUE,stochastic.recruitment=T) {


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

  if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
  dir.create(scenario.dir,showWarnings = FALSE)

  SMS.files.single<-c("area_names.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                      "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                      "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                      "proportion_M_and_F_before_spawning.in","proportion_landed.in",
                      "zero_catch_year_season.in","zero_catch_season_ages.in",
                      "Exploitation_pattern.in","covariance_N.in","HCR_options.dat",
                      "SMS.exe","SMS.dat")

  for (from.file in SMS.files.single) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }
 
  source(file.path(prog.path,"make_psv.R"))
  
# Make HCR control object
SMS.control<-read.FLSMS.control()
SMS.control@test.output<-0
 
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
HCR@assessment[,]<- c(1,1,0.2,1)   #  Assessment uncertanties

HCR@intermediate.F[,]<-c( 0.01, -1) # F two first years
HCR@intermediate.TAC[]<- c( -1, -1) # TAC two first years
HCR@recruit.adjust.CV[]<-0                   # adjust recruit by half of the variance 
HCR@read.F[]<-c(2012,2012) 
HCR@use.read.F<-1

if (scenario=="Scen_101") {
  HCR@HCR[1,]<-101

  HCR@growth.model[2,1]<-0.13  #  Cap F for the first half-year                
  HCR@growth.model[3,1]<-0.47  #  Cap F for the second half-year
  HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC                
  HCR@growth.model[5,1]<-200000  #  Maximum (observed) TAC   
  HCR@intermediate.TAC[]<- c( -1, 30000) # TAC two first years (actually first year, and (first half) of second year)
   
  HCR@real.time[,]<- c(1,1,0.20,0)   #  Assessment uncertanties September assessment
  
  if (run=='minTAC')   {
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<- 0000
    max<-50000
    step=10000
  }
  
  
  if (run=='minTAC_high_rec') {
    HCR@read.stock.N[1,]<-c(2012,2012)  # Remember tu put recruitment in predict_stock_N.in 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<- 0000
    max<-50000
    step=10000
  }
  if (run=='maxTAC')   {
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<-  50000
    max<- 250000
    step<- 25000
  }
  if (run=='final100')   {
    HCR@intermediate.TAC[]<- c( -1, 27000) # TAC two first years (actually first year, and (first half) of second year)
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-100000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<- 100000
    max<- 100000
    step<-100000
  }
    if (run=='final250')   {
    HCR@intermediate.TAC[]<- c( -1, 27000) # TAC two first years (actually first year, and (first half) of second year)
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-250000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<- 250000
    max<- 250000
    step<-250000
  }
  if (run=='finalSensi100')   {
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-100000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  if (run=='finalSensi250')   {
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-250000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
}
else if (scenario=="Scen_110") {
  HCR@HCR[1,]<-110
  HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
  HCR@growth.model[2,1]<-0.13  #  Cap F for the first half-year                
  HCR@growth.model[3,1]<-0.47  #  Cap F for the second half-year
  HCR@growth.model[4,1]<-0.35  # Annual Target F 
  HCR@growth.model[5,1]<-1.0   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               

  if (run=='minTAC')   {
    HCR.vari<-"constant.TAC"     # slot name to be changed for each iteration
    xlab.title<-'Fixed TAC (tonnes)'
    min<- 0000
    max<-50000
    step=10000
  }
  else if (run=='TACfactor25K')   {
    HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'TAC factor'
    min<- 0.5
    max<- 1.0
    step= 0.1
  }
  else if (run=='TACfactor50K')   {
    HCR@constant.TAC[]<-50000    # Fixed TAC first half-year.
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'TAC factor'
    min<- 0.5
    max<- 1.0
    step= 0.1
  }
  else if (run=='CapFSensi25K')   {
    HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
    HCR@growth.model[5,1]<-0.7   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  else if (run=='CapFSensi50K')   {
    HCR@constant.TAC[]<-50000    # Fixed TAC first half-year.
    HCR@growth.model[5,1]<-0.7   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  else if (run=='final')   {
    HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
    HCR@growth.model[5,1]<-0.7   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Fixed TAC (tonnes))'
    min<- 50000
    max<- 50000
    step<-50000
  }
}
else if (scenario=="Scen_1010") {
  HCR@HCR[1,]<-101

  HCR@growth.model[2,1]<-0.00  #  Cap F for the first half-year   may be changed later on        
  HCR@growth.model[3,1]<-0.47  #  Cap F for the second half-year
  HCR@growth.model[4,1]<- 20000  #  Minimum (observed) TAC                
  HCR@growth.model[5,1]<-50000  #  Maximum (observed) TAC   
  HCR@intermediate.TAC[]<- c( -1, 30000) # TAC two first years (actually first year, and (first half) of second year)
   
  HCR@real.time[,]<- c(1,1,0.20,0)   #  Assessment uncertanties September assessment
  
  if (run=='minTAC')   {
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<- 0000
    max<-50000
    step=10000
  }
  if (run=='minTAC_high_rec') {
    HCR@read.stock.N[1,]<-c(2012,2012)  # Remember t0 put recruitment in predict_stock_N.in 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Minimum TAC (tonnes)'
    min<-00000
    max<-50000
    step=10000
  }
  if (run=='maxTAC')   {
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<-  50000
    max<- 250000
    step<- 25000
  }
  if (run=='final100')   {
    HCR@intermediate.TAC[]<- c( -1, 27000) # TAC two first years (actually first year, and (first half) of second year)
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-100000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<- 100000
    max<- 100000
    step<-100000
  }
    if (run=='final250')   {
    HCR@intermediate.TAC[]<- c( -1, 27000) # TAC two first years (actually first year, and (first half) of second year)
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-250000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Maximum TAC (tonnes)'
    min<- 250000
    max<- 250000
    step<-250000
  }
  if (run=='finalSensi100')   {
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-100000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  if (run=='finalSensi250')   {
    HCR@growth.model[4,1]<- 27000  #  Minimum (observed) TAC 
    HCR@growth.model[5,1]<-250000  #  Maximum (observed) TAC 
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
}
else if (scenario=="Scen_110") {
  HCR@HCR[1,]<-110
  HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
  HCR@growth.model[2,1]<-0.13  #  Cap F for the first half-year                
  HCR@growth.model[3,1]<-0.47  #  Cap F for the second half-year
  HCR@growth.model[4,1]<-0.35  # Annual Target F 
  HCR@growth.model[5,1]<-1.0   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               

  if (run=='minTAC')   {
    HCR.vari<-"constant.TAC"     # slot name to be changed for each iteration
    xlab.title<-'Fixed TAC (tonnes)'
    min<- 0000
    max<-50000
    step=10000
  }
  else if (run=='TACfactor25K')   {
    HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'TAC factor'
    min<- 0.5
    max<- 1.0
    step= 0.1
  }
  else if (run=='TACfactor50K')   {
    HCR@constant.TAC[]<-50000    # Fixed TAC first half-year.
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'TAC factor'
    min<- 0.5
    max<- 1.0
    step= 0.1
  }
  else if (run=='CapFSensi25K')   {
    HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
    HCR@growth.model[5,1]<-0.7   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  else if (run=='CapFSensi50K')   {
    HCR@constant.TAC[]<-50000    # Fixed TAC first half-year.
    HCR@growth.model[5,1]<-0.7   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  else if (run=='final')   {
    HCR@constant.TAC[]<-25000    # Fixed TAC first half-year.
    HCR@growth.model[5,1]<-0.7   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Fixed TAC (tonnes))'
    min<- 50000
    max<- 50000
    step<-50000
  }
}
else if (scenario=="Scen_111") {
  HCR@HCR[1,]<-111
  HCR@constant.TAC[]<-30000    # Fixed TAC first half-year.
  HCR@growth.model[2,1]<-0.13  #  Cap F for the first half-year                
  HCR@growth.model[3,1]<-0.47  #  Cap F for the second half-year
  HCR@growth.model[4,1]<-0     # Annual Target F (not relevante)
  HCR@growth.model[5,1]<-1.0   # proportion of the yield calculated for the second half-year  (escapement strategy) that is used for second half year TAC               

  if (run=='minTAC')   {
    HCR.vari<-"constant.TAC"     # slot name to be changed for each iteration
    xlab.title<-'Fixed TAC (tonnes)'
    min<- 0000
    max<-50000
    step=10000
  }
  else if (run=='CapFSensi25K')   {
    HCR@constant.TAC[]<-30000    # Fixed TAC first half-year.
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  else if (run=='CapFSensi50K')   {
    HCR@constant.TAC[]<-50000    # Fixed TAC first half-year.
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Factor, Annual Cap F=0.6'
    min<- 0.5
    max<- 3.0
    step<-0.5
  }
  else if (run=='final')   {
    HCR@constant.TAC[]<-50000    # Fixed TAC first half-year.
    HCR.vari<-""     # slot name to be changed for each iteration
    xlab.title<-'Fixed TAC (tonnes))'
    min<- 50000
    max<- 50000
    step<-50000
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
      else if (scenario=="Scen_101") {
         if (run=='minTAC' | run=='minTAC_high_rec') HCR@growth.model[4,1]<-option
         if (run=='maxTAC' | run=='final100' | run=='final250') HCR@growth.model[5,1]<-option
         if (run=='finalSensi' |run=='finalSensi100' |run=='finalSensi250') {
           HCR@growth.model[2,1]<-0.13*option  #  Cap F for the first half-year                
           HCR@growth.model[3,1]<-0.47*option  #  Cap F for the second half-year
         }     
      }
      else if (scenario=="Scen_1010") {
         if (run=='minTAC' | run=='minTAC_high_rec') HCR@growth.model[4,1]<-option
         if (run=='maxTAC' | run=='final100' | run=='final250') HCR@growth.model[5,1]<-option
         if (run=='finalSensi' |run=='finalSensi100' |run=='finalSensi250') {
           HCR@growth.model[2,1]<-0.13*option  #  Cap F for the first half-year                
           HCR@growth.model[3,1]<-0.47*option  #  Cap F for the second half-year
         }     
      }

      else if (scenario=="Scen_110") {
         if (run=='CapFSensi25K' | run=='CapFSensi50K')  {
           HCR@growth.model[2,1]<-0.13*option  #  Cap F for the first half-year                
           HCR@growth.model[3,1]<-0.47*option  #  Cap F for the second half-year
         }
         if (run=='TACfactor25K' | run=='TACfactor50K')  {
           HCR@growth.model[5,1]<-option  #  Proportion af calculated TAC actually used               
         }
      }
      else if (scenario=="Scen_111") {
         if (run=='CapFSensi25K' | run=='CapFSensi50K')  {
           HCR@growth.model[2,1]<-0.13*option  #  Cap F for the first half-year                
           HCR@growth.model[3,1]<-0.47*option  #  Cap F for the second half-year
         }
       }

            
      write.FLSMS.predict.control(HCR,SMS.control,file='HCR_options.dat')

      #run SMS
      shell(paste( file.path(data.path,scenario.dir,"sms.exe"),"-mceval  ",sep=" "), invisible = TRUE)
       
      aa<-Read.MCMC.SSB.rec.data(dir=outdir)
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
    
        # risk 1
        q<-q.save
        q[q>T1]<-0
        q[q>0]<-1
        p.r1<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])
        
        #print(tapply(a$SSB,list(a$Year,a$Iteration),sum))
        #print((q[,1,]))
        # risk 2
        b<-apply(q,c(2,3),sum)
        #print(aa)
        b[b>0]<-1
       # print(b)
        p.r2<-sum(b)/length(b)
    
        # risk 3
        b<-apply(q,c(1,2),sum)
        #print(b)
        b<-b/dim(q)[3]
        #print(aa)
        p.r3<-max(b)
        return( c(p.r1,p.r2,p.r3))
      }
      
      if (new.risk) {
        short<-calc.Risk(year.short)
        long<-calc.Risk(year.long)
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

  
  cleanup()
  plotfile<-function(dev='screen',out) {
    out<-file.path(data.path,paste(out,run,sep='_'))
    cat(out,'\n')
    if (dev=='screen') X11(width=8, height=8, pointsize=12)
    if (dev=='wmf') win.metafile(filename = paste(out,'.wmf',sep=''), width=8, height=6, pointsize=12)
    if (dev=='png') png(filename = paste(out,'.png',sep=''), width = 1200, height = 1000,units = "px", pointsize = 25, bg = "white")
    if (dev=='pdf')  pdf(file =paste(out,'.wmf',sep=''), width = 8, height = 6,pointsize = 12,onefile=FALSE)
   }

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
       c('SSB','Yield','F', 'Risk(2017-26)', 'Risk(2013)','Risk(2014)','Risk(2015)','Risk(2016)'),
     #  c('SSB','Yield','F', 'Short term risk', 'Long term risk'),
        pch="   L3456",lty=c(1,2,5,3,4,4,4,4),col=c(1,2,3,4,6,6,6,6),lwd=rep(2,8))
   }
  } else {
    legend(min(x),0.85*max(s,y),
       c('SSB','Yield','F'),
       pch="   ",lty=c(1,2,5),lwd=rep(2,3),col=c(1,2,3))
  }
  lines(x,y,lty=2,lwd=2,col=2)

  par(new=T)
  plot(x,a$F500,axes=F,xlab=x.lab, ylab=' ',lty=5,lwd=2,ylim=c(0,0.5),type='l',col=3)
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
     #if (R3) lines(x,a$p.r3.short,lty=3,type='b',pch="s",col=4)
     
     if (R1) lines(x,a$p.r1.long,lty=4,type='b',pch="L",col=4)
    }
   
    if (short.lived.risk) {
       #lines(x,a$short.1,lty=4,type='b',pch="1",col=6)      #2012
       lines(x,a$short.2,lty=4,type='b',pch="3",col=6)       #13
       lines(x,a$short.3,lty=4,type='b',pch="4",col=6)      #14
       lines(x,a$short.4,lty=4,type='b',pch="5",col=6)      #15
       lines(x,a$short.5,lty=4,type='b',pch="6",col=6)      #16

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
   source(file.path(prog.path,"HCR_output_histo.R"))
   setwd(data.path)
 }
}

#NOP(scenario="Scen_101",run='minTAC',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_101",run='minTAC_high_rec',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_101",run='maxTAC',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_101",run='final100',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_101",run='final250',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_101",run='finalSensi100',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_101",run='finalSensi250',doHisto=F,xpos=1.5,ypos=0.89) 

#NOP(scenario="Scen_1010",run='minTAC',doHisto=F,xpos=1,ypos=0.83) 
NOP(scenario="Scen_1010",run='minTAC_high_rec',doHisto=T,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_1010",run='maxTAC',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_1010",run='final100',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_1010",run='final250',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_1010",run='finalSensi100',doHisto=F,xpos=1,ypos=0.83) 
#NOP(scenario="Scen_1010",run='finalSensi250',doHisto=F,xpos=1.5,ypos=0.89) 



#NOP(scenario="Scen_110",run='minTAC',doHisto=F,xpos=1,ypos=0.95) 
#NOP(scenario="Scen_110",run='TACfactor25K',doHisto=F,xpos=1,ypos=0.95) 
#NOP(scenario="Scen_110",run='TACfactor50K',doHisto=F,xpos=1,ypos=0.95) 
#NOP(scenario="Scen_110",run='final250',doHisto=F,xpos=1,ypos=0.95) 
#NOP(scenario="Scen_110",run='CapFSensi25K',doHisto=F,xpos=1,ypos=0.95) 
#NOP(scenario="Scen_110",run='CapFSensi50K',doHisto=F,xpos=1,ypos=0.95) 
#NOP(scenario="Scen_110",run='final',doHisto=T,xpos=1,ypos=0.95) 

#NOP(scenario="Scen_111",run='minTAC',doHisto=F,xpos=1,ypos=0.8) 
#NOP(scenario="Scen_111",run='CapFSensi25K',doHisto=F,xpos=3,ypos=0.89) 
#NOP(scenario="Scen_111",run='CapFSensi50K',doHisto=F,xpos=3,ypos=0.89) 
#NOP(scenario="Scen_111",run='final',doHisto=T,xpos=1,ypos=0.8) 


