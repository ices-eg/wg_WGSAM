 
HPC<-TRUE   # use High Performance Computer (on Unix only)

############################
## North Sea or Baltic Sea runs
my.area<-c('North Sea','Baltic Sea')[1]   
NS.Key.2014<-T  # use the NS key run from 2014 (more species in the 2014 run, with more species for output figures)   
do.MSFD.indicators<-FALSE
use.stochastic.recruitement<-T
doHessian<-F
do.sensitivity<-FALSE

#######################
source(file.path(prog.path.func,"hcr_op_batch_optimize_function.r"))

  #                     Cod     Whiting     Haddock      Saithe     Herring      NorthSandeel  southSan     Nor. pout       Sprat      Plaice        Sole 
  HCR1<-            c(    1,          1,          1,          1,          1,          1,           1,          1,          1,          1,          1) 
  HCR2<-            c(    2,          2,          2,          2,          2,         22,          22,         22,         22,          2,          2) 
  my.FBlim.adjust<- c(    0,          0,          0,          0,          0,          1,           1,          1,          1,          0,          0)  # for specifying T1 in HCR (0: F=0 at SSB at 0, 1: F=0 at SSB at Blim)
  
  my.Fpa.mult1<-     c(    1,          1,          1,          1,          1,          1,          1,          1,          1,          1,          1)  # Maximum F relative to Fpa
  my.Fpa.mult2<-     c(    1,          1,          1,          1.5,        1,          1,          1,          1,          1,          1,          1)  # to let Saithe F be greater than Fpa (0.4)
  my.Fpa.mult3<-     c(    3,          3,          3,          1,          3,          3,          3,          3,          3,          3,          3)  

my.Fpa.mult.all<-matrix(c(my.Fpa.mult1,my.Fpa.mult2,my.Fpa.mult3),byrow=T,nrow=3)
HCRs<-matrix(c(HCR1,HCR2),byrow=TRUE,nrow=2)

if (do.sensitivity) {  # Sensistivity of the stock size of othet predators
  #
  #Other species multiplier
  
  mult<-0.8
  my.other<-c(
  #1    2  3     4     5     6     7    8     9   10
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #Fulmar         
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #Guillemot      
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #Her. Gull      
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #Kittiwake      
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #GBB. Gull      
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #Gannet         
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #Puffin         
  1, mult, 1,    1,    1,    1,    1,   1,    1,   mult,   #Razorbill      
  1, 1,    mult, 1,    1,    1,    1,   1,    1,   mult,   #R. radiata     
  1, 1,    1,    mult, 1,    1,    1,   1,    1,   mult,   #G. gurnards    
  1, 1,    1,    1,    mult, 1,    1,   1,    1,   mult,   #W. mackerel    
  1, 1,    1,    1,    mult, 1,    1,   1,    1,   mult,   #N. mackerel    
  1, 1,    1,    1,    1,    mult, 1,   1,    1,   mult,   #W. horse mac    
  1, 1,    1,    1,    1,    mult, 1,   1,    1,   mult,   #N. horse mac    
  1, 1,    1,    1,    1,    1,    mult,1,    1,   mult,   #Grey seal      
  1, 1,    1,    1,    1,    1,    1,   mult, 1,   mult,   #H. porpoise    
  1, 1,    1,    1,    1,    1,    1,   1,    mult,mult)   #Hake  
  
  fac.other.first.year<-2015  # first year for change (must be larger than first year in predeiction)
  fac.other.last.year<-2015  # last year for change
  
  my.other<-matrix(my.other,nrow=first.VPA-1,byrow=TRUE);
  dimnames(my.other)[[1]]<-sp.names[1:(first.VPA-1)]
  my.other
  #my.other<-my.other[,1:3]
  my.other<-my.other^(1/(fac.other.last.year-fac.other.first.year+1))
  n.oth<-(1:dim(my.other)[[2]])
} else {
  fac.other.first.year<- -1  # first year for change (must be larger than first year in predeiction)
  fac.other.last.year<- -1  # last year for change
  my.other<-matrix(1,nrow=first.VPA-1,ncol=2);
  n.oth<-1
}

###############  user input

just.batch.file<-FALSE   # make a batch file for a later run
just.plot.results<-FALSE   # just plot results from a previous run

nsp.VPA<-sum(SMS.control@species.info[,'predator'] %in% c(0,1))
recruit.adjust.factor<-rep(1,nsp.VPA)  # default no adjustment factor

if (use.stochastic.recruitement) {
  my.first.year<-2024
  my.last.year<-2124
  stochastic.recruitment<-1  # Stochastic recruitment?.
                          # 1=Yes   S-R parameters and variance from op_config.dat
                          # 2=Yes,  S-R parameters from op_config.dat and variance-covariance matrix from file covariance_Rec.in
                          # 3=Yes,  S-R parameters from op_config.dat and residuals from op_ssb_rec_residuals.in
                           # 21=Yes, S-R parameters from EquiSim results, file op_eqsim_stoch.in
  my.recruit.adjust.CV<-0    # adjust recruitment with half of the variance (factor exp((CV^2)/2) option adjust.recruit.CV
                             # 0=no adjustment
                             # 1=stochastic recruitment, do adjustment such that average recruitment becomes equal to the median (downward adjustment). Close to meaningless
                             # 2=determenistic recruitment, do ajustment such that recruitment becomes equal to the mean of the log-normal distribution (upward adjustment) 
} else {   # deterministic
  my.first.year<-2034 # years for optimization
  my.last.year<-2043
  #recruit.adjust.factor[3]<-0.75  #addjust haddock recruitment
  stochastic.recruitment<-20          # Stochastic recruitment?.
                # 0=No,   S-R parameters from op_config.dat
                # 20=No,  S-R parameters from EquiSim results, file op_eqsim.in

  my.recruit.adjust.CV<-c(2) #adjust recruitment with half of the variance (factor exp((CV^2)/2) option adjust.recruit.CV
                            # 0=no adjustment
                            # 1=stochastic recruitment, do adjustment such that average recruitment becomes equal to the median (downward adjustment). Close to meaningless
                            # 2=determenistic recruitment, do ajustment such that recruitment becomes equal to the mean of the log-normal distribution (upward adjustment)
}


# FPAmult is multiplier for Fpa to define maximum F 

for (FPAmult in (c(1))) for (adjRec in my.recruit.adjust.CV) for (my.HCR in c(1)) for (my.oth in n.oth) {  
  my.Fpa.mult<-as.vector(my.Fpa.mult.all[FPAmult,])    # upper limit for Fmsy relative to Fpa
  HCR.used<-HCRs[ifelse(my.HCR==3,2,my.HCR),]  # do not change
  if (my.HCR==2)  FBlim.adjust<-my.FBlim.adjust else FBlim.adjust<-1
  
   #  scenario        x.a     x.b     x.c   x.d    x.e    x.f      where x=HCR
   #----------------------------------------------------------
   #  FMSY<=Fpa       yes    yes     yes    yes    yes     yes
   #  ssb penalty     no     Blim    Bpa    no     Blim    Bpa
   #  price at age    yes    yes     yes    no     no      no   
   #----------------------------------------------------------
  
  #value
  #OP.opti(scenario.basis=paste(ifelse(use.stochastic.recruitement,'S','D'),my.HCR,"a",sep=''),my.first.year=my.first.year,adjust.ini.N=F,my.last.year=my.last.year,use.weigting=F,use.price.at.age=T,stochastic.recruitment=stochastic.recruitment,use.penalty=F,HCR=HCR.used,penalty='Blim',penalty.type=0,Hessian=doHessian, Fpa.Mult=my.Fpa.mult, recruit.adjust.CV=adjRec, recruit.adjust.factor=recruit.adjust.factor, FBlim.adjust=FBlim.adjust, just.batch.file=just.batch.file, just.plot.results=just.plot.results,fac.other.first.year=fac.other.first.year,fac.other.last.year=fac.other.last.year,fac.other=my.other[,my.oth],lab.other=ifelse(!do.sensitivity,'',as.character(my.oth)))
  #OP.opti(scenario.basis=paste(ifelse(use.stochastic.recruitement,'S','D'),my.HCR,"b",sep=''),my.first.year=my.first.year,adjust.ini.N=F,my.last.year=my.last.year,use.weigting=F,use.price.at.age=T,stochastic.recruitment=stochastic.recruitment,use.penalty=T,HCR=HCR.used,penalty='Blim',penalty.type=1,Hessian=doHessian, Fpa.Mult=my.Fpa.mult, recruit.adjust.CV=adjRec, recruit.adjust.factor=recruit.adjust.factor, FBlim.adjust=FBlim.adjust, just.batch.file=just.batch.file, just.plot.results=just.plot.results,fac.other.first.year=fac.other.first.year,fac.other.last.year=fac.other.last.year,fac.other=my.other[,my.oth],lab.other=ifelse(!do.sensitivity,'',as.character(my.oth)))
  #OP.opti(scenario.basis=paste(ifelse(use.stochastic.recruitement,'S','D'),my.HCR,"c",sep=''),my.first.year=my.first.year,adjust.ini.N=F,my.last.year=my.last.year,use.weigting=F,use.price.at.age=T,stochastic.recruitment=stochastic.recruitment,use.penalty=T,HCR=HCR.used,penalty='Bpa', penalty.type=1,Hessian=doHessian, Fpa.Mult=my.Fpa.mult, recruit.adjust.CV=adjRec, recruit.adjust.factor=recruit.adjust.factor, FBlim.adjust=FBlim.adjust, just.batch.file=just.batch.file, just.plot.results=just.plot.results,fac.other.first.year=fac.other.first.year,fac.other.last.year=fac.other.last.year,fac.other=my.other[,my.oth],lab.other=ifelse(!do.sensitivity,'',as.character(my.oth)))
  
  #weight
  #OP.opti(scenario.basis=paste(ifelse(use.stochastic.recruitement,'S','D'),my.HCR,"d",sep=''),my.first.year=my.first.year,adjust.ini.N=F,my.last.year=my.last.year,use.weigting=F,use.price.at.age=F,stochastic.recruitment=stochastic.recruitment,use.penalty=F,HCR=HCR.used,penalty='Blim',penalty.type=0,Hessian=doHessian,Fpa.Mult=my.Fpa.mult,recruit.adjust.CV=adjRec, recruit.adjust.factor=recruit.adjust.factor, FBlim.adjust=FBlim.adjust, just.batch.file=just.batch.file, just.plot.results=just.plot.results, fac.other.first.year=fac.other.first.year,fac.other.last.year=fac.other.last.year,fac.other=my.other[,my.oth],lab.other=ifelse(!do.sensitivity,'',as.character(my.oth)))
  OP.opti(scenario.basis=paste(ifelse(use.stochastic.recruitement,'S','D'),my.HCR,"e",sep=''),my.first.year=my.first.year,adjust.ini.N=F,my.last.year=my.last.year,use.weigting=F,use.price.at.age=F,stochastic.recruitment=stochastic.recruitment,use.penalty=T,HCR=HCR.used,penalty='Blim',penalty.type=1,Hessian=doHessian,Fpa.Mult=my.Fpa.mult,recruit.adjust.CV=adjRec, recruit.adjust.factor=recruit.adjust.factor, FBlim.adjust=FBlim.adjust, just.batch.file=just.batch.file, just.plot.results=just.plot.results, fac.other.first.year=fac.other.first.year,fac.other.last.year=fac.other.last.year,fac.other=my.other[,my.oth],lab.other=ifelse(!do.sensitivity,'',as.character(my.oth)))
  #OP.opti(scenario.basis=paste(ifelse(use.stochastic.recruitement,'S','D'),my.HCR,"f",sep=''),my.first.year=my.first.year,adjust.ini.N=F,my.last.year=my.last.year,use.weigting=F,use.price.at.age=F,stochastic.recruitment=stochastic.recruitment,use.penalty=T,HCR=HCR.used,penalty='Bpa' ,penalty.type=1,Hessian=doHessian,Fpa.Mult=my.Fpa.mult,recruit.adjust.CV=adjRec, recruit.adjust.factor=recruit.adjust.factor, FBlim.adjust=FBlim.adjust, just.batch.file=just.batch.file, just.plot.results=just.plot.results,fac.other.first.year=fac.other.first.year,fac.other.last.year=fac.other.last.year,fac.other=my.other[,my.oth],lab.other=ifelse(!do.sensitivity,'',as.character(my.oth)))  
}

