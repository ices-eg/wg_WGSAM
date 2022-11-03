############################
# Script for performing forecast scenarios for a high number of F combinations

###

my.area<-c('North Sea','Baltic Sea')[1]   ## North Sea or Baltic Sea runs
KeyRunYear<-c(2014,2017)[2]    # Number of species depends on key run (year) 

ask.me<-F  # you can be asked to confirm a run
HPC<-F   # use High Performance Computer (on DTU Unix only)

do.MSFD.indicators<-F
make.new.run<-T       # make a new run, or use the results from a previously run for plotting etc.



##########  Choose arae (above) and define scenario (e.g. HCR_1_deter_noadjust_test from keywords below .  


#sce<- 'HCR_1_deter_noadjust_single_test'   # Fixed F and Constant determenistic recruitment
#sce<- 'HCR_1_deter_adjust_single_test'     # Fixed F and Constant determenistic recruitment (but adjusted by half of the variance, to provide recruitmens similar to the mean of stochastic recruitments )
sce<- 'HCR_2_deter_adjust_single_test'    # Fixed F for SSB above treshold 
#sce<- 'HCR_2_deter_noadjust_single_test'    # Fixed F for SSB above treshold 
 
########## key_word for scenarios, for automatic settings, one from each group
# key words :
# _stoch1_  : recruitmentMode<-1    
# _stoch2_  : recruitmentMode<-2 
# _stoch3_  : recruitmentMode<-3 
# _stoch21_ : recruitmentMode<-21   # Equisim
# _deter_   : recruitmentMode<-0    # deterministic recruitment
# _deter20_ : recruitmentMode<-21   # deterministic recruitment Equisim
#
#_noadjust_" : recruit.adjust.CV<-0
#_adjust_" : recruit.adjust.CV<-2   # median to mean, only deterministic recruitment 
#
# HCR_1    : HCR_1 (fixed F)
# HCR_2    : HCR_2 (SSB dependent F)
#
# _single  : single species mode for creation af F combinations (just the diagonal), (default multispecies mode=all combinations)
#
# _wide1 .. _wide9    : combinations of F values 
# _narrow1  .. _narow9: combinations of F values 
# _test               : combinations of F values



##################################################################################
# Define combinations of F

if (grepl('_test',sce)) {
  ss<-0.5 
  if (KeyRunYear==2017) rr<-matrix(c(
    0.1, 0.5, ss,     #  COD   
    0.1, 0.5, ss,      # WHG
    0.1, 0.5, ss,     #  HAD
    0.1, 0.5, ss,     # POK 
    0.1, 0.5, ss,     # Mackerel
    0.1, 0.3, ss,     #  HER
    0.1, 0.5, ss,     # North SAN
    0.1, 0.5, ss,     # South SAN
    0.1, 1.0, ss,     #  NOP
    0.1, 1.0, ss,     # SPR
    0.2,  0.2,  ss,  #  PLE
    0.2,  0.2,  ss )  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  
  rr[,]<-0.1  # Alex test
  rr[4,]<-0.4  # Saithe
  rr[1,]<-0.2  # cod
 # rr[2,]<-0.2  # whiting
  rr[5,]<-0.3  # mackerel
}

##################################################################################
if (OS!="unix" ) HPC<-FALSE

automatic.settings<-T   # set options automaticacly from sceenario name (sce) 

# Stochastic recruitment.
# 0=No,  S-R parameters from OP_config.dat
# 1=Yes  S-R parameters and variance from OP_config.dat
# 2=Yes, S-R parameters from OP_config.dat and variance-covariance matrix from file covariance_Rec.in
# 3=Yes, S-R parameters from OP_config.dat and residuals from OP_SSB_rec_residuals.in

recruitmentMode<-1
if (automatic.settings) {
 if (grepl('_stoch',sce)) {
   if (grepl('_stoch1_',sce)) recruitmentMode<-1 
   if (grepl('_stoch2_',sce)) recruitmentMode<-2 
   if (grepl('_stoch3_',sce)) recruitmentMode<-3 
   if (grepl('_stoch21_',sce)) recruitmentMode<-21 
 }
 else if (grepl('_deter_',sce)) recruitmentMode<-0
 else if (grepl('_deter20_',sce)) recruitmentMode<-20

 if (grepl('_single',sce)) justDiagonal<-T else justDiagonal<-F
}


# adjust recruitment with half of the variance (factor exp(-(CV^2)/2) option adjust.recruit.CV
# 0=no adjustment\n",
# 1=stochastic recruitment, do adjustment such that average recruitment becomes equal to the median (downward adjustment)\n",
# 2=determenistic recruitment, do ajustment such that recruitment becomes equal to the mean of the log-normal distribution (upward adjustment)\n", file=file,append=T,sep="")
recruit.adjust.CV<-0        
if (automatic.settings) {
 if (grepl("_noadjust_",sce)) recruit.adjust.CV<-0
 if (grepl("_adjust_",sce)) {
   if (recruitmentMode==0 |recruitmentMode==20 ) recruit.adjust.CV<-2 else  recruit.adjust.CV<-1
 }
}


nsp.VPA<-sum(SMS.control@species.info[,'predator'] %in% c(0,1))

recruit.adjust.factor<-rep(1,nsp.VPA)  # default no adjustment factor

if (OS=='windows') op.command<-'op.exe' else if (OS=='unix') op.command<-'op'



source(file=file.path(prog.path.func,'hcr_op_batch_simulate_function.r'))
 
if (my.area=='North Sea') {
  
  # OP options, HCR by species
  # 1: Target F
  #      F=Ftarget
  # 2: Target F, and known trigger T1 and T2
  #      F=0                        for     SSB<T1
  #      F=Ftarget*(SSB-T1)/(T2-T1) for  T1<SSB<T2
  #      F=Ftarget                  for  T2<SSB
  # 3: Target F and Fslope, and known trigger T1 and T2
  #      F=0                          for     SSB<T1
  #      F=Ftarget*(SSB-T1)/(T2-T1)   for  T1<SSB<T2
  #      F=Ftarget+(SSB-T2)/T2*Fslope for  T2<SSB
  #  4: Logistic curve
  #      F=Ftarget / (1+exp(S1 - S1/SSB50%*SSB)
  #  22, 33 and 44 is the same rule as 2,3 and 4 but TSB is used
  
  if (KeyRunYear==2014) {
    #                     Cod     Whiting     Haddock      Saithe     Herring     North-Sandeel South-Sandeel   Nor. pout       Sprat      Plaice        Sole 
    HCR1<-            c(    1,          1,          1,          1,          1,          1,          1,             1,          1,          1,          1) 
    HCR2<-            c(    2,          2,          2,          2,          2,         22,         22,            22,         22,          2,          2) 
    my.FBlim.adjust<- c(    0,          0,          0,          0,          0,          1,          1,             1,          1,          0,          0)     # used for HCR2 to simulate escapement startegy for short lived sp
    my.FBlim.adjust3<-c(    1,          1,          1,          1,          1,          1,          1,             1,          1,          0,          0)     # used for HCR3

  } else if (KeyRunYear==2017) {
    #                     Cod     Whiting     Haddock      Saithe     Mackerel Herring     North-Sandeel South-Sandeel   Nor. pout       Sprat      Plaice        Sole 
    HCR1<-            c(    1,          1,          1,          1,       1,      1,          1,          1,             1,          1,          1,          1) 
    HCR2<-            c(    2,          2,          2,          2,       2,      2,         22,         22,            22,         22,          2,          2) 
    my.FBlim.adjust<- c(    0,          0,          0,          0,       0,      0,          1,          1,             1,          1,          0,          0)     # used for HCR2 to simulate escapement startegy for short lived sp
    my.FBlim.adjust3<-c(    1,          1,          1,          1,       1,      1,          1,          1,             1,          1,          0,          0)     # used for HCR3
  }  
    
} else if (my.area=='Baltic Sea') {
  #                     Cod       Herring        Sprat   
  HCR1<-            c(    1,          1,          1) 
  HCR2<-            c(    2,          2,          2)  
  my.FBlim.adjust<- c(    0,          0,          0)
  my.FBlim.adjust3<-c(    1,          1,          1)
}

local.HCR<-HCR1        
if (automatic.settings) {
 if (grepl("HCR_1",sce)) local.HCR<-HCR1
 if (grepl("HCR_2",sce)) local.HCR<-HCR2
}
if (automatic.settings) {
  cat('\nscenario name:',sce,'\n') 
  cat('\nrecruitmentMode is set to:',  recruitmentMode) 
  cat('\nrecruit.adjust.CV is set to:', recruit.adjust.CV) 
  cat('\nHCR is set to :', local.HCR,'\n') 
}



# defined later on    justAboveBlim<-T     # make plots only for Fcombinations where the risk to Blim <5%
# justAboveBpa<-F

fac.other.first.year<- -1  # first year for change (must be larger than first year in prediction)
fac.other.last.year<- -1  # last year for change
my.other<-rep(1,first.VPA-1);

FBlim.adjust<-my.FBlim.adjust  # F=0 at SSB at 0 (ICES default), except for short lived species  

riskLevels<-rep(5,nsp.VPA)  # Risk levels accepted for SSB below Blim, used in stepwise reduction
if (my.area=='North Sea') riskLevels<-rep(5,nsp.VPA-2) # forget plaice and sole

keepPro<-0.95  # keep F values which is factor keepPro of  MSY, used in stepwise reduction

# scenarios


##### fixed M species
if (F) {
  source(file.path(prog.path.func,"fmsy_matrix_functions_tables.r"))

 POK.PL.SOL(HCR=HCR1,stochastic.recruitment=recruitmentMode,recruit.adjust.CV=2)
  #POK.PL.SOL(HCR=HCR2,stochastic.recruitment=recruitmentMode,recruit.adjust.CV=1)
  #POK.PL.SOL(HCR=HCR2,FBlim.adjust=my.FBlim.adjust,stochastic.recruitment=recruitmentMode,recruit.adjust.CV=0)
}

# result from previous run                                                       
PLE.seq<-seq(0.20,0.20,0.05)
SOL.seq<-seq(0.35,0.35,0.05)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# defined later on    justAboveBlim<-T     # make plots only for Fcombinations where the risk to Blim <5%
# justAboveBpa<-F


FBlim.adjust<-my.FBlim.adjust  # F=0 at SSB at 0 (ICES default), except for short lived species  

################ 

my.OP.output=15  # default=14 for condensed output for  combinations of F 

##########################################


if  (my.area=='North Sea') {
  
  if (justDiagonal) { # single species combinations ("just the diagonal")
    targetFs<-NULL
    rr2<-rr
    for (a in (1:dim(rr)[[1]])) {
      rr<-rr2
      rr[,2]<-rr[,1]
      rr[a,]<-rr2[a,]
      if (KeyRunYear==2014) Fs<-expand.grid(COD=seq(rr['Cod',1],rr['Cod',2],rr['Cod',3]),
                      WHG=seq(rr['Whiting',1],rr['Whiting',2],rr['Whiting',3]),
                      HAD=seq(rr['Haddock',1],rr['Haddock',2],rr['Haddock',3]),
                      POK=seq(rr['Saithe',1],rr['Saithe',2],rr['Saithe',3]),
                      HER=seq(rr['Herring',1],rr['Herring',2],rr['Herring',3]),
                      NSA=seq(rr['N. sandeel',1],rr['N. sandeel',2],rr['N. sandeel',3]),
                      SSA=seq(rr['S. sandeel',1],rr['S. sandeel',2],rr['S. sandeel',3]),
                      NOR=seq(rr['Nor. pout',1],rr['Nor. pout',2],rr['Nor. pout',3]),
                      SPR=seq(rr['Sprat',1],rr['Sprat',2],rr['Sprat',3]),
                      PLE=seq(rr['Plaice',1],rr['Plaice',2],rr['Plaice',3]),
                      SOL=seq(rr['Sole',1],rr['Sole',2],rr['Sole',3]))
      if (KeyRunYear==2017) Fs<-expand.grid(COD=seq(rr['Cod',1],rr['Cod',2],rr['Cod',3]),
                                            WHG=seq(rr['Whiting',1],rr['Whiting',2],rr['Whiting',3]),
                                            HAD=seq(rr['Haddock',1],rr['Haddock',2],rr['Haddock',3]),
                                            POK=seq(rr['Saithe',1],rr['Saithe',2],rr['Saithe',3]),
                                            MAC=seq(rr['Mackerel',1],rr['Mackerel',2],rr['Mackerel',3]),
                                            HER=seq(rr['Herring',1],rr['Herring',2],rr['Herring',3]),
                                            NSA=seq(rr['N. sandeel',1],rr['N. sandeel',2],rr['N. sandeel',3]),
                                            SSA=seq(rr['S. sandeel',1],rr['S. sandeel',2],rr['S. sandeel',3]),
                                            NOR=seq(rr['Nor. pout',1],rr['Nor. pout',2],rr['Nor. pout',3]),
                                            SPR=seq(rr['Sprat',1],rr['Sprat',2],rr['Sprat',3]),
                                            PLE=seq(rr['Plaice',1],rr['Plaice',2],rr['Plaice',3]),
                                            SOL=seq(rr['Sole',1],rr['Sole',2],rr['Sole',3]))
      
      targetFs<-rbind(targetFs,Fs)
  }}
  if (!justDiagonal) {
    if (KeyRunYear==2014) targetFs<-expand.grid(COD=seq(rr['Cod',1],rr['Cod',2],rr['Cod',3]),
                                                WHG=seq(rr['Whiting',1],rr['Whiting',2],rr['Whiting',3]),
                                                HAD=seq(rr['Haddock',1],rr['Haddock',2],rr['Haddock',3]),
                                                POK=seq(rr['Saithe',1],rr['Saithe',2],rr['Saithe',3]),
                                                HER=seq(rr['Herring',1],rr['Herring',2],rr['Herring',3]),
                                                NSA=seq(rr['N. sandeel',1],rr['N. sandeel',2],rr['N. sandeel',3]),
                                                SSA=seq(rr['S. sandeel',1],rr['S. sandeel',2],rr['S. sandeel',3]),
                                                NOR=seq(rr['Nor. pout',1],rr['Nor. pout',2],rr['Nor. pout',3]),
                                                SPR=seq(rr['Sprat',1],rr['Sprat',2],rr['Sprat',3]),
                                                PLE=seq(rr['Plaice',1],rr['Plaice',2],rr['Plaice',3]),
                                                SOL=seq(rr['Sole',1],rr['Sole',2],rr['Sole',3])) 
    if (KeyRunYear==2017) targetFs<-expand.grid(COD=seq(rr['Cod',1],rr['Cod',2],rr['Cod',3]),
                                                WHG=seq(rr['Whiting',1],rr['Whiting',2],rr['Whiting',3]),
                                                HAD=seq(rr['Haddock',1],rr['Haddock',2],rr['Haddock',3]),
                                                POK=seq(rr['Saithe',1],rr['Saithe',2],rr['Saithe',3]),
                                                MAC=seq(rr['Mackerel',1],rr['Mackerel',2],rr['Mackerel',3]),
                                                HER=seq(rr['Herring',1],rr['Herring',2],rr['Herring',3]),
                                                NSA=seq(rr['N. sandeel',1],rr['N. sandeel',2],rr['N. sandeel',3]),
                                                SSA=seq(rr['S. sandeel',1],rr['S. sandeel',2],rr['S. sandeel',3]),
                                                NOR=seq(rr['Nor. pout',1],rr['Nor. pout',2],rr['Nor. pout',3]),
                                                SPR=seq(rr['Sprat',1],rr['Sprat',2],rr['Sprat',3]),
                                                PLE=seq(rr['Plaice',1],rr['Plaice',2],rr['Plaice',3]),
                                                SOL=seq(rr['Sole',1],rr['Sole',2],rr['Sole',3]))  
  } 
    

} else if  (my.area=='Baltic Sea') {
   targetFs<-expand.grid(COD=seq(rr['Cod',1],rr['Cod',2],rr['Cod',3]),
                       HER=seq(rr['Herring',1],rr['Herring',2],rr['Herring',3]),
                       SPR=seq(rr['Sprat',1],rr['Sprat',2],rr['Sprat',3]))
}

 #just checking
cat('\nScenarium:',sce,'\n')
cat('Area:',my.area,'\n')
cat('\nrecruitmentMode:',recruitmentMode,'\n')
dim(targetFs)


 if (recruitmentMode %in% c(0,20)) { # deterministic recruitment
    my.last.year<-2040; years.in.average<-24;     
    nSim<-dim(targetFs)[[1]]
    nRuns<-1 # number of  batch run
    no.iters<-rep(1,nRuns)            # number of iterations in each batch run
    first.iter<-no.iters
    seqRuns<-round(seq(0,nSim,len=nRuns+1))
    first.run.no<-seqRuns[1:(length(seqRuns)-1)]+1
    cat("First run no in each run:",first.run.no,'\n')
    fromTo<-matrix(c(first.run.no,seqRuns[-1]),nrow=2,byrow=T)
} else  if (recruitmentMode>0) {   #stochastic recruitment 
  if (justDiagonal) {years.in.average<-1; my.last.year<-2045; }
  else {years.in.average<-100; my.last.year<-2014+years.in.average+10;}
  
  if (OS=='windows') {
       kernels<-4  # number of CPU kernels
       nrep<-2     # number of iteration for each kernel
       if (justDiagonal) nrep<-125
   } else if (OS=='unix') {
      kernels<-10
      nrep<-1
   } 
   no.iters<-rep(nrep,kernels)  # number of iterations in each batch run by CPUE kernel 
   no.iter.stoch<-sum(no.iters)
   nRuns<-length(no.iters)
   first.run.no<-rep(1,nRuns)
   first.iter<-no.iters
   first.iter[1]<-1
   if (nRuns>1) for (i in (2:nRuns)) first.iter[i]<-round(first.iter[i-1]+no.iters[i-1])
   cat("Number of kernels used (number og runs):",kernels,"\nNumber of iterations:",no.iter.stoch,"\nFirst iteration no in each run:", first.iter,'\n')
   fromTo<-matrix(c(rep(1,nRuns),rep(dim(targetFs)[[1]],nRuns)),nrow=2,byrow=T)
}

if (make.new.run) {    
  scenario.gem<-NULL 
  scenario.gemmes<-NULL
  
  # run in batch mode

    for (i in (1:nRuns)) {
      NO<-formatC(i,w=2,flag="0")
      OP.simulate(scenario=paste(sce,NO,sep='_'),first.no.iter.stoch=first.iter[i],first.run.no=first.run.no[i],no.iter.stoch=no.iters[i],just.batch.file=T,HCR=local.HCR,targetFs=targetFs[fromTo[1,i]:fromTo[2,i],],FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=recruitmentMode,recruit.adjust.CV=recruit.adjust.CV,recruit.adjust.factor=recruit.adjust.factor,OP.output=my.OP.output,do.indicators=do.MSFD.indicators,fac.other.first.year=fac.other.first.year,fac.other.last.year=fac.other.last.year,fac.other=my.other)
      scenario.gemmes<-c(scenario.gemmes,scenario.gem)
    }
        
    save(scenario.gemmes, file =file.path(data.path, paste(sce,'scenarios.RData',sep='_')))
  
    if (HPC) {  #make the script that runs all the sub-scripts
     cat(paste(
          "#!/bin/sh",
          "# embedded options to qsub - start with #PBS",
          "# -- Name of the job ---",
          "#PBS -N OP_BATCH",
          "# –- specify queue --",
          "#PBS -q hpc",
          "# -- estimated wall clock time (execution time): hh:mm:ss --",
          "#PBS -l walltime=00:05:00",
          "# –- user email address --",
          "#PBS -M mv@aqua.dtu.dk",
          "# –- mail notification –-",
          "#PBS -m abe", "\n", sep='\n'),
          file=file.path(data.path,'OP_batch.sh')) 
       for (i in  scenario.gemmes) {
        cat(paste(        
            paste("chmod +rwx",file.path(data.path,i,'run_OP.sh')),
            paste("qsub",file.path(data.path,i,'run_OP.sh')),"# --\n", 
            sep='\n'),append=T,file=file.path(data.path,'OP_batch.sh'))
       }
      stop("\nPlease note: you have to run (qsub) the shell script ",file.path(data.path,'OP_batch.sh'), " and wait for its completion \n")
          
     } else if (OS=='windows') { 
        for (i in  scenario.gemmes) {
          cat('Command:',file.path(data.path,i,"run_OP.bat"),'has been submitted, wait for its completion\n')
          system2(command=file.path(data.path,i,"run_OP.bat"), wait = F,stdout = file.path(data.path,i,"Run_OP_out.dat")) 
          
        }    
        stop('\nPlease note: you have to wait runing the rest of the script, until the bach runs have completed.\nCheck the "Windows Task Manager", "CPUE usage" for completion\n')       
     } 

} else { # "old" run exist already
  load(file =file.path(data.path, paste(sce,'scenarios.RData',sep='_')),verbose = T)
}
#############################
 
if (FALSE) { # used to subset data befor further processing. Mainly used with very large datasets 
   scenario<-paste(sce,'01',paste("HCR",local.HCR[1],sep=''),FBlim.adjust[1],paste("Rec",recruitmentMode,sep=''),ifelse(recruit.adjust.CV[1]==1 | recruit.adjust.CV[1]==2,"Recadj",""),my.last.year,sep='_')
   scenario.dir<-file.path(data.path,scenario)
   load(file =file.path(scenario.dir, "condensed.RData"),verbose=T)
   # op 2 condensed<-droplevels(subset(condensed,COD>=0.5 & WHG==0.15 & HAD>=0.55 &  HER>=0.35 & NSA>=0.55 & SSA>=0.35 & NOR>=0.45 & SPR>=0-65 ))
   condensed<-droplevels(subset(condensed,COD==0.3 & WHG==0.1 & HAD>=0.55 &  HER>=0.35 & (NSA==0.50 | NSA==0.55) & SSA>=0.35  & SSA<=0.45 & NOR>=0.45 & NOR<=0.5 & SPR>=0-65 ))
   save(condensed, file =file.path(scenario.dir, "condensed.RData"))
   rm(condensed)
   
   load(file =file.path(scenario.dir, "a.RData"),verbose=T)
   # op2 a<-droplevels(subset(a,COD>=0.5 & WHG==0.15 & HAD>=0.55 &  HER>=0.35 & NSA>=0.55 & SSA>=0.35 & NOR>=0.45 & SPR>=0-65 ))
    a<-droplevels(subset(a,COD==0.3 & WHG==0.1 & HAD>=0.55 &  HER>=0.35 & (NSA==0.50 | NSA==0.55) & SSA>=0.35  & SSA<=0.45 & NOR>=0.45 & NOR<=0.5 & SPR>=0-65 ))
   save(a, file =file.path(scenario.dir, "a.RData"));
   rm(a)
   
   load(file =file.path(scenario.dir, "indicators.RData"),verbose=T)
   dim(indi)
   # op2 indi<-droplevels(subset(indi,COD>=0.5 & WHG==0.15 & HAD>=0.55 &  HER>=0.35 & NSA>=0.55 & SSA>=0.35 & NOR>=0.45 & SPR>=0-65 ))
   indi<-droplevels(subset(indi,COD==0.3 & WHG==0.1 & HAD>=0.55 &  HER>=0.35 & (NSA==0.50 | NSA==0.55) & SSA>=0.35  & SSA<=0.45 & NOR>=0.45 & NOR<=0.5 & SPR>=0-65 ))
   dim(indi)
   save(indi, file =file.path(scenario.dir, "indicators.RData"))
}
  
  # read results into R data sets
 for (i in (1:nRuns)) {
    NO<-formatC(i,w=2,flag="0")
    OP.simulate(scenario=paste(sce,NO,sep='_'),first.run.no=first.run.no[i],first.no.iter.stoch=first.iter[i],targetFs=targetFs[fromTo[1,i]:fromTo[2,i],],do.simulation=F, read.condense=T,read.indicators=T, do.plot.indicators=F, do.plot.condense=F,HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=recruitmentMode,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV)
  }

  
  # combine into one run  
  justAboveBlim<-F  # do also include plot for just combinations with <5% risk to Blim for any species in the combination
  justAboveBpa<-F
  OP.simulate(scenario=paste(sce,'01',sep='_'),targetFs=targetFs,do.simulation=F,read.condense=F,read.indicators=F, do.plot.indicators=F, do.plot.condense=F,combine.scenario.run=T,ReduceTable=F,riskLevels=riskLevels,keepPro=keepPro,scenario.dirs=scenario.gemmes, scenario.dirs.out=scenario.gemmes[1], HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=recruitmentMode,justAboveBlim=justAboveBlim,justAboveBpa=justAboveBpa,recruit.adjust.CV=recruit.adjust.CV)

 
   # prepare to make plots, select potential subset  
   if  (my.area=='North Sea') {
  
      if (TRUE) { # reduction 1
        
        if (KeyRunYear==2014) {
                           #COD    WHG   HAD  POK  HER  NSAN   SSAN   NOR    SPR  PLE  SOL 
        my.Frange<-matrix(c(0.0,   0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0, 0.0, 0.0,     # min
                            2.0,  0.10, 2.0, 2.0, 2.0, 2.0,   2.0,   2.0  , 2.0, 2.0, 2.0),     # max
                            ncol=11,byrow=T)  # min and max F for inclusion in plots, remember to set option subset.out below
        }
        if (KeyRunYear==2017) {
                             #COD    WHG   HAD  POK  MAC  HER  NSAN   SSAN   NOR    SPR  PLE  SOL 
          my.Frange<-matrix(c(0.0,   0.0, 0.0, 0.0, 0.0, 0.0, 0.0,   0.0,   0.0,   0.0, 0.0, 0.0,     # min
                              2.0,  0.10, 2.0, 2.0, 2.0, 2.0, 2.0,   2.0,   2.0  , 2.0, 2.0, 2.0),     # max
                            ncol=12,byrow=T)  # min and max F for inclusion in plots, remember to set option subset.out below
        }
        
        
        
      }
 
     
    }  else if  (my.area=='Baltic Sea') {
                         #COD   HER  SPR        DOES NOT WORK YET for the BAltic
      my.Frange<-matrix(c(0.0,  0.0, 0.0,      # min
                          2.0,  2.0, 2.0 ),     # max
                          ncol=3,byrow=T)  # min and max F for inclusion in plots, remember to set option subset.out below
    }
  
  source(file.path(prog.path.func,"fmsy_matrix_functions_tables.r"))

  my.dev<-'png' 
  
  library(gtools)
  #make plots
  my.subset.out<-F  # use subset of data  specified above
  my.ReduceTable<-F
  my.do.plot.condense<-T; my.do.plot.indicators<-T & do.MSFD.indicators; 
  OP.simulate(singleSp=justDiagonal, scenario=paste(sce,'01',sep='_'),subset.out=my.subset.out,subset.species=my.Frange,my.dev=my.dev,targetFs=targetFs,do.simulation=F, read.condense=F,read.indicators=F,do.plot.indicators=my.do.plot.indicators,do.plot.condense=my.do.plot.condense,ReduceTable=my.ReduceTable,riskLevels=riskLevels,keepPro=keepPro,HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=recruitmentMode,no.iter.stoch=no.iter.stoch,justAboveBlim=justAboveBlim,justAboveBpa=justAboveBpa,recruit.adjust.CV=recruit.adjust.CV)


 
  ##################
  
  #Reduce tables,  default output
  #OP.simulate(scenario=paste(sce,'01',sep='_'),subset.out=F,subset.species=my.Frange,my.dev=my.dev,targetFs=targetFs,do.simulation=F, read.condense=F,read.indicators=F,do.plot.indicators=FALSE,do.plot.condense=FALSE,ReduceTable=TRUE,ReduceTableDetails=F,riskLevels=riskLevels,keepPro=keepPro,HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=recruitmentMode,no.iter.stoch=no.iter.stoch,justAboveBlim=justAboveBlim,justAboveBpa=justAboveBpa,recruit.adjust.CV=recruit.adjust.CV)

  ##################
if (FALSE) {  
  #Reduce tables option12&3 and 4
  riskLevels<-rep(0.0,nsp.VPA-2) # risk level (%), -2 to forget plaice and sole
  keepPro<-0.95  # keep F values which is factor keepPro of  MSY, used in stepwise reduction
  saveOutput<-TRUE
  OP.simulate(scenario=paste(sce,'01',sep='_'),subset.out=F,subset.species=my.Frange,my.dev=my.dev,targetFs=targetFs,do.simulation=F, read.condense=F,read.indicators=F,do.plot.indicators=FALSE,do.plot.condense=FALSE,ReduceTable=TRUE,ReduceTableDetails=T,ReduceOption='option12', saveOutput=saveOutput,riskLevels=riskLevels,keepPro=keepPro,HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=recruitmentMode,no.iter.stoch=no.iter.stoch,justAboveBlim=justAboveBlim,justAboveBpa=justAboveBpa,recruit.adjust.CV=recruit.adjust.CV)
}  

 # load(file =file.path(scenario.dir, "option4.RData"),verbose=T)
 # load(file =file.path(scenario.dir, "indicators.RData"),verbose=T)
 # load(file =file.path(scenario.dir, "indi1.RData"),verbose=T)
#  names(indiNew)

  
if (FALSE) {  
  load(file =file.path(scenario.dir.simul, "condensed.RData")) 
  a<-condensed
  a<-a[order(paste(formatC(a$Species.n,flag="0",w=3), formatC(a$run,flag="0",w=3),formatC(a$iteration,flag="0",w=3))),]
  head(a,20)
  
  res<-matrix(a$yield,ncol=10,nrow=11,byrow=F) 
  dimnames(res)<-list(c('start',paste('F',sp.names[17:26],sep='_')),paste('yield',sp.names[17:26],sep='_'))
  res<-t(res)
  res
  
  avg<-res[,1] 
  res.delta<-res
  
  
  for (r in (2:11)) res.delta[,r]<-res.delta[,r]-res.delta[,1]
  
  res.delta<-res.delta[,-1]
  res.delta<-res.delta /delta   # denne skal ligne admodel.dep filen
  
  cat("\nres.delta=admodel.dep\n")
  print(round(res.delta))    # svarende til en ?gning i F p? 1.0
  
  change.val<-0.10   # ?ndring p? 10%
  Fval.change<-Fval *change.val
  
  res.delta.change<-res.delta*Fval.change
  
  cat("\n relativ change of yield in percentage of a ",change.val*100,"% of species F (first line)\n")
  print(round(Fval,2)) 
  print(round(100*res.delta.change/avg,1))      # relative change

}

  