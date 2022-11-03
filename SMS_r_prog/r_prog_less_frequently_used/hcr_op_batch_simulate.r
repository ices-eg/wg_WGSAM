
memory.limit(size = 85000)

############################
## North Sea or Baltic Sea runs
my.area<-c('North Sea','Baltic Sea')[2]   

##########  Choose arae (above) and one of the following scenarios
 scens<-c('HCR_1_deter_noadjust_wide',    # 1 Fixed F (HCR=1),  wide F range and large F steps, and NO recruitment median to mean adjustment 
          'HCR_1_deter_adjust_wide',      # 2 Fixed F (HCR=1),  wide F range and large F steps, and recruitment median to mean adjustment 
          'HCR_1_deter_noadjust_narrow',  # 3 Fixed F (HCR=1), narrow F range, and NO recruitment median to mean adjustment 
          'HCR_1_deter_adjust_narrow',    # 4 Fixed F (HCR=1), narrow F range, and  recruitment median to mean adjustment 

          'HCR_1_stoch_wide',             # 5 
          'HCR_1_stoch_narrow1',          # 6
          'HCR_1_stoch_narrow2',          # 7
          'HCR_1_stoch_narrow3',          # 8  
          'HCR_2_stoch_wide',             # 9 sidste North Sea kørsel før sommerferien 
          'HCR_2_stoch_narrow1',          # 10
          'HCR_2_stoch_narrow2',          # 11
          'HCR_2_stoch_narrow3')          # 12
 

sce<-scens[1]        # Make your choice


##################################################################################

automatic.settings<-T   # set options automaticacly form sceenario name (sce) 

# Stochastic recruitment.
# 0=No,  S-R parameters from OP_config.dat
# 1=Yes  S-R parameters and variance from OP_config.dat
# 2=Yes, S-R parameters from OP_config.dat and variance-covariance matrix from file covariance_Rec.in
# 3=Yes, S-R parameters from OP_config.dat and residuals from OP_SSB_rec_residuals.in

stochastic.recruitment<-1
if (automatic.settings) {
 if (grepl('_stoch_',sce)) stochastic.recruitment<-3 else if (grepl('_deter_',sce)) stochastic.recruitment<-0
}


# adjust recruitment with half of the variance (factor exp(-(CV^2)/2) option adjust.recruit.CV
# 0=no adjustment\n",
# 1=stochastic recruitment, do adjustment such that average recruitment becomes equal to the median (downward adjustment)\n",
# 2=determenistic recruitment, do ajustment such that recruitment becomes equal to the mean of the log-normal distribution (upward adjustment)\n", file=file,append=T,sep="")
recruit.adjust.CV<-0        
if (automatic.settings) {
 if (grepl("_noadjust_",sce)) recruit.adjust.CV<-0
 if (grepl("_adjust_",sce)) {
   if (stochastic.recruitment==0) recruit.adjust.CV<-2 else  recruit.adjust.CV<-1
 }
}


source(file=file.path(prog.path,'HCR_OP_batch_simulate_function.R'))
 
if (my.area=='North Sea') { 
  #                     Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole 
  HCR1<-            c(    1,          1,          1,          1,          1,          1,          1,          1,          1,          1) 
  HCR2<-            c(    2,          2,          2,          2,          2,         22,         22,         22,          2,          2) 
  my.FBlim.adjust<- c(    0,          0,          0,          0,          0,          1,          1,          1,          0,          0)     # used for HCR2 to simulate escapement startegy for short lived sp
  my.FBlim.adjust3<-c(    1,          1,          1,          1,          1,          1,          1,          1,          0,          0)     # used for HCR3
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
  cat('\nstochastic.recruitment is set to:',  stochastic.recruitment,' from scenario name:',sce,'\n') 
  cat('\nrecruit.adjust.CV is set to:', recruit.adjust.CV,' from scenario name:',sce,'\n') 
  cat('\nHCR is set to :', local.HCR,' from scenario name:',sce,'\n') 
}

ask.me<-F  # you ca be asked to confirm a run

if (stochastic.recruitment==0) { 
  my.last.year<-2051; years.in.average<-2;   no.iters<-1 
} else {
  my.last.year<-2111; years.in.average<-100; 
  no.iters<-c(2,2,2,2,2)  # number of iterations in each batch run by CPUE kernel (max 5 kernels)
}
no.iter.stoch<-sum(no.iters)


# scenarios


##### fixed M species
if (F) {
  POK.PL.SOL(HCR=HCR1,stochastic.recruitment=stochastic.recruitment)
  POK.PL.SOL(HCR=HCR2,stochastic.recruitment=stochastic.recruitment)
  POK.PL.SOL(HCR=HCR2,FBlim.adjust=my.FBlim.adjust,stochastic.recruitment=stochastic.recruitment)
}

# result from previous run                                                       
PLE.seq<-seq(0.30,0.30,0.05)
SOL.seq<-seq(0.40,0.40,0.05)

step.0<-0.1
step.1<-0.05
step.2<-0.025

POK.seq<-seq(0.60,0.60,step.0)    # FMSY is around F=0.35, however ...      


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# defined later on    justAboveBlim<-T     # make plots only for Fcombinations where the risk to Blim <5%
# justAboveBpa<-F



FBlim.adjust<-my.FBlim.adjust  # F=0 at SSB at 0 (ICES default), except for short lived species  

if (my.area=='North Sea') { riskLevels<-c(5,5,5,5,5,5,5,5) # Risk levels accepted for SSB below Blim, used in stepwise reduction
} else if (my.area=='Baltic Sea') riskLevels<-c(5,5,5) # Risk levels accepted for SSB below Blim, used in stepwise reduction
keepPro<-0.95  # keep F values which is factor keepPro of  MSY, used in stepwise reduction

 ################ 


##### fixed M species
if (F) {
  no.iter.stoch<-1
  POK.PL.SOL(HCR=HCR1,stochastic.recruitment=stochastic.recruitment,recruit.adjust.CV=0)
  POK.PL.SOL(HCR=HCR1,stochastic.recruitment=stochastic.recruitment,recruit.adjust.CV=2)
  no.iter.stoch<-10
}

##
if (sce=='HCR_1_deter_noadjust_wide' | sce=='HCR_1_deter_adjust_wide') {
  if  (my.area=='North Sea') {
    step.0<-0.1
    rr<-matrix(c(
    0.3, 0.6, step.0,     #  COD   There is made one "kernel" run for each cod F, (do not use more than 5 F values for cod)
    0.1, 0.5, step.0,     # WHG
    0.2, 0.6, step.0,     #  HAD
    0.3, 0.4, step.0,     # POK
    0.1, 0.4, step.0,     #  HER
    0.2, 0.6, step.0,     # SAN
    0.2, 0.6, step.0,     #  NOP
    0.3, 1.0, step.0,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
    dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  }
  if  (my.area=='Baltic Sea') {
    step.0<-0.05
    rr<-matrix(c(
    0.2, 0.65, step.0,     #  COD   
    0.1, 0.4, step.0,     #  HER
    0.1, 0.55, step.0),     # SPR
    ,ncol=3,byrow=T)
    dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  }
}


################  stochastic rec, Constant F  (equal to HCR=1 in paper)
if (sce=='HCR_1_stoch_wide' ) {
  if  (my.area=='North Sea') {
    step.0<-0.1
    rr<-matrix(c(
    0.3, 0.6, step.0,     #  COD   
    0.1, 0.5, step.0,     # WHG
    0.1, 0.5, step.0,     #  HAD
    0.3, 0.4, step.0,     # POK
    0.1, 0.4, step.0,     #  HER
    0.0, 0.6, step.0,     # SAN
    0.0, 0.6, step.0,     #  NOP
    0.0, 1.0, step.0,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
    dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  }
  if  (my.area=='Baltic Sea') {
    step.0<-0.05
    rr<-matrix(c(
    0.2, 0.7, step.0,     #  COD   
    0.1, 0.4, step.0,     #  HER
    0.1, 0.6, step.0),     # SPR
    ,ncol=3,byrow=T)
    dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  }
}


###






## HERTIL ER DER RETTET

if (sce=='HCR_2_deter_noadjust_wide' | sce=='HCR_2_deter_adjust_wide') {
  step.0<-0.1
  rr<-matrix(c(
  0.3, 0.6, step.0,     #  COD   There is made one "kernel" run for each cod F, (do not use more than 5 F values for cod)
  0.1, 0.5, step.0,     # WHG
  0.2, 0.6, step.0,     #  HAD
  0.3, 0.4, step.0,     # POK
  0.1, 0.4, step.0,     #  HER
  0.2, 0.6, step.0,     # SAN
  0.2, 0.6, step.0,     #  NOP
  0.3, 1.0, step.0,     # SPR
  PLE.seq, PLE.seq, 0,  #  PLE
  SOL.seq, SOL.seq, 0)  #  SOL
  ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  if (sce=='HCR_2_deter_noadjust_wide')  recruit.adjust.CV<-0  else  recruit.adjust.CV<-2
}
###


if (sce=='HCR_1_deter_noadjust_narrow' ) {
  local.HCR<-HCR1 
  ss<-0.025 
  #ss<-0.05
  rr<-matrix(c(
  0.40, 0.50, ss,     #  COD   There is made one "kernel" run for each cod F, (do not use more than 5 F values for cod)
  0.20, 0.30, ss,     # WHG
  0.15, 0.25, ss,     #  HAD
  0.35, 0.40, ss,     # POK
  0.3, 0.35, ss,     #  HER
  0.30, 0.40, ss,     # SAN
  0.55, 0.6, ss,     #  NOP
  0.8, 1.0, ss,     # SPR
  PLE.seq, PLE.seq, 0,  #  PLE
  SOL.seq, SOL.seq, 0)  #  SOL
  ,ncol=3,byrow=T)

  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  recruit.adjust.CV<-0  }

if (sce=='HCR_1_deter_adjust_narrow') {
  local.HCR<-HCR1 
  ss<-0.025 
  ##ss<-0.05 
  rr<-matrix(c(
  0.40, 0.50, ss,     #  COD   There is made one "kernel" run for each cod F, (do not use more than 5 F values for cod)
  0.20, 0.40, ss,     # WHG
  0.20, 0.40, ss,     #  HAD
  0.35, 0.40, ss,     # POK
  0.3, 0.35, ss,     #  HER
  0.30, 0.40, ss,     # SAN
  0.55, 0.6, ss,     #  NOP
  0.8, 1.0, ss,     # SPR
  PLE.seq, PLE.seq, 0,  #  PLE
  SOL.seq, SOL.seq, 0)  #  SOL
  ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  recruit.adjust.CV<-2
}

 ################ stochastic rec ##############################


################  TEST stochastic rec, Constant F  (equal to HCR=1 in paper)
if (sce=='HCR_1_stoch_test' ) {
  local.HCR<-HCR1
  step.0<-0.1
  rr<-matrix(c(
  0.3, 0.6, step.0,     #  COD   
  0.1, 0.1, step.0,     # WHG
  0.1, 0.2, step.0,     #  HAD
  0.4, 0.4, step.0,     # POK
  0.3, 0.4, step.0,     #  HER
  0.0, 0.1, step.0,     # SAN
  0.5, 0.6, step.0,     #  NOP
  0.8, 1.0, step.0,     # SPR
  PLE.seq, PLE.seq, 0,  #  PLE
  SOL.seq, SOL.seq, 0)  #  SOL
  ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}



################  stochastic rec, Constant F  (equal to HCR=2 in paper)
if (sce=='HCR_2_stoch_wide') {
  local.HCR<-HCR2
  step.0<-0.1
  rr<-matrix(c(
  0.3, 0.6, step.0,     #  COD   
  0.1, 0.5, step.0,     # WHG
  0.1, 0.5, step.0,     #  HAD
  0.3, 0.4, step.0,     # POK
  0.1, 0.4, step.0,     #  HER
  0.0, 0.6, step.0,     # SAN
  0.0, 0.6, step.0,     #  NOP
  0.0, 1.0, step.0,     # SPR
  PLE.seq, PLE.seq, 0,  #  PLE
  SOL.seq, SOL.seq, 0)  #  SOL
  ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}

if (sce=='HCR_3_stoch_wide') {
 local.HCR<-HCR2      # it should be HCR2 even though it is rule 3 (defined fy FBlim.adjust)
  FBlim.adjust<-my.FBlim.adjust3 
  step.0<-0.1
  rr<-matrix(c(
  0.3, 0.6, step.0,     #  COD   
  0.1, 0.5, step.0,     # WHG
  0.2, 0.6, step.0,     #  HAD
  0.3, 0.4, step.0,     # POK
  0.1, 0.4, step.0,     #  HER
  0.2, 0.6, step.0,     # SAN
  0.2, 0.6, step.0,     #  NOP
  0.3, 1.0, step.0,     # SPR
  PLE.seq, PLE.seq, 0,  #  PLE
  SOL.seq, SOL.seq, 0)  #  SOL
  ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}




if  (my.area=='North Sea') { 
  targetFs<-expand.grid(COD=seq(rr['Cod',1],rr['Cod',2],rr['Cod',3]),
                      WHG=seq(rr['Whiting',1],rr['Whiting',2],rr['Whiting',3]),
                      HAD=seq(rr['Haddock',1],rr['Haddock',2],rr['Haddock',3]),
                      POK=seq(rr['Saithe',1],rr['Saithe',2],rr['Saithe',3]),
                      HER=seq(rr['Herring',1],rr['Herring',2],rr['Herring',3]),
                      SAN=seq(rr['Sandeel',1],rr['Sandeel',2],rr['Sandeel',3]),
                      NOR=seq(rr['Nor. pout',1],rr['Nor. pout',2],rr['Nor. pout',3]),
                      SPR=seq(rr['Sprat',1],rr['Sprat',2],rr['Sprat',3]),
                      PLE=seq(rr['Plaice',1],rr['Plaice',2],rr['Plaice',3]),
                      SOL=seq(rr['Sole',1],rr['Sole',2],rr['Sole',3]))

} else if  (my.area=='Baltic Sea') {
   targetFs<-expand.grid(COD=seq(rr['Cod',1],rr['Cod',2],rr['Cod',3]),
                       HER=seq(rr['Herring',1],rr['Herring',2],rr['Herring',3]),
                       SPR=seq(rr['Sprat',1],rr['Sprat',2],rr['Sprat',3]))
}

 #just checking
cat('\nScenarium:',sce,'\n')
cat('Area:',my.area,'\n')
cat('\nstochastic.recruitment:',stochastic.recruitment,'\n')
 dim(targetFs)

if (stochastic.recruitment==0) {       # determenistic runs
    nSim<-dim(targetFs)[[1]]
    nRuns<-5
    dim(targetFs)[[1]]/nRuns/8000    # no of hours to run
     # number of iterations in each batch run
    no.iters<-rep(1,nRuns)            # number of iterations in each batch run
    seqRuns<-seq(0,nSim,len=nRuns+1)
    first.iter<-seqRuns[1:(length(seqRuns)-1)]+1
    first.iter  # first run no in each run
} else  if (stochastic.recruitment>0) {   #stochastic runs
  
   if (no.iter.stoch != sum(no.iters)) cat("\nWarning the sum of iterations (=",sum(no.iters),") is not equal to the default number of iterations (=",no.iter.stoch,") !\n")
    nRuns<-length(no.iters)
    first.iter<-no.iters
    first.iter[1]<-1
    if (nRuns>1) for (i in (2:nRuns)) first.iter[i]<-first.iter[i-1]+no.iters[i-1]
     first.iter; 
     no.iters
}
   
  scenario.gem<-NULL 
  scenario.gemmes<-NULL
  
  # run in batch mode
  for (i in (1:nRuns)) {
    NO<-formatC(i,w=2,flag="0")
    OP.simulate(scenario=paste(sce,NO,sep='_'),first.no.iter.stoch=first.iter[i],no.iter.stoch=no.iters[i],just.batch.file=T,HCR=local.HCR,targetFs=targetFs,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,recruit.adjust.CV=recruit.adjust.CV)
    scenario.gemmes<-c(scenario.gemmes,scenario.gem)
  }
  save(scenario.gemmes, file =file.path(data.path, paste(sce,'scenarios.RData',sep='_')))
  stop('\nPlease note: you have to wait runing the rest of the script, until the bach runs have completed\n')
  
  
  if (F) {         # if something went wrong, or to re-start
    load( file =file.path(data.path, paste(sce,'scenarios.RData',sep='_')))  
    nRuns<-2 # reduce the number of runs to fit memory limitations
    scenario.gemmes<-scenario.gemmes[1:nRuns]
  }
  
  
  # read results into R data sets
 for (i in (1:nRuns)) {
    NO<-formatC(i,w=2,flag="0")
    OP.simulate(scenario=paste(sce,NO,sep='_'),first.run.no=first.iter[i],targetFs=targetFs,do.simulation=F, read.condense=T,read.indicators=T, do.plot.indicators=F, do.plot.condense=F,HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV)
  }
  
  
  # combine into one run  
  justAboveBlim<-F  # do also include plot for just combinations with <5% risk to Blim for any species in the combination
  justAboveBpa<-F
  OP.simulate(scenario=paste(sce,'01',sep='_'),targetFs=targetFs,do.simulation=F,read.condense=F,read.indicators=F, do.plot.indicators=F, do.plot.condense=F,combine.scenario.run=T,ReduceTable=F,riskLevels=riskLevels,keepPro=keepPro,scenario.dirs=scenario.gemmes, scenario.dirs.out=scenario.gemmes[1], HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,justAboveBlim=justAboveBlim,justAboveBpa=justAboveBpa,recruit.adjust.CV=recruit.adjust.CV)


  if (F)  {  # just to repair form memory overload
     tmpDir<-"C:/users/movi/sms/NS_63-10-Nov-2013/HCR_1_stoch_wide_01_HCR1_1_Rec3__2111"
     load(file =file.path(tmpDir, "condensed.RData"))
     source(file.path(prog.path,"HCR_OP_condense_function.r"))
     a<-transform.condensed(a=condensed,my.area=my.area)
     cat("\nStarts writing set  a\n")
     save(a, file =file.path(tmpDir, "a.RData"));  
  }
  
  
   # make plots 
   if  (my.area=='North Sea') {
                         #COD   WHG  HAD  POK  HER  SAN  NOR  SPR  PLE  SOL 
      my.Frange<-matrix(c(0.0,  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     # min
                          2.0,  2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),     # max
                          ncol=10,byrow=T)  # min and max F for inclusion in plots, remember to set option subset.out below
    }  else if  (my.area=='Baltic Sea') {
                         #COD   HER  SPR        DOES NOT WORK YET for the BAltic
      my.Frange<-matrix(c(0.0,  0.0, 0.0,      # min
                          2.0,  2.0, 2.0 ),     # max
                          ncol=3,byrow=T)  # min and max F for inclusion in plots, remember to set option subset.out below
    }
  
  my.dev<-'png' # for Baltic Sea
  OP.simulate(scenario=paste(sce,'01',sep='_'),subset.out=F,subset.species=my.Frange,my.dev=my.dev,targetFs=targetFs,do.simulation=F, read.condense=F,read.indicators=F,do.plot.indicators=T,do.plot.condense=T,ReduceTable=T,riskLevels=riskLevels,keepPro=keepPro,HCR=local.HCR,FBlim.adjust=FBlim.adjust,ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,justAboveBlim=justAboveBlim,justAboveBpa=justAboveBpa,,recruit.adjust.CV=recruit.adjust.CV)
  ##################



if (F) {  
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
  print(round(res.delta))    # svarende til en øgning i F på 1.0
  
  change.val<-0.10   # ændring på 10%
  Fval.change<-Fval *change.val
  
  res.delta.change<-res.delta*Fval.change
  
  cat("\n relativ change of yield in percentage of a ",change.val*100,"% of species F (first line)\n")
  print(round(Fval,2)) 
  print(round(100*res.delta.change/avg,1))      # relative ændring

}

  