# this script makes multispecies projections using the OP model given
#   a range of F targets by species


OP.opti<-function(scenario.basis="test",use.weigting=F,use.price.at.age=F,stochastic.recruitment=0,recruit.adjust.CV=0, recruit.adjust.factor=1,my.area='North Sea',
                  change.Other.pred.N=F,use.penalty=F,HCR=1,FBlim.adjust=1,my.first.year=2020,my.last.year=2050,penalty='Blim',penalty.type=1,
                  Hessian=F,option=' ',Fpa.Mult=1,adjust.ini.N=F,OPindicator=1,
                  fac.other=1,  # factor to adjust other predator stock size
                  fac.other.first.year=1,  # first year for change
                  lab.other='',            #scenario label for other
                  fac.other.last.year=10,  # last year for change
                  just.batch.file=F,   # make a batch file for a later run
                  just.plot.results=F   # just plot resultsfrom a previous run
                  ) {
  
  if (OS=='windows') op.command<-'op.exe' else if (OS=='unix') op.command<-'op'
  if (OS=='windows') HPC<-F
  
  if (use.price.at.age) use.weigting<-F
  if (length(HCR)>1) oHCR<-HCR[1] else oHCR<-HCR;
  scenario<<-paste(scenario.basis,'HCR',oHCR,FBlim.adjust[1],paste("Rec",stochastic.recruitment,sep=''),ifelse(use.penalty,ifelse(penalty=='Blim','penBlim','penBpa'),''),ifelse(use.price.at.age,'atAgeW',''),ifelse(use.weigting,'spW',''),ifelse(change.Other.pred.N,'Oth',''),ifelse(recruit.adjust.CV==0,'','CVadj'),'limF',round(sum(Fpa.Mult)*10),ifelse(lab.other=='','',paste('Oth',lab.other,sep='')),sep='_')
  
  
  scenario.dir<<-file.path(data.path,scenario)
  scenario.dir.optim<<-file.path(data.path,scenario)
  
  if (!just.plot.results) {
    if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
    dir.create(scenario.dir,showWarnings = FALSE)
    
    OP.files<-c(op.command,"area_names.in","species_names.in","op_config.dat","op_seed.in","just_one.in", "op_exploitation.in",
                "op_consum.in","op_f.in","op_m1.in","op_m.in","op_n.in","op_propmat.in","op_prop_landed.in","op_size.in","op_wcatch.in","op_wsea.in",
                "op_growth_type1.in","op_consum_ab.in","op_other_n.in","op_price.in","op_msfd.dat","op_reference_points.in","op_length_weight_relations.in",
                "covariance_rec.in","op_ssb_rec_residuals.in",'op_eqsim.in','op_eqsim_stoch.in')
    
    for (from.file in OP.files) {
      to.file<-file.path(scenario.dir,from.file)
      (file.copy(from.file, to.file, overwrite = TRUE))
    }
  }  
  
  # Make OP control object
  setwd(data.path)
  
  #basic settings
  source(file=file.path(prog.path.func,'hcr_op_batch_common.r'))
  res<-make.OP.dat(my.area=my.area,my.last.year=my.last.year,first.year.output=my.first.year,stochastic.recruitment=stochastic.recruitment,recruit.adjust.CV=recruit.adjust.CV, recruit.adjust.factor=recruit.adjust.factor)
  OP<-res[["OP"]]
  #SMS<.res[["SMS"]]
  
  
  OP@indicator<-OPindicator
  OP@last.year<-my.last.year
  
  OP@F.or.C[]<-11
  OP@output<-15
  if (!all(fac.other == 1)) {
    OP@other.predator[2,]<-fac.other.first.year;
    OP@other.predator[3,]<-fac.other.last.year;
    OP@other.predator[1,]<- fac.other;
  } 

  
  
  ###
  
  ref<-Read.reference.points.OP(dir=file.path(data.path,scenario)) 
  Blim<-as.vector(ref[,'Blim'])
  Bpa<-as.vector(ref[,'Bpa'])
  Fpa<-as.vector(ref[,'Fpa'])
  
  if (length(FBlim.adjust)==1) FBlim.adjust<-rep(1,length(Fpa))
  
  
  if (my.area=='North Sea') {
    if (NS.Key.2014) { 
      #             Cod     Whiting     Haddock      Saithe     Herring     NorthSandeel  southSan       Nor. pout       Sprat      Plaice        Sole 
      TSBBlim<- c(    1,          1,          1,          1,          1,     Blim[6]*3.1,  Blim[7]*3.2,  Blim[8]*2.4,     Blim[9]*2.3,          1,          1) 
      TSBBpa<-  c(    1,          1,          1,          1,          1,    Bpa[6]*3.1,   Bpa[7]*3.2,   Bpa[8]*2.4,      Bpa[9]*2.3,          1,          1) 
    }
    if (!NS.Key.2014) {
      #             Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole 
      TSBBlim<- c(    1,          1,          1,          1,          1,     787000,     263000,      157000,          1,          1) 
      TSBBpa<-  c(    1,          1,          1,          1,          1,    1098000,     440000,      213000,          1,          1) 
    } 
  } else {
    TSBBlim<- c(1,1,1)
    TSBBpa<-  c(1,1,1)
  }
  
  
  SMS<-read.FLSMS.control()
  nsp<-SMS@no.species
  n.other.pred<-sum(SMS@species.info[,'predator']==2)
  n.pred<-n.other.pred+sum(SMS@species.info[,'predator']==1)
  n.vpa<-nsp-n.other.pred
  n.vpa.pred<-sum(SMS@species.info[,'predator']==1)
  
  #cat('\n',n.vpa,n.other.pred)
  
  OPT<-read.FLOPtrigger.control(file="op_trigger.dat",n.VPA=n.vpa,n.other.pred=n.other.pred) 
  OPT@first.year<-my.first.year 
  OPT@last.year<-my.last.year
  OPT@no.iter<-1
  
  OPT@SSBpenalty['use',]<-0    # initialise to zero (no SSB penalize)
  if (use.penalty) {
    if (penalty.type==1) {
      OPT@SSBpenalty['use',]<-1
      if (my.area=='North Sea'){
        OPT@SSBpenalty['use','Plaice']<- 0
        OPT@SSBpenalty['use','Sole']<- 0
      }
      if (penalty=='Blim' | penalty=='Bpa') {
        pfac1<-0.72; # L50 relative to limit  WAS 0.7
        pfac2<- 1.1   # steepnes,  
        if (penalty=='Blim') {
          OPT@SSBpenalty['SSB50',]<-Blim*pfac1
        } else {
          OPT@SSBpenalty['SSB50',]<-Bpa*pfac1; 
        }
        OPT@SSBpenalty['SSB75',]<-OPT@SSBpenalty['SSB50',]*pfac2
      } 
      if (stochastic.recruitment>=1) factor1<-100 else factor1<-10; 
      #OPT@SSBpenalty['factor',]<-factor1*(Bpa*Fpa) 
      OPT@SSBpenalty['factor',]<-factor1*(Bpa)  
    } 
    
    if (penalty.type==2) {    # simple version
      OPT@SSBpenalty['use',]<-2
      if (my.area=='North Sea'){
        OPT@SSBpenalty['use','Plaice']<- 0
        OPT@SSBpenalty['use','Sole']<- 0
      }
      if (penalty=='Blim') OPT@SSBpenalty['SSB50',]<-Blim else  OPT@SSBpenalty['SSB50',]<-Bpa; 
      OPT@SSBpenalty['SSB75',]<-2  # power function exponent
      OPT@SSBpenalty['factor',]<-0.1*(Bpa*Fpa)  
    }
    
    
    if (use.price.at.age ) {
      price<-read.table(file=file.path(data.path,'op_price.in'))
      price[price==0]<-NA
      price<-apply(price,1,mean,na.rm=T)
      OPT@SSBpenalty['factor',]<-OPT@SSBpenalty['factor',]*price
    }
    
    
  } else OPT@SSBpenalty['use',]<-0
  
  if (use.weigting) {
    if (my.area=='North Sea') {
      if (NS.Key.2014) OPT@weighting[]<-c(10, 4, 4, 4, 2, 1, 1,1,1,5, 20) else OPT@weighting[]<-c(10, 4, 4, 4, 2, 1, 1,1,5, 20) 
    } else if (my.area=='Baltic Sea') OPT@weighting[]<-c(1,1,1) 
  } else OPT@weighting[]<-1 
  
  
  if (use.price.at.age) {
    OPT@weighting[]<-1
    OPT@at.age.weighting<-1
  } else OPT@at.age.weighting<-0
  
  
  OPT@HCR[]<-HCR                       
  for (s in(1:n.vpa)) {
    
    if (HCR[s]==1 | HCR[s]==2) {   
      OPT@Ftarget['lower',s]<-0.0    
      OPT@Ftarget['higher',s]<-Fpa[s] * Fpa.Mult[s]               
      OPT@Ftarget['init',s]<-Fpa[s]*0.7           
      OPT@Ftarget['phase',s]<-1
      if (F) {
        OPT@Ftarget['init','Plaice']<- 0.376
        OPT@Ftarget['init','Sole']<- 0.344
        OPT@Ftarget['phase','Plaice']<- -1
        OPT@Ftarget['phase','Sole']<- -1
      }
    }
    
    if (HCR[s]==2) {   
      OPT@trigger['T1',s]<-Blim[s] * FBlim.adjust[s]
      OPT@trigger['T2',s]<-Bpa[s]      
    }
    
    if (HCR[s]==22) {
      OPT@trigger['T1',s]<-TSBBlim[s]* FBlim.adjust[s] 
      OPT@trigger['T2',s]<-TSBBpa[s]; 
    } 
    
    if (HCR[s]==4) {
      OPT@SSB50['lower',s]<-Blim[s]/1000*0.75
      OPT@SSB50['higher',s]<-Bpa[s]/1000
      OPT@SSB50['init',s]<-  Blim[s]/1000*0.75
      OPT@SSB50['phase',s] <-1
      
      #S1<-L50*log(3.0)/(L75-L50)
      L50<-OPT@SSB50['init',s]
      L75<-L50*1.25   # min range
      OPT@S1['lower',s]<-L50*log(3.0)/(L75-L50)
      L75<-L50*1.05   # max
      OPT@S1['higher',s]<-L50*log(3.0)/(L75-L50)
      L75<-L50*1.10   # init
      OPT@S1['init',s]<-L50*log(3.0)/(L75-L50)
      OPT@S1['phase',s]<-1
    }
  }
  
  if (change.Other.pred.N) {
    othN<-scan(file=file.path(data.path,'op_Other_n.in'),comment.char = "#" )
    dim(othN)<-c(SMS@max.age.all+1,n.other.pred,SMS@last.season)
    dimnames(othN)=list( paste('a_',(0:SMS@max.age.all),sep=''),
                         sp.names[1:n.other.pred],
                         paste('q_',(1:SMS@last.season),sep=''))
  }
  
  
  
  sp.name<-SMS.control@species.names
  
  setwd(scenario.dir)
  
  if (adjust.ini.N) {
    
    ini.N<-matrix( scan(file="op_n.in",comment.char="#"),nrow=n.vpa,byrow=T)
    if (my.area=='North Sea') {
      ini.N[1,]<-ini.N[1,]*1.35  # cod
      # ini.N[1,]<-ini.N[3,]*1.5  # haddock
    }
    write.matrix(ini.N,file="op_n.in")
  }
  
  if (!just.plot.results) {
    write.FLOP.control(OP,file='op.dat',nice=T)
    write.FLOPtrigger.control(OPT,file="op_trigger.dat") 
  }
  OPTG<<-OPT
  

  
  #run OP
  if (Hessian) h=' ' else h='-nohess'
 
  
  #The run OP command
  comm.sms<-paste( file.path(scenario.dir,op.command),"-nox ",h,sep=" ")
  
  if (just.batch.file) {
    if (OS=='windows') bat.file<-"run.OP.bat"  else if (OS=='unix') bat.file<-"run_OP.sh" 
    if (OS=='windows') {
      cat("cd ",scenario.dir,"\n",comm.sms,' >ud.dat\n',file=file.path(scenario.dir,bat.file))
      #stop(paste("go to directory",scenario.dir,"and run script ",bat.file,"\n"))
      system(command=file.path(scenario.dir,bat.file), wait = F)
    }
    if (OS=='unix' & HPC) {
      #run OP command
      comm.sms<-paste( op.command,"-maxfn 0 -nohess ",sep=" ")
      
      cat(paste(
        "#!/bin/sh",
        "# embedded options to qsub - start with #PBS",
        "# -- Name of the job ---",
        "#PBS -N SMS",
        "# –- specify queue --",
        "#PBS -q hpc",
        "# -- estimated wall clock time (execution time): hh:mm:ss --",
        "#PBS -l walltime=00:30:00",
        "# –- number of processors/cores/nodes --",
        "#PBS -l nodes=1:ppn=4",
        "# –- user email address --",
        "# - PBS -M mv@aqua.dtu.dk",
        "# –- mail notification –-",
        "# - PBS -m abe",
        paste("cd ",scenario.dir,sep=''),
        paste(comm.sms," > ud.dat","\n"),
        sep='\n'),file=file.path(scenario.dir,bat.file))    
    }
    
  } else   if (!just.plot.results) {
    if (OS=='windows') shell(comm.sms, invisible = TRUE)
    cat("\ncommand: ",comm.sms,'\n')
    if (OS=='unix') system(comm.sms)
  }
  
  

  setwd(data.path)
  
  if (!just.batch.file) {
    source(file.path(prog.path,"hcr_op_plot.r"))
    
    mode<-scan(file=file.path(scenario.dir,'op_config.dat'),n=1,comment.char = "#")
    ptitle<-paste(ifelse(mode==0,'Single species, ',''),ifelse(stochastic.recruitment>0,'Stochastic','Determenistic'),' recruitment, ',
                  ifelse(use.penalty,ifelse(penalty=='Blim','penalize SSB below Blim,','penalize SSB below Bpa,'),''),' maximising total ',
                  ifelse(use.price.at.age | use.weigting,ifelse(use.price.at.age,'value','value'),'yield'),', Years: ',my.first.year,'-',my.last.year,sep='')
    cat(ptitle,'\n')
    plot.OP.HCR(file.name="HCR-OP", my.device='png',first.year.on.plot=my.first.year,last.year.on.plot=my.last.year,scenario.dir=scenario.dir,tit=ptitle,used.penalty=penalty)
    plot.OP.HCR(file.name="HCR-OP", my.device='png',first.year.on.plot=my.first.year,last.year.on.plot=my.last.year,scenario.dir=scenario.dir,tit=ptitle,used.penalty=penalty,portrait=FALSE)
    
    
    source(file.path(prog.path,"plot_summary_ices_multi.r"))
    if (Hessian) source(file.path(prog.path.func,"hcr_op_batch_optimize_derivatives.r"))
    if (stochastic.recruitment>0) source(file.path(prog.path,"hcr_op_plot_sr_residuals.r"))
  }
}
