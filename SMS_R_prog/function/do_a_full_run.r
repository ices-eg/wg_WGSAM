# Function to make runs with SMS
# label:        a text string included in the file name for sceen output (if Scrren.show=TRUE)
#do.single:     do a single species SMS
#do.multi.1     do a sms run in multispecies mode 1
#do.multi.2     do a sms run in multispecies mode 2
#do.hessian     Create the Hessian matrix and parameter uncertanities in sms.std file
#do.mcmc        perform Markov Chain Monte Carlo with a number (=mcmc) of simulations and save (=mcsave) a set of parameters for MCMC analysis
#mcmc           create a number (=mcmc) number of MCMC simulations
#mcsave         save the parameters for every mcsave simultaions
#do.likelihood.profil  # Make a likelihood profile of SSB 
#do.prsave      # save results of likelihood profile
#do.prediction  perform a single species prediction
#Screen.show    Show results from SMS run on the R-console screen (TRUE) or write results to file ud*.*
#pause          pause after each SMS run
#do.run         Run the batch file (do.run=TRUE) or just create batch file (do.run=FALSE)

do.a.full.SMS.run<-function(label="",rundir=data.path,outdir=data.path,do.single=TRUE,do.multi.1=FALSE,do.multi.2=FALSE,do.multi.2.redo=FALSE, do.multi.2.redo.Nbar=FALSE,
                            do.hessian=FALSE,do.MCMC=FALSE,mcmc=1,mcsave=1,do.MCMCeval=F,shake.ms2.par=F, cleanup=F,SSB.R.seperate=F, do.likelihood.profile=F,do.prsave=F,
                            do.prediction=FALSE,do.run=TRUE, Screen.show=TRUE,pause=FALSE,ADMB.options="-gbs 1500000000 ",deleteFiles=NA,format.options=T,HPC=F) {


  # label="run_";rundir=data.path;outdir=data.path
file<-file.path(outdir,'sms.dat')

if (OS=="windows") {
  dos.root<-gsub(.Platform$file.sep,"\\",root,fixed=TRUE)
  outdir<-paste(gsub(.Platform$file.sep,"\\",outdir,fixed=TRUE),"\\",sep="")
  #outdir<-gsub(.Platform$file.sep,"\\",outdir,fixed=TRUE)
  do.bat<-"do.bat"
  del<-"del /f "
  cp<-"copy /Y "
  quo<-'"'
  HPC<-F

} else if (OS=="unix") {
  dos.root<-root
  do.bat<-"do.sh"
  del<-"rm -f "
  cp<-"cp -f "
  quo<-''
  
}
    
pgm<-"sms"

sms.do<-file.path(outdir,paste0(label,do.bat))
iter<-0
append<-FALSE



control<-read.FLSMS.control(dir=rundir)   # read option file, SMS.dat

if (OS=="unix" & HPC){
  HPC.init<-paste(
    "#!/bin/sh",
    "# embedded options to qsub - start with #PBS",
    "# -- Name of the job ---",
    "#PBS -N SMS",
    "# –- specify queue --",
    "#PBS -q hpc",
    "# -- estimated wall clock time (execution time): hh:mm:ss --",
    ifelse(do.hessian, "#PBS -l walltime=00:60:00","#PBS -l walltime=00:30:00"),
    "# –- number of processors/cores/nodes --",
    "#PBS -l nodes=1:ppn=1",
    "# –- user email address --",
    "# - PBS -M mv@aqua.dtu.dk",
    "# –- mail notification –-",
    "# - PBS -m abe",
    "# -- run in the current working (submission) directory --",
    "if test X$PBS_ENVIRONMENT = XPBS_BATCH; then cd $PBS_O_WORKDIR; fi",
    "# here follow the commands you want to execute\n",
    sep='\n')
  #cat(HPC.init);cat('\n\n')
  cat(HPC.init,file=sms.do,append=append)
  append<-TRUE
  
  #delete requested files
  if (!is.na(deleteFiles[1]) & do.single) {
    for  (dels in deleteFiles) {
      cat(paste(del, dels,"\n",sep=""),file=sms.do,append=append)
    }
  }
  
}
if (do.single) {

 if (OS=="windows") {  
     #delete requested files
     cat(paste(dosDrive,"\n","cd ",quo,outdir,quo,"\n",sep=""),file=sms.do,append=append)
     if (!is.na(deleteFiles[1])) {
        for  (dels in deleteFiles) {
        cat(paste("del ", dels,"\n",sep=""),file=sms.do,append=TRUE)
       }
     }
  } 
   
 append<-TRUE
 
    #run SMS in single species mode
    control@VPA.mode<-0          # be sure that the the run is made in single species mode
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-2 
       control@phase.SSB.R.beta<-2 
    }
    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms0.dat',sep="")),write.multi=F,nice=format.options,writeSpNames=F)

    if (do.hessian) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave)
    if (do.MCMCeval) hes.string<-paste(" -mceval ")
    if (do.likelihood.profile)  hes.string<-paste(" -lprof ")
    if (do.likelihood.profile & do.prsave)  hes.string<-paste(" -lprof -prsave ")
   
    if (do.multi.1 | do.multi.2 | do.multi.2.redo) hes.string<-' -nohess '

   if (OS=="windows")  cat(paste(dosDrive,"\n","cd ",quo,outdir,quo,"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste(del,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (Screen.show) cat(paste(pgm," -nox -ind ",label,"ms0.dat",hes.string,"\n",sep=""),file=sms.do,append=TRUE)
    if (!Screen.show) cat(paste(pgm," -nox -ind ",label,"ms0.dat",hes.string," >",label,"out0_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.par",quo," ",quo,label,"ms0.par",quo,"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.rep",quo," ",quo,label,"ms0",".rep",quo,"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian & !(do.multi.1 | do.multi.2 | do.multi.2.redo)) cat(paste(cp,quo,"sms.std",quo," ",quo,label,"ms0",".std",quo,"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.1) {
    #run SMS in multi species mode 1, start to change options
    control@VPA.mode<-1                    # set multispecies mode =1
    if (control@stomach.variance != 33) control@phase.stom.var<-2              # stomach variance
    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms1.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)

    #run in musltispecies mode 1
     if (do.hessian & do.multi.2==F & do.multi.2.redo==F)  hes.string<-' ' else hes.string<-' -nohess '
    if (OS=="windows") cat(paste(dosDrive,"\n","cd ",quo,outdir,quo,"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste(del,quo,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,quo,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,quo,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms1.dat  -ainp ",label,"ms0.par -phase 2 >",label,"out1_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms1.dat   -ainp ",label,"ms0.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.par",quo," ",quo,label,"ms1.par",quo,"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.rep",quo," ",quo,label,"ms1",".rep",quo,"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.2) {
    #run SMS in multi species mode 2, start to change options
    control@VPA.mode<-2                    # set multispecies mode =2
    control@phase.stom.var<- -1            # stomach variance
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-3 
       control@phase.SSB.R.beta<-3 
    }

    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms2.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)

    #run in multispecies mode 2

    if (do.hessian &  do.multi.2.redo==F & do.multi.2.redo.Nbar==F) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC & do.hessian & do.multi.2.redo==F & do.multi.2.redo.Nbar==F) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave)
    if (OS=="windows") cat(paste(dosDrive,"\n","cd ",quo,outdir,quo,"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste(del,quo,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,quo,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,quo,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms2.dat -ainp ",label,"ms1.par -phase 2 >",label,"out2_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms2.dat -ainp ",label,"ms1.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.par",quo," ",quo,label,"ms2.par",quo,"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.rep",quo," ",quo,label,"ms2",".rep",quo,"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian) cat(paste(cp,quo,"sms.std",quo," ",quo,label,"ms2",".std",quo,"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.2.redo) {
    #run SMS in multi species mode 2 with estimation of all parameters, start to change options
    control@VPA.mode<-2                    # set multispecies mode =2
    control@phase.stom.var<-2              # stomach variance
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-3 
       control@phase.SSB.R.beta<-3 
    }

    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms3.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)

    #run in multispecies mode 2 using parameter file from previous multi=2 run
    if (do.hessian &  do.multi.2.redo.Nbar==F) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC & do.hessian & do.multi.2.redo.Nbar==F) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave)
    if (OS=="windows") cat(paste(dosDrive,"\n","cd ",quo,outdir,quo,"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste(del,quo,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,quo,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste(del,quo,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3.dat -ainp ",label,"ms2.par -phase 2 >",label,"out3_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3.dat -ainp ",label,"ms2.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.par",quo," ",quo,label,"ms3.par",quo,"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.rep",quo," ",quo,label,"ms3",".rep",quo,"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian) cat(paste(cp,quo,"sms.std",quo," ",quo,label,"ms3",".std",quo,"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}

if (do.multi.2.redo.Nbar) {
    #run SMS in multi species mode 2 with estimation of all parameters, start to change options
    control@VPA.mode<-2                    # set multispecies mode =2
    control@phase.stom.var<- 2              # stomach variance.
    control@use.Nbar<-1                    # Use mean stock number 
    control@M2.iterations<-6
    control@max.M2.sum2<-0 
    if (SSB.R.seperate) {
       control@phase.SSB.R.alfa<-3 
       control@phase.SSB.R.beta<-3 
    }

    write.FLSMS.control(control,file=file.path(rundir,paste(label,'ms3Nbar.dat',sep="")),write.multi=T,nice=format.options,writeSpNames=F)
    
    if (shake.ms2.par) {
       p<-scan(file.path(rundir,paste(label,"ms2.par",sep="")),comment.char = "#")
       p<-p*runif(n=length(p),min=1.0,max=1.15)
       cat(p,file=file.path(rundir,paste(label,"ms2.par",sep="")))
    }
    #run in multispecies mode 2 using parameter file from previous multi=2 run
    if (do.hessian) hes.string<-' ' else hes.string<-' -nohess '
    if (do.MCMC & do.hessian) hes.string<-paste(" -mcmc",as.integer(mcmc)," -mcsave",mcsave)
    if (OS=="windows") cat(paste(dosDrive,"\n","cd ",quo,outdir,quo,"\n",sep=""),file=sms.do,append=append)
   if (cleanup) cat(paste("rm -f ",quo,"sms.rep \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("rm -f ",quo,"sms.par \n",sep=""),file=sms.do,append=TRUE)
   if (cleanup) cat(paste("rm -f ",quo,"sms.std \n",sep=""),file=sms.do,append=TRUE)

    if (!Screen.show) cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3Nbar.dat -ainp ",label,"ms2.par -phase 2 >",label,"out4_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -nox ",ADMB.options,hes.string," -ind ",label,"ms3Nbar.dat -ainp ",label,"ms2.par -phase 2 \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.par",quo," ",quo,label,"ms3Nbar.par",quo,"\n",sep=""),file=sms.do,append=TRUE)
    cat(paste(cp,quo,"sms.rep",quo," ",quo,label,"ms3Nbar",".rep",quo,"\n",sep=""),file=sms.do,append=TRUE)
    if (do.hessian) cat(paste(cp,quo,"sms.std",quo," ",quo,label,"ms3Nbar",".std",quo,"\n",sep=""),file=sms.do,append=TRUE)
    append<-TRUE
}


if (do.prediction) {
  if (OS=="windows") cat(paste(dosDrive,"\n","cd ",quo,outdir,quo,"\n",sep=""),file=sms.do,append=append)
    control@read.HCR<-1
     ll<-"ms3.dat"
    if (do.single) ll<-"ms0.dat"
    if (do.multi.2) ll<-"ms2.dat"
    if (do.multi.2.redo) ll<-"ms3.dat"

    if (!Screen.show) cat(paste(pgm," -ind ",label,ll," -mceval >",label,"out5_",iter,".lg\n",sep=""),file=sms.do,append=TRUE)
    if (Screen.show)  cat(paste(pgm," -ind ",label,ll," -mceval \n",sep=""),file=sms.do,append=TRUE)
    if (pause & Screen.show) cat("pause \n",file=sms.do,append=TRUE)
}

if (do.run) {

    command<-paste(quo,sms.do,quo,sep='')
    command<-sub('/','',command)
    if (OS=="windows") shell(command, invisible = TRUE) 
    if (OS=="unix") system(command,show.output.on.console =Screen.show) 
    
} else cat(paste("batch file: ",sms.do,"for SMS run is made\n"))
}

