# user options

#year window, first and last year
f.year<-2017
l.year<-2021
make_runs<-TRUE

dev<-'screen'
nox<-1; noy<-3;

# use survey obs for the year after the last assessment year (assuming the survey is conducted the 1. January.)
# 0=no, 1=yes, use extended survey series
extend.survey<-1

# Multispecies
bio.interact<-T


fleet.no<-"all"    # specification of fleets to be used in retrospective analysis
                   # "all"  all availeble fleets area used
                   #  fleet number, e.g. fleet.no<-1 or fleet.no<-c(1,2)
#fleet.no<-c(2)

######   end user options  ###################
cleanup()

setwd(data.path)
oldDir<-data.path

if  (make_runs) {
 
  
  # read data and options into FLR objects
  control<-read.FLSMS.control()
  indices<-SMS2FLIndices(control) 
  
  
  
  # write all the files in a number of directories
  for (y in (l.year:f.year)){
  
     # retrospective runs are made in a separate directory
     retro.dir<-file.path(oldDir,paste("retro",y,sep=''))
     copySMSfiles(control,scenario.dir=retro.dir,doSingle=TRUE,doMulti=TRUE,doArea=FALSE,verbose=FALSE)
     setwd(retro.dir)
  
      for (j in 1:length(indices)) {
        min.yr <- min(as.numeric(dimnames(indices[[j]]@index)$year))
        max.yr <- max(as.numeric(dimnames(indices[[j]]@index)$year))
        if (y < min.yr) stop("year.range is outside indices year range")
        indices[[j]] <- trim(indices[[j]],year=min.yr:(min(max.yr,y+extend.survey)))
      }
      
      outIndi<-indices
      if (fleet.no=="all") outIndi<-indices
      if (fleet.no!="all") {
         outIndi <- FLIndices()
         used.f<-1
         for (f in fleet.no) {  outIndi[[used.f]]<-indices[[f]]; used.f<-used.f+1}
      }
      
      FLIndices2SMS(indices=outIndi,control=control)
   
      control@last.year.model<-y
      control@read.HCR<-0
      control@OP.output<-0  # no OP output, to save time
      #control@catch.sep.year[[1]][3]<-y-6 
      
      write.FLSMS.control(control,write.multi=bio.interact) 
  
      cat("\nDoing retrospective run for year",y,"\n")
     
      do.a.full.SMS.run(outdir=retro.dir, rundir=retro.dir,
                    label="run_",                   # label for output
                    cleanup=T,                      # delete files in the deleteFiles variable?
                    do.single=T,                    # run SMS in single species mode
                    do.multi.1=bio.interact,        # Make preliminary estimate of "predation parameters"
                    do.multi.2=bio.interact,        # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                    do.multi.2.redo=bio.interact,   # Run the full model, with simultaneously estimation of all parameters
                    do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters, Use mean stock numbers (Nbar) for predation
                    do.hessian=F,                   # Make the Hessian matrix and estimate uncertainties
                    do.MCMC=F,                      # Prepare for MCMC analysis
                    mcmc=1000,mcsave=100,                # Options for MCMS analysis
                    do.prediction=F,                # Make a prediction
                    pause=F,                        # Make a pause between each stage
                    Screen.show=F,                  # show the output on screen, or save it in file
                    do.run=T,                       # Make the run immediately, or just make the batch file for the run
                    deleteFiles=NA      )       # clean up in files before the run is made
                 
  
     
     # if (!file.copy("summary.out", paste("summary",y,".out",sep=""), overwrite = TRUE)) stop(paste("Retro stopped: something went wrong in copying summary.dat for year:",y))
     # file.remove("summary.out")
  }
}

dirs<-paste('retro',l.year:f.year,sep='')
labels<-as.character(l.year:f.year)
setwd(oldDir)
oldRoot<-root; root<-oldDir


#source(file.path(prog.path,"compare_runs.R"))

retro_files<-compare_runs(
  dirs=dirs,
  labels=labels,
  nox=2, noy=2,
  paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
  run.ID='retro',         # file id used for paper output
  doGrid=TRUE,
  extent.SSB=FALSE,  # plot SSB for the year after last assessment year
  first.year.on.plot= my.first.year.on.plot,
  last.year.on.plot=my.last.year.on.plot,
  plot.MCMC=FALSE,                        # plot values from MCMC scenarios. FALSE=plot hindcast values from "summary_table_raw.out"
  single.species=FALSE,                   # single species mode or multi species mode
  include.assess.forcast.line=FALSE,      # vertical line at last assessment year
  include.F.reference.points=FALSE,
  include.SSB.reference.points=FALSE,
  include.1.std=FALSE,                   # Include values plus/minus 1 times the standard deviation
  include.2.std=FALSE,
  #incl.sp=c('Herring'),                      # species number to be included. Numbers or "all"
  #incl.sp="all",
  first.pch=0,    # first pch symbol
  first.color=1,   # first color
  palette="default"               # good for colour full plots
  #palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only
)  




source(file.path(prog.path,"compare_runs_objective_function.R"))

if (bio.interact) {
  retro_files_M2<-compare_runs_M2( dirs=dirs,labels=labels,run.ID='retro_M2',sumQuarterly=TRUE)
}

save(retro_files,retro_files_M2,file=file.path(data.path,'Retro_files.Rdata'))

#if (bio.interact) source(file.path(prog.path,"compare_runs_n.R"))
