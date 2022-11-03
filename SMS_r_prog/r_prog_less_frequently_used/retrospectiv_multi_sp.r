# user options

#year window, first and last year
f.year<-2014
l.year<-2019

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

copySMSfiles<-function(scenario.dir) {

  if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
  dir.create(scenario.dir,showWarnings = FALSE)

  SMS.files.single<-c("area_names.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                      "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                      "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                      "proportion_M_and_F_before_spawning.in","proportion_landed.in",
                      "Exploitation_pattern.in","covariance_N.in","HCR_options.dat","sms.dat",
                      "SMS.exe")

  for (from.file in SMS.files.single) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  SMS.files.multi<-c("run_ms3.dat","alk_stom.in","consum.in","Length_weight_relations.in","lsea.in","N_haul_at_length.in",
                     "natmor1.in","other_food.in","season_overlap.in","stom_pred_length_at_sizecl.in","stom_struc_at_length.in",
                     "stomcon_at_length.in","stomlen_at_length.in","stomweight_at_length.in","pred_prey_size_range_param.in",
                     "stomtype_at_length.in","stomnumber_at_length.in","other_pred_n.in","cons_multiplier_options.in",
                     "incl_stom.in","temperature.in","recruitment_years.in","n_proportion_m2.in")

  for (from.file in SMS.files.multi) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }
  
  SMS.files.area<-c("stock_distribution.in","predator_area_presence.in")
  if (control@no.areas > 1)  for (from.file in SMS.files.area) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }  
   
}

# read data and options into FLR objects
control<-read.FLSMS.control()
indices<-SMS2FLIndices(control) 



# write all the files in a number of directories
for (y in (l.year:f.year)){

   # retrospective runs are made in a separate dirictory
   retro.dir<-file.path(oldDir,paste("retro",y,sep=''))
   copySMSfiles(retro.dir)

   setwd(retro.dir)

  
    for (j in 1:length(indices))
    {
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
                  cleanup=F,                      # delete files in the deleteFiles variable?
                  do.single=T,                    # run SMS in single species mode
                  do.multi.1=bio.interact,                   # Make preliminary estimate of "predation parameters"
                  do.multi.2=bio.interact,                   # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                  do.multi.2.redo=bio.interact,              # Run the full model, with simultaneously estimation of all parameters
                  do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters, Use mean stock numbers (Nbar) for predation
                  do.hessian=F,                   # Make the Hessian matrix and estimate uncertainties
                  do.MCMC=F,                      # Prepare for MCMC analysis
                  mcmc=1000,mcsave=100,                # Options for MCMS analysis
                  do.prediction=F,                # Make a prediction
                  pause=F,                        # Make a pause between each stage
                  Screen.show=F,                  # show the output on screen, or save it in file
                  do.run=T,                       # Make the run immediately, or just make the batch file for the run
                  deleteFiles=NA       ,        # clean up in files before the run is made
                  )                  
   
   # if (!file.copy("summary.out", paste("summary",y,".out",sep=""), overwrite = TRUE)) stop(paste("Retro stopped: something went wrong in copying summary.dat for year:",y))
   # file.remove("summary.out")
}


dirs<-paste('retro',l.year:f.year,sep='')
labels<-as.character(l.year:f.year)
setwd(oldDir)
oldRoot<-root; root<-oldDir


source(file.path(prog.path,"compare_runs_objective_function.R"))
source(file.path(prog.path,"compare_runs.R"))
if (bio.interact) source(file.path(prog.path,"compare_runs_M2.R"))
if (bio.interact) source(file.path(prog.path,"compare_runs_N.R"))
