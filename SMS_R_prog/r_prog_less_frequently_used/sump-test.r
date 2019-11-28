# user options

my.pmax<-c(10,15,20,30,50,100,200,500)
my.pmax<-c(10,50,200,500)
# my.pmax<-c(500)

area<-c('NorthSea','Baltic')[1]
bio.interact<-T
do.run<- F    # run the assessment (or just present results)

if (T) {
  file.copy(file.path(data.path,"stomcon_at_length_org.in"),file.path(data.path,"stomcon_at_length.in"),overwrite=TRUE)
  file.copy(file.path(data.path,"canum_org.in"),file.path(data.path,"canum.in"),overwrite=TRUE)
  file.copy(file.path(data.path,"fleet_catch_org.in"),file.path(data.path,"fleet_catch.in"),overwrite=TRUE)
  file.copy(file.path(data.path,"SMS_org.dat"),file.path(data.path,"SMS.dat"),overwrite=TRUE)
  }
  
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
                      "Exploitation_pattern.in","covariance_N.in",
                      "HCR_options.dat","sms.dat",
                      "SMS.exe")

  for (from.file in SMS.files.single) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  SMS.files.multi<-c("alk_stom.in","consum.in","Length_weight_relations.in","lsea.in","N_haul_at_length.in",
                     "natmor1.in","other_food.in","season_overlap.in","stom_pred_length_at_sizecl.in","stom_struc_at_length.in",
                     "stomcon_at_length.in","stomlen_at_length.in","stomweight_at_length.in","pred_prey_size_range_param.in",
                     "incl_stom.in","temperature.in")

  for (from.file in SMS.files.multi) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }
  
  SMS.files.area<-c("stock_distribution.in","predator_area_presence.in")
  if (control@no.areas > 1)  for (from.file in SMS.files.area) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }  
  
  if (area=='NorthSea') {
     SMS.NS.files<-c("zero_catch_season_ages.in","zero_catch_year_season.in","F_q_ini.in","known_recruitment.in","other_pred_N.in")
     for (from.file in SMS.NS.files) {
      to.file<-file.path(scenario.dir,from.file)
      file.copy(from.file, to.file, overwrite = TRUE)
    } 
   } 
}

if (do.run) {
  # read data and options into FLR objects
  control<-read.FLSMS.control()
  
  
  # write all the files in a number of directories
  for (pmax in my.pmax){
  
     #  runs are made in a separate directory
     my.dir<-file.path(oldDir,paste('pmax',pmax,sep='_'))
     copySMSfiles(my.dir)
  
     setwd(my.dir)
     
      control@test.output <- 0
      control@OP.output <- 0
      control@stomach.variance<-3  # Dirichlet
      control@min.stom.cont[]<-0.9E-4 
      
      
      control@stom.max.sumP[control@stom.max.sumP!=100]<-pmax  
    
      write.FLSMS.control(control,write.multi=bio.interact) 
  
      cat("\nDoing run:",pmax,"\n")
   
      do.a.full.SMS.run(outdir=my.dir, rundir=my.dir,  
                    label="run_",                   # label for output
                    cleanup=F,                      # delete files in the deleteFiles variable?
                    do.single=T,                    # run SMS in single species mode
                    do.multi.1=bio.interact,                   # Make preliminary estimate of "predation parameters"
                    do.multi.2=bio.interact,                   # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                    do.multi.2.redo=bio.interact,     # bio.interact,              # Run the full model, with simultaneously estimation of all parameters
                    do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters, Use mean stock numbers (Nbar) for predation
                    do.hessian=T,                   # Make the Hessian matrix and estimate uncertainties
                    do.MCMC=F,                      # Prepare for MCMC analysis
                    mcmc=1000,mcsave=100,           # Options for MCMS analysis
                    do.prediction=F,                # Make a prediction
                    pause=F,                        # Make a pause between each stage
                    Screen.show=F,                  # show the output on screen, or save it in file
                    do.run=T,                       # Make the run immediately, or just make the batch file for the run
                    deleteFiles=NA       ,        # clean up in files before the run is made
                    new.version=F)                  # copy a (new) version of the sms program from the program directory (default=FALSE)
                
  
     
     # if (!file.copy("summary.out", paste("summary",y,".out",sep=""), overwrite = TRUE)) stop(paste("Retro stopped: something went wrong in copying summary.dat for year:",y))
     # file.remove("summary.out")
  }
}
my.runs<-paste('pmax',my.pmax,sep='_')
dirs<-my.runs
labels<-my.runs
setwd(oldDir)
oldRoot<-root; root<-oldDir
cleanup()

source(file.path(prog.path,"compare_runs_objective_function.R"))
source(file.path(prog.path,"compare_runs.R"))
#source(file.path(prog.path,"compare_runs_prey_size_selection.r"))
#if (bio.interact) source(file.path(prog.path,"compare_runs_M2.R"))
#if (bio.interact) source(file.path(prog.path,"compare_runs_N.R"))

# data.path<-file.path(data.path,'Size') ; setwd(data.path)
