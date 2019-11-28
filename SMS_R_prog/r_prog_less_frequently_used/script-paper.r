# user options
my.runs<-c('Single species',      # Single species

           'Uniform-Var-Sum',     # Uniform size distribution,         estimate stom variance,        sum prey sizez for likelihood
           'Uniform-noVar-Sum',   # Uniform size distribution, do not estimate stom variance,         sum prey sizez for likelihood
           'Uniform-Var-noSum',   # Uniform size distribution,         estimate stom variance, do not sum prey sizez for likelihood
           'Uniform-noVar-noSum', # Uniform size distribution, do not estimate stom variance,  do not sum prey sizez for likelihood            
           'Uniform-equalVar-noSum',  # Uniform size distribution,         equal stom variance, do not sum prey sizez for likelihood
           'Uniform-equalVar-Sum',    # Uniform size distribution,    equal stom variance,  sum prey sizez for likelihood            

           'Size-Var-Sum',     # log-norm size distribution,         estimate stom variance,        sum prey sizez for likelihood
           'Size-noVar-Sum',   # log-norm size distribution, do not estimate stom variance,         sum prey sizez for likelihood
           'Size-Var-noSum',   # log-norm size distribution,         estimate stom variance, do not sum prey sizez for likelihood
           'size-noVar-noSum', # log-norm size distribution, do not estimate stom variance,  do not sum prey sizez for likelihood  
           'Size-equalVar-noSum',   # log-norm size distribution,         equal stom variance, do not sum prey sizez for likelihood
           'size-equalVar-Sum') # log-norm size distribution, equal stom variance,    sum prey sizez for likelihood            

        
if (F) {   #fine tuning results      
 my.runs<-c('Size-Var-Sum',       # log-norm size distribution,         estimate stom variance,        sum prey sizez for likelihood
           'Size-Var-noSum',      # log-norm size distribution,         estimate stom variance, do not sum prey sizez for likelihood
           'Size-equalVar-noSum', # log-norm size distribution,         equal stom variance, do not sum prey sizez for likelihood
           'size-equalVar-Sum',  
           'Size-noVar-Sum',      # log-norm size distribution, do not estimate stom variance,         sum prey sizez for likelihood
           'size-noVar-noSum')    # log-norm size distribution, do not estimate stom variance,  do not sum prey sizez for likelihood  
}         

if (T) my.runs<-c('Uniform-Var-Sum',     # Uniform size distribution,         estimate stom variance,        sum prey sizez for likelihood
           'Uniform-noVar-Sum',   # Uniform size distribution, do not estimate stom variance,         sum prey sizez for likelihood
           'Uniform-equalVar-Sum')    # Uniform size distribution,    equal stom variance,  sum prey sizez for likelihood            



area<-c('NorthSea','Baltic')[1]
do.run<- F    # run the assessment (or just present results)


file.copy(file.path(data.path,"stomcon_at_length_org.in"),file.path(data.path,"stomcon_at_length.in"),overwrite=TRUE)
file.copy(file.path(data.path,"canum_org.in"),file.path(data.path,"canum.in"),overwrite=TRUE)
file.copy(file.path(data.path,"fleet_catch_org.in"),file.path(data.path,"fleet_catch.in"),overwrite=TRUE)
file.copy(file.path(data.path,"SMS_org.dat"),file.path(data.path,"SMS.dat"),overwrite=TRUE)

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
  for (r in my.runs){
  
     # retrospective runs are made in a separate directory
     my.dir<-file.path(oldDir,r)
     copySMSfiles(my.dir)
  
     setwd(my.dir)
     
      if (area=='NorthSea') {
        # common settings for size selection
                   #           Fulmar   Guillemot   Her. Gull   Kittiwake   GBB. Gull      Gannet      Puffin   Razorbill  R. radiata G. gurnards W. mackerel N. mackerel W.horse mac N.horse mac   Grey seal H. porpoise         Cod     Whiting     Haddock      Saithe 
                              # FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK
        size.selection<-    c(   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,   1,  0,  1) 
        no.size.selection<- c(   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,   0,  0,  0) 
  
        
        other.food.uniform<- paste("# FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK\n",
                                    " 1E6 1E5 1E7 1E7 1E7 1E7 1E5 1E5 1E6 1E6 1E7 1E7 1E6 1E5 1E6 1E6 1E6 1E6 1E7 1E6\n")     
        #Other food, size selection
        other.food.size   <- paste("# FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK\n",
                                    " 1E6 1E5 1E7 1E7 1E7 1E7 1E5 1E5 1E6 1E6 1E7 1E7 1E6 1E5 1E6 1E6 1E6 1E4 1E7 1E6\n")
        
                                  # FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK
        var.scale.stom<-        c(   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1) 
        var.scale.stom.uniform<-c(  10, 10, 10, 10, 10, 10, 10, 10,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1) 
      }
      if (area=='Baltic') {
        size.selection<- 1 
        no.size.selection<- 4 
        other.food.uniform<- paste("# COD\n",
                                    " 1E6 \n")  
        other.food.size   <- paste("# COD\n",
                                    " 1E6 \n")    
        var.scale.stom<-        c(0.1) 
        var.scale.stom.uniform<-c(0.1) 
      }
      
      control@test.output <- 0
      control@var.scale.stom<-var.scale.stom
      control@stomach.variance<-3  # Dirichlet
      control@min.stom.cont[]<-0.9E-4 
      
      
     if (grepl('noSum',r)) control@sum.stom.like[]<-0 else control@sum.stom.like[]<-1
     if (grepl('noVar',r)) control@stom.obs.var[]<-0  else if (grepl('equalVar',r)) control@stom.obs.var[]<-2  else  control@stom.obs.var[]<-1    
             
     if (grepl('Uniform',r)) {
       control@size.selection[]<-0
       control@var.scale.stom<-var.scale.stom.uniform
       control@phase.pref.size.ratio<- -2
       control@phase.var.size.ratio<- -2
       cat(other.food.uniform,file="other_food.in")    
     }
  
     if (grepl('Size',r)) {
        control@size.selection[]<-size.selection
        control@var.scale.stom<-var.scale.stom
        control@phase.pref.size.ratio<-2
        control@phase.var.size.ratio<-2
        cat(other.food.size,file="other_food.in")   
     }

    if (grepl('noVar',r)) control@var.scale.stom[]<-1   else if (grepl('equalVar',r)) control@var.scale.stom[]<-1  

   
     if (grepl('Single',r)) bio.interact<-F else bio.interact<-T
  
     #if (grepl('74',r)) control@first.year.model<-1974
   
      write.FLSMS.control(control,write.multi=bio.interact) 
  
      cat("\nDoing run:",r,"\n")
     
      do.a.full.SMS.run(outdir=my.dir, rundir=my.dir,  
                    label="run_",                   # label for output
                    cleanup=F,                      # delete files in the deleteFiles variable?
                    do.single=T,                    # run SMS in single species mode
                    do.multi.1=bio.interact,                   # Make preliminary estimate of "predation parameters"
                    do.multi.2=bio.interact,                   # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                    do.multi.2.redo=bio.interact,     # bio.interact,              # Run the full model, with simultaneously estimation of all parameters
                    do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters, Use mean stock numbers (Nbar) for predation
                    do.hessian=F,                   # Make the Hessian matrix and estimate uncertainties
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

dirs<-my.runs
labels<-my.runs
setwd(oldDir)
oldRoot<-root; root<-oldDir
cleanup()

source(file.path(prog.path,"compare_runs_objective_function.R"))
source(file.path(prog.path,"compare_runs.R"))
source(file.path(prog.path,"compare_runs_prey_size_selection.r"))
#if (bio.interact) source(file.path(prog.path,"compare_runs_M2.R"))
#if (bio.interact) source(file.path(prog.path,"compare_runs_N.R"))

# data.path<-file.path(data.path,'Size') ; setwd(data.path)
