# user options
my.runs<-
 c('Single_species',      # Single species
  'Diri_Uniform_noNumber_noMesh_Sum_type2',
  'Diri_Uniform_noNumber_noMesh_Sum_type3',
  'Diri_Uniform_noNumber_noMesh_noSum_type2',
  'Diri_Uniform_noNumber_noMesh_noSum_type3',
  'Diri_Size_noNumber_noMesh_noSum_type2',
  'Diri_Size_noNumber_noMesh_Sum_type2',
  'Diri_Size_Number_noMesh_Sum_type2',
  'Diri_Size_Number_noMesh_noSum_type2',
  'Diri_Size_noNumber_noMesh_Sum_type2_ALK',
  'lNorm_Size_noNumber_noMesh_Sum_type2_ALK',
  'lNorm_Size_noNumber_noMesh_Sum_type2',
  'lNorm_Size_Number_noMesh_Sum_type2',
  'lNorm_Size_Number_noMesh_noSum_type2'   
  )

 #my.runs<-c('lNorm_Size_noNumber_noMesh_Sum_type2_ALK')



area<-c('NorthSea','Baltic')[1]
do.run<- T    # run the assessment (or just present results)

# make use of number model by species (if Number options has been chosen)
                           # FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK
use.number.model<-        c(   F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  T,  T,  F,  T) 
use.size.model<-use.number.model
#use.size.model<-         c(   F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  F,  T,  T,  F,  T) 

                 #          Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole 
mesh.selction<-c(            -1,         -1,         -1,         -1,         -1,         -1,         -1,         -1,         -1,         -1) 


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

  SMS.files.multi<-c("alk_stom.in","ALK_all.in","consum.in","Length_weight_relations.in","lsea.in","N_haul_at_length.in",
                     "natmor1.in","other_food.in","season_overlap.in","stom_pred_length_at_sizecl.in","stom_struc_at_length.in",
                     "stomcon_at_length.in","stomlen_at_length.in","stomweight_at_length.in","stomnumber_at_length.in","stomtype_at_length.in","pred_prey_size_range_param.in",
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
      
      #default settings
        control@incl.stom.all<-           1 
        control@use.Nbar<-                0 
        control@M2.iterations<-           3 
        control@max.M2.sum2<-             3 
        control@stom.likelihood<-         1 
        control@stomach.variance<-        3       
        control@simple.ALK<-              0 
        control@consum<-                  0 
        control@size.select.model<-       2     # 2=prey weights from stom_obs, 3=from l-W relation
        control@L50.mesh[]<-             -1 
        control@size.selection[]<-        0
        control@sum.stom.like[]<-         0 
        control@stom.obs.var[]<-          1 
                                        # FUL  GLT  HEG  KTW  GBG  GNT  PUF  RAZ  RAJ  GUR  W_M  N_M  W_H  N_H  GSE  HBP  COD  WHG  HAD  POK
        control@stom.max.sumP[]<-       c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 500, 100, 100, 100)
        control@var.scale.stom[]<-        1 
                                            # FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK
        control@size.other.food.suit<-    c(   0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1, 1 ,  0,  0,  0,  0,  0,   1,  1,  1) 
        control@min.stom.cont[]<-      1E-09 
        control@max.stom.sampl[]<-      1000 
        control@stom.type.include[]<-      2
                                           # FUL  GLT  HEG  KTW  GBG  GNT  PUF  RAZ  RAJ  GUR  W_M  N_M  W_H  N_H  GSE  HBP  COD  WHG  HAD  POK
        control@prey.pred.size.fac<-       c(5,   5,   5,   5,   5,   5,   5,  5,    0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 50,  50,  0.5, 0.6, 0.5, 0.5) 

        control@use.overlap<-             0 
        control@phase.vulnera<-           2 
        control@phase.other.suit.slope<-  2 
        control@phase.pref.size.ratio<-   -1 
        control@phase.pref.size.ratio.correction<-    -1 
        control@phase.prey.size.adjustment<-  -1 
        control@phase.var.size.ratio<-    -1 
        control@phase.season.overlap<-    2 
        control@phase.stom.var<-          2 
        control@phase.mesh.adjust<-      -1 

      
      
      
      
        # common settings for size selection
  
        
        other.food.uniform<- paste("# FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK\n",
                                    " 1E6 1E5 1E7 1E7 1E7 1E7 1E5 1E5 1E6 1E6 1E6 1E7 1E6 1E5 1E6 1E6 1E6 1E6 1E7 1E6\n")     
        #Other food, size selection
        other.food.size   <- paste("# FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK\n",
                                    " 1E6 1E5 1E7 1E7 1E7 1E7 1E5 1E5 1E6 1E6 1E7 1E7 1E6 1E5 1E6 1E6 1E6 1E5 1E7 1E6\n")
        
                                  # FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK
        var.scale.stom<-        c(   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1) 
        var.scale.stom.uniform<-c(  10, 10, 10, 10, 10, 10, 10, 10,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1) 
        var.scale.stom.uniform<- c(  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1) 

                                          # FUL GLT HEG KTW GBG GNT PUF RAZ RAJ GUR W_M N_M W_H N_H GSE HBP COD WHG HAD POK
        sum.stom.like<-                 c(   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1) 
        no.sum.stom.like<-              c(   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0) 


      }
      if (area=='Baltic' & F) {
        other.food.uniform<- paste("# COD\n",
                                    " 1E6 \n")  
        other.food.size   <- paste("# COD\n",
                                    " 1E6 \n")    
        var.scale.stom<-        c(0.1) 
        var.scale.stom.uniform<-c(0.1) 
      }
      
      control@test.output <- 0
      
     if (grepl('Single',r)) bio.interact<-F else bio.interact<-T
      
     if (grepl('Diri_',r))   control@stomach.variance<- 3 
     if (grepl('lNorm_',r))   control@stomach.variance<- 1 
      
       if (grepl('_ALK',r))  control@simple.ALK<-1

     if (grepl('_Uniform',r)) {
       control@size.selection[]<-0
       control@var.scale.stom<-var.scale.stom.uniform
       control@phase.pref.size.ratio<- -2
       control@phase.var.size.ratio<- -2
       cat(other.food.uniform,file="other_food.in")    
     }
  
     if (grepl('_Size',r)) {
        do.size<-T
        control@size.selection[]<-0;
        control@size.selection[use.size.model]<-1
        
        control@var.scale.stom<-var.scale.stom
        control@phase.pref.size.ratio<-2
        control@phase.var.size.ratio<-2
        cat(other.food.size,file="other_food.in")   
     } else do.size<-F
     
     if (grepl('_Number_',r))   {
       control@stom.likelihood<-2 
       do.number<-T
     } else {
       control@stom.likelihood<-1 
       do.number<-F
     }
     
     if (grepl('noSum',r)) control@sum.stom.like[]<-no.sum.stom.like else  if (grepl('_Sum',r)) control@sum.stom.like[]<-sum.stom.like
   
     if (grepl('Single',r)) bio.interact<-F else bio.interact<-T

     if (grepl('_type1',r))   {
       control@stom.type.include[ ]<-1 
       if (do.number) control@stom.type.include[use.number.model]<-3 
     }

     if (grepl('_type2',r))   {
       control@stom.type.include[ ]<-2 
       if (do.number) control@stom.type.include[use.number.model]<-3 
       if (do.size) control@stom.type.include[use.size.model]<-3 

     }
  
     if (grepl('_type3',r))   {
       control@stom.type.include[ ]<-3 
     }
  
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
