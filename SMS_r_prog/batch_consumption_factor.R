predators<-sp.names[1:npr]

runs<-lapply(0:npr,sum) # just to make a lost with runs. One predator per combination

runs[[length(runs)+1]]<-16:20
runs[[length(runs)+1]]<-1:20


setwd(data.path)
oldDir<-data.path


lapply(runs,function(x)paste(as.character(x),collapse='_'))
names(runs)<-lapply(runs,function(x)paste(sp.names[x],collapse='_'))


cons<-matrix(scan(file.path(data.path,"cons_multiplier_options.in"),comment.char = "#"),ncol=3,byrow=T)
rownames(cons)<-predators

bio.interact<-TRUE

lapply(runs,function(x){ 

  scenario.dir<-paste0('cons_',paste(as.character(x),collapse='_'))
  
  copySMSfiles<-function(scenario.dir) {
    
    if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
    dir.create(scenario.dir,showWarnings = FALSE)
    
    SMS.files.single<-c("area_names.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                        "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                        "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                        "proportion_M_and_F_before_spawning.in","proportion_landed.in","recruitment_years.in",
                        "zero_catch_season_ages.in","zero_catch_year_season.in","F_q_ini.in",
                        "Exploitation_pattern.in","covariance_N.in","HCR_options.dat","sms.dat",
                        "SMS.exe")
    
    for (from.file in SMS.files.single) {
      to.file<-file.path(scenario.dir,from.file)
      file.copy(from.file, to.file, overwrite = TRUE)
    }
    
    SMS.files.multi<-c("alk_stom.in","consum.in","Length_weight_relations.in","lsea.in","N_haul_at_length.in",
                       "natmor1.in","other_food.in","season_overlap.in","stom_pred_length_at_sizecl.in","stom_struc_at_length.in",
                       "stomcon_at_length.in","stomlen_at_length.in","stomweight_at_length.in","stomtype_at_length.in",
                       "stomnumber_at_length.in","pred_prey_size_range_param.in","other_pred_N.in",
                       "incl_stom.in","temperature.in","n_proportion_m2.in","consum_ab.in","cons_multiplier_options.in")
    
    for (from.file in SMS.files.multi) {
      to.file<-file.path(scenario.dir,from.file)
      file.copy(from.file, to.file, overwrite = TRUE)
    }
  
    SMS.parms<-c("run_ms2.dat","run_ms1.par")
    for (from.file in SMS.parms) {
      to.file<-file.path(scenario.dir,from.file)
      file.copy(from.file, to.file, overwrite = TRUE)
    }
    
  }

copySMSfiles(scenario.dir)
if (sum(x)>0) {  
  cons[,3]<- -1
  cons[x,3]<-2
  write(t(cons),file=file.path(scenario.dir,"cons_multiplier_options.in"),ncolumns = 3)
}

cat("\nDoing  run for scenario",scenario.dir,"\n")
  
do.a.full.SMS.run(outdir=file.path(data.path,scenario.dir), rundir=file.path(data.path,scenario.dir),
                    label="run_",                   # label for output
                    cleanup=F,                      # delete files in the deleteFiles variable?
                    do.single=F,                    # run SMS in single species mode
                    do.multi.1=F,                  # Make preliminary estimate of "predation parameters"
                    do.multi.2=T,       # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                    do.multi.2.redo=F,          # Run the full model, with simultaneously estimation of all parameters
                    do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters, Use mean stock numbers (Nbar) for predation
                    do.hessian=F,                   # Make the Hessian matrix and estimate uncertainties
                    do.MCMC=F,                      # Prepare for MCMC analysis
                    mcmc=1000,mcsave=100,                # Options for MCMS analysis
                    do.prediction=F,                # Make a prediction
                    pause=F,                        # Make a pause between each stage
                    Screen.show=F,                  # show the output on screen, or save it in file
                    do.run=F,                       # Make the run immediately, or just make the batch file for the run
                    deleteFiles=NA      )       # clean up in files before the run is made


})



#source(file.path(prog.path,"compare_runs.R"))

#rr<-runs[2:17]
rr<-runs

obj<-lapply(rr,function(x){ 
  
  scenario.dir<-paste0('cons_',paste(as.character(x),collapse='_'))
  a<-Read.objective.function(dir=scenario.dir,extend=FALSE,read.init.function=TRUE)
  a<-subset(a,select=c(-n.catch, -n.CPUE, -n.SSB.R, -n.stom, -n.all.obs, -penalty, -stomachs.N))
})
obj

obj.all<-lapply(obj,function(x){ 
  head(x,1)$all
})
obj.all
plot(unlist(obj.all),ylab='log likelihood')


pp<-lapply(rr,function(x){ 
 # x<-rr[[22]]
  scenario.dir<-paste0('cons_',paste(as.character(x),collapse='_'))
  a<-readLines(file.path( scenario.dir,"sms.par"))
  lapply(x,function(xx) as.numeric(a[grep(paste0("# cons_multiplier[",xx,']:'),a,fixed=TRUE)+1])) 
})
pp



