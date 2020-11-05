# Function to run SMS

# delete files before the first run is made, only in force if do.single=T

deleteFiles<-c("*.?0?","*.out","*.mcm","*.bin","admodel.*","*.csv","*.std","*.bar","*.mc2","*.cor","*.psv","*.ecm",
                    "*.xls","*.html", "mcout*.all","_*.txt","OP_BATCH*.*","SMS.o*",
                    "*.wmf","*.png","*.ps","*.lg","*.log","ud.dat","gradient.dat","op_*.out","iter*.*","stom_and_noise*.in","canum_and_noise*","survey_and_noise*","baseline*.*",
                    "HCR_prob.dat","HCR_yield.dat","HCR_SSB.dat","*.par","*.rep","*.hst","*.eva","*.tmp","amoeba*.*","covariance_*.*","forecast*.*")
#deleteFiles<-NA

if (F) {
  file.copy(file.path(data.path,"stomcon_at_length_org.in"),file.path(data.path,"stomcon_at_length.in"),overwrite=TRUE)
  file.copy(file.path(data.path,"canum_org.in"),file.path(data.path,"canum.in"),overwrite=TRUE)
  file.copy(file.path(data.path,"fleet_catch_org.in"),file.path(data.path,"fleet_catch.in"),overwrite=TRUE)
  file.copy(file.path(data.path,"SMS_org.dat"),file.path(data.path,"SMS.dat"),overwrite=TRUE)
}
do.a.full.SMS.run(label="run_",                   # label for output
                  cleanup=T,                      # delete files in the deleteFiles variable?
                  do.single=T,                    # run SMS in single species mode
                  do.multi.1=T,                   # Make preliminary estimate of "predation parameters"
                  do.multi.2=T,                   # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                  do.multi.2.redo=T,              # Run the full model, with simultaneously estimation of all parameters
                  do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters, Use mean stock numbers (Nbar) for predation
                  do.hessian=T,                   # Make the Hessian matrix and estimate uncertainties
                  SSB.R.seperate=F,               # Estimate S/R parameters in a seperate step  
                  do.MCMC=F,                      # Prepare for MCMC analysis
                  mcmc=0,mcsave=0,                # Options for MCMC analysis
                  do.prediction=F,                # Make a prediction
                  pause=F,                        # Make a confirm between each stage
                  Screen.show=F,                  # show the output on screen (TRUE), or save it in files "*.lg" (FALSE)
                  do.run=F,                       # Make the run immediately, or just make the batch file for the run
                  deleteFiles=deleteFiles,        # clean up in files before the run is made
                  HPC=F)                          # run it as batch program on the UNIX High  Performance Computer 
              






