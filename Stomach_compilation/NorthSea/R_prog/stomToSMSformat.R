# script for updating stomach data

# start to run init.R for choice of environment

newEnv<-"NS_2023"  #Directory for R source

exchangeDir<-file.path( "Data_NorthSea","input_NS_2023")   # directory with data  files on spread sheet format
RexchangeDir<-"MakeANewEnvironment"     # directory with R scripts to convert files etc.
finalExchangeDir<-file.path(root,"Data_NorthSea","final_input_NS_2023")



## North Sea all other pred (one Mackerel), Two sandeel stocks and hake 2023 WGSAM

min.stomach.sampled.in.stratum<-1
code.name<-       c("OTH","FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP","HKE",'COD','WHG','HAD','POK','MAC','HER','NSA','SSA','NOP','SPR','PLE','SOL')
code.name.pred<-        c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP","HKE",'COD','WHG','HAD','POK','MAC')
code.name.prey<- c("OTH",'COD','WHG','HAD','HER','NSA','SSA','SPR','NOP','PLE')
selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1995,2000,2002,2005,2010,2013)
#selected.years<-c(1981,2013)

year.q<-NULL
var.groups.size<-NULL
var.groups<-     NULL
min.stom.groups<-1

rescale_sampling <-FALSE  # TRUE rescales the sampling effort, such that the maximum value get the value 100
boot_sample_id<-c('sample_id','haul')
w_type<-c("as_observed","meanBoots","mu")
stomMark<-c("_Boots_0500","_Simple_0001")
hb<-c('hb','')
labs<-expand.grid(sample=boot_sample_id,hb=hb,type=stomMark,w_type=w_type) %>%
  mutate(lab=if_else(hb=='hb',paste(type,sample,hb,w_type,sep='_'),paste(type,sample,w_type,sep='_')))
labs
#
#

labs

my_labs<-labs[c(19),]  # sample
my_labs




my_labs<-labs[c(20),]  # haul
my_labs


my_labs<-labs[c(8),]  # haul
my_labs

my_labs<-labs[c(4,20),]  # haul
my_labs

#just checking file existence
for (i in (1:dim(my_labs)[[1]])) {
  stomMark<-my_labs[i,'lab']
  f<-file.path(finalExchangeDir,paste0("ALK_stom_list",stomMark,'.dat'))
  cat(i,f,' exists:',file.exists(f),'\n')
  f<-file.path(finalExchangeDir,paste0("stomcon_list",stomMark,'.dat'))
  cat(' ',f,' exists:',file.exists(f),'\n')
}

rm(species)  # to make the next call work
source(file.path(prog.path,RexchangeDir,newEnv,'From_list_to_SMS_format.R'))

base_dir<-"NS_2023_04"


if (FALSE) {
 i<-1 # test

  if (FALSE) for (i in (1:dim(my_labs)[[1]])) {
      stomMark<-my_labs[i,'lab']
      cat(stomMark,'\n')
      odir<-file.path(root,paste(base_dir,stomMark,sep='_'))
      if (file.exists( odir)) unlink(  odir,recursive = T)
      cat(odir,'\n')
      if (!dir.exists(odir)) dir.create(odir)
      for (ff in list.files(file.path(root,base_dir))) file.copy(from=file.path(root,base_dir,ff),
                                                 to=file.path(odir,ff),overwrite=TRUE)

      data.path<-odir
      if (my_labs[i,'sample']=='sample_id') samp_eff<- "stom_no"  else samp_eff<- "haul_no"
      SMS.data.transform(list.data.path=file.path(finalExchangeDir),stomMark=stomMark,
                       trans.bio=T, trans.catch=T,
                       trans.meanL=F,  trans.meanL.from.weight=FALSE,
                       trans.stomach=T, trans.ALK.stomach=T, trans.other=T, trans.Consum=T, trans.ALK.all=F,
                       stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
                       inserted.haul.no.propor=1.0, sampling_effort= samp_eff,
                       formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)

      file.copy(from=file.path(odir,"N_haul_at_length.in"),to=file.path(odir,"N_haul_at_length_org.in"),overwrite =TRUE)
      file.copy(from=file.path(odir,"N_haul_at_length_scaled.in"),to=file.path(odir,"N_haul_at_length_scaled_org.in"),overwrite =TRUE)
  } #end for

  for (i in (1:dim(my_labs)[[1]])) {
    stomMark<-my_labs[i,'lab']
    cat(stomMark,'\n')
    odir<-file.path(root,paste(base_dir,stomMark,sep='_'))
    cat(odir,'\n')
    data.path<-odir
    setwd(odir)

    do.a.full.SMS.run(label="run_",                   # label for output
                  cleanup=T,                      # delete files in the deleteFiles variable?
                  do.single=T,                    # run SMS in single species mode
                  do.multi.1=T,                   # Make preliminary estimate of "predation parameters"
                  do.multi.2=T,                   # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                  do.multi.2.redo=T,              # Run the full model, with simultaneously estimation of all parameters
                  do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter, Use mean stock numbers (Nbar) for predation
                  do.hessian=T,                   # Make the Hessian matrix and estimate uncertainties
                  shake.ms2.par=F,
                  SSB.R.seperate=F,               # Estimate S/R parameters in a separate step
                  do.MCMC=F,                      # Prepare for MCMC analysis
                  mcmc=0,mcsave=0,                # Options for MCMC analysis
                  do.prediction=F,                # Make a prediction
                  pause=F,                        # Make a confirm between each stage
                  Screen.show=F,                  # show the output on screen (TRUE), or save it in files "*.lg" (FALSE)
                  do.run=F,                       # Make the run immediately, or just make the batch file for the run
                  deleteFiles=deleteFiles,        # clean up in files before the run is made
                  HPC=F)                          # run it as batch program on the UNIX High Performance Computer
    } #end for
}


if (FALSE) {
 getwd()
  #stomMark<-"_Simple_0001_haul_as_observed"
  stomMark<-"_Boots_0500_haul_as_observed"
  #stomMark<-"_Boots_0500_haul_mu"
  #stomMark<-"_Boots_0500_haul_meanBoots"
  SMS.data.transform(list.data.path=file.path(finalExchangeDir),stomMark=stomMark,
                   trans.bio=T, trans.catch=T,
                   trans.meanL=T,  trans.meanL.from.weight=FALSE,
                   trans.stomach=T, trans.ALK.stomach=T, trans.other=T, trans.Consum=T, trans.ALK.all=F,
                   stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
                   inserted.haul.no.propor=1.0, sampling_effort= c("stom_no","haul_no")[2],
                   formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)

  file.copy(from=file.path(data.path,"N_haul_at_length.in"),to=file.path(data.path,"N_haul_at_length_org.in"),overwrite =TRUE)
  file.copy(from=file.path(data.path,"N_haul_at_length_scaled.in"),to=file.path(data.path,"N_haul_at_length_scaled_org.in"),overwrite =TRUE)
  if (rescale_sampling) file.copy(from=file.path(data.path,"N_haul_at_length_scaled_org.in"),to=file.path(data.path,"N_haul_at_length_scaled.in"),overwrite =TRUE)
}
