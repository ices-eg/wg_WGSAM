read.in.files<-function(years,quarters,areas,species,ages,filename,checksum=T) {
  a<-expand.grid(Area=areas,Species.n=species,Year=years,Quarter=quarters,Age=ages)
  a<-a[order(a$Area,a$Species.n,a$Year,a$Quarter,a$Age),]
  variable<-scan(file.path(data.path,filename),comment.char='#')
  if (checksum) variable<-head(variable,-1)
  a$variable<-variable
  a
}


m1<-read.in.files(years=SMS.control@first.year:SMS.control@last.year,
                    quarters=1:SMS.control@last.season,
                    areas=1:SMS.control@no.areas,
                    species=sp.names[2:nsp],
                    ages=SMS.control@first.age:SMS.control@max.age.all,
                    filename="natmor1.in")

names(m1)<-c("Area","Species","Year","Quarter","Age","m1")

m11<-tapply(m1$m1,list(m1$Species,m1$Year,m1$Quarter,m1$Age),sum,na.rm=T)
m11[m11<0]<-0
m1fac<-0.5
m11<-m11*m1fac
dim(m11)
dimnames(m11)
filename<-"natmor1.in"

out<-file.path(data.path,filename)
unlink(out)
dig<-3

  cat(paste("# M1 values, changed by a factor ",m1fac,"\n"),file=out,append=TRUE)
  
  for (sp in sp.names[2:(nsp)]) {

    out1<<-m11[sp,,,]
    for (y in dimnames(m11)[[2]]){
      out2<-out1[as.character(y),,]
      out2[is.na(out2)]<--1
      cat(paste("#",sp ,"year:",y,"\n"),file=out,append=TRUE)
      write.table(format(round(out2,dig),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)    
    }
  }
  cat('-999 # checksum',file=out,append=TRUE)

 
  
  deleteFiles<-c("*.?0?","*.out","*.mcm","*.bin","admodel.*","*.csv","*.std","*.bar","*.mc2","*.cor","*.psv","*.ecm",
                 "*.xls","*.html", "mcout*.all","_*.txt","OP_BATCH*.*","SMS.o*","gradient.dat","*.grd","*.Rdata",
                 "*.wmf","*.png","*.ps","*.lg","*.log","ud.dat","gradient.dat","op_*.out","iter*.*","stom_and_noise*.in","canum_and_noise*","survey_and_noise*","baseline*.*",
                 "HCR_prob.dat","HCR_yield.dat","HCR_SSB.dat","*.par","*.rep","*.hst","*.eva","*.tmp","amoeba*.*","covariance_*.*","forecast*.*")
  
   
  do.a.full.SMS.run(label="run_",                   # label for output
                    cleanup=T,                      # delete files in the deleteFiles variable?
                    do.single=T,                    # run SMS in single species mode
                    do.multi.1=T,                   # Make preliminary estimate of "predation parameters"
                    do.multi.2=T,                   # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter
                    do.multi.2.redo=T,              # Run the full model, with simultaneously estimation of all parameters
                    do.multi.2.redo.Nbar=F,         # Run the full model, with simultaneously estimation of all parameters except the stomach variance parameter, Use mean stock numbers (Nbar) for predation
                    do.hessian=F,                   # Make the Hessian matrix and estimate uncertainties
                    shake.ms2.par=F,
                    SSB.R.seperate=F,               # Estimate S/R parameters in a separate step  
                    do.MCMC=F,                      # Prepare for MCMC analysis
                    mcmc=0,mcsave=0,                # Options for MCMC analysis
                    do.prediction=F,                # Make a prediction
                    pause=F,                        # Make a confirm between each stage
                    Screen.show=T,                  # show the output on screen (TRUE), or save it in files "*.lg" (FALSE)
                    do.run=T,                       # Make the run immediately, or just make the batch file for the run
                    deleteFiles=deleteFiles,        # clean up in files before the run is made
                    HPC=F)                          # run it as batch program on the UNIX High Performance Computer 
  
  
  source(file.path(prog.path,'plot_summary_ices_multi.r'))
  
  dirs<-c("Baltic-2022-keyRun","Baltic-2022-V07_M1sensiti")
  labels<-c("2022 key run","M1*0.5")
  

  source(file.path(prog.path,'compare_runs_M2.R'))
  source(file.path(prog.path,'compare_runs.R'))  
  
  a<-Read.summary.data(dir=file.path(root,dirs[1])) %>%  mutate(M1M2a=M1+M2) %>%
    dplyr::select(Species,Year,Quarter,Species.n,Age,M1M2a)
  b<-Read.summary.data(dir=file.path(root,dirs[2])) %>%  mutate(M1M2b=M1+M2) %>%
    dplyr::select(Species,Year,Quarter,Species.n,Age,M1M2b)
 ab<-full_join(a,b) %>% filter(M1M2a>0 & Age %in% c(0,1,2,4,7)) %>% dplyr::mutate(Age=paste('Age',Age))

 summary(ab)
 ggplot(ab, aes(M1M2a, M1M2b)) + geom_point() + facet_grid(rows = vars(Species),cols=vars(Age),scales="free_x")+
     geom_abline(intercept = 0, slope = 1,colour = 'red')+xlab('Key run M1+M2')+ylab('M1+M2 with keyrun M1*0.5')
 ggsave('compM1_quarter.png')
 
 ggplot(ab, aes(M1M2a, M1M2b)) + geom_point() + facet_grid(rows = vars(Species),cols=vars(Age),scales="free_x")+
   geom_abline(intercept = 0, slope = 1,colour = 'red')+xlab('Key run M1+M2')+ylab('M1+M2 with keyrun M1*0.5')+
 geom_smooth(method = "lm", se = FALSE,color='blue')
 ggsave('compM1_quarter_extra.png')
 
 

 
 ab2<-ab %>% group_by(Species,Year,Age) %>% summarize(M1M2a=sum(M1M2a),M1M2b=sum(M1M2b))
 ggplot(ab2, aes(M1M2a, M1M2b)) + geom_point() + facet_grid(rows = vars(Species),cols=vars(Age),scales="free_x")+
   geom_abline(intercept = 0, slope = 1,colour = 'red')+xlab('Key run M1+M2')+ylab('M1+M2 with keyrun M1*0.5')
 ggsave('compM1_annual.png')
 
 
 
 