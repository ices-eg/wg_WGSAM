# this script makes multispecies projections using the OP model given
#   a range of F targets by species
 
OP.simulate<-function(
          scenario="my-scenario", 
          my.area=c('North Sea','Baltic Sea')[1],
          my.last.year=2061, years.in.average=50,
          
          HCR=NA,
          FBlim.adjust=1,
          
          stochastic.recruitment=0,
          recruit.adjust.CV=0,        #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
          recruit.adjust.factor=rep(1,nsp.VPA), # factor for adjutment of recruitment
          
          first.run.no=1,         # first run number (within the present iteration)
          first.no.iter.stoch=1,  # iteration number 
          no.iter.stoch=10,  # default for stochastic recruitment 
          
          do.indicators=T,
          
          do.simulation=T,     # do the simulations and create data files for plots
          just.batch.file=F,   # make a batch file for a later run
          
          combine.scenario.run=F,    # special for combining scenarios
          scenario.dirs='a',         # name of scenarios to be combined
          scenario.dirs.out='Samlet',   # directory name for output R data set
          
          adjustBaltic=F,
          OP.output=14,  # 14 for combinations of F
          change.Other.pred.N=F,
          adjust.ini.N=F,     # adjust inital population size  (NS cod)
          
          read.condense=T,
          read.indicators=T,
          
          do.plot.indicators=T,
          do.plot.condense=T,
          ReduceTable=F,      # stepwise selection table of F from given criteria
          ReduceTableDetails=F,   # output full (detalied results after each reduction)
          ReduceOption='default', #
          saveOutput=F,   # save output from reduction of tables
          riskLevels=rep(5,nsp.VPA), # Risk levels for SSB below Blim
          keepPro=0.95,  # keep F values which is factor keepPro of  MSY, used in stepwise reduction
          
          subset.out=FALSE,       # make a subset of data set used to make plots
          justAboveBlim=FALSE,    # make plot for Fcombinations where the risk to Blim <5%
          justAboveBpa=F,
          subset.species=matrix(c(rep(0,nsp.VPA),rep(2,nsp.VPA)),ncol=nsp.VPA,byrow=T),  # min and max F for inclusion in plots
          ask.me=F,
          targetFs=NA,   # combinations of F
         
          fac.other=1,  # factor to adjust other predator stock size
          fac.other.first.year=-1,  # first year for change
          lab.other='',            #scenario label for other
          fac.other.last.year=-1,  # last year for change
          
          singleSp=F,      # Make single species plot      
          
           my.dev='png'
){


first.year.output<-my.last.year-years.in.average+1
cat("first year used in output and in average values:",first.year.output,"\nlast year:",my.last.year,"\n")


no.iter<-1  # for determenistic recruitment
if (stochastic.recruitment>0) no.iter<-no.iter.stoch

#####################################

#  runs are made in a separate directory

scenario<-paste(scenario,paste("HCR",HCR[1],sep=''),FBlim.adjust[1],paste("Rec",stochastic.recruitment,sep=''),ifelse(recruit.adjust.CV[1]==1 | recruit.adjust.CV[1]==2,"Recadj",""),my.last.year,sep='_')

scenario.gem<<-scenario
scenario.dir<-file.path(data.path,scenario)


ref<-Read.reference.points.OP(dir=data.path) 
Blim<-as.vector(ref[,'Blim'])
Bpa<-as.vector(ref[,'Bpa'])
Fpa<-as.vector(ref[,'Fpa'])


if (my.area=='North Sea') {
 if (KeyRunYear==2014) { 
  #             Cod     Whiting     Haddock      Saithe     Herring     NorthSandeel  southSan       Nor. pout       Sprat      Plaice        Sole 
  TSBBlim<- c(    1,          1,          1,          1,          1,    Blim[6]*3.1,  Blim[7]*3.2,  Blim[8]*2.4,     Blim[9]*2.3,          1,          1) 
  TSBBpa<-  c(    1,          1,          1,          1,          1,    Bpa[6]*3.1,   Bpa[7]*3.2,   Bpa[8]*2.4,      Bpa[9]*2.3,          1,          1) 
 }
# if (!NS.Key.2014) {
#   #             Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole 
#   TSBBlim<- c(    1,          1,          1,          1,          1,     787000,     263000,      157000,          1,          1) 
#   TSBBpa<-  c(    1,          1,          1,          1,          1,    1098000,     440000,      213000,          1,          1) 
# } 
if (KeyRunYear==2017) {
     #             Cod     Whiting     Haddock      Saithe     Herring    Mackerel,  Sandeel   Nor. pout       Sprat      Plaice        Sole 
     TSBBlim<- c(    1,          1,          1,          1,          1,      1,         787000,     263000,      157000,          1,          1) 
     TSBBpa<-  c(    1,          1,          1,          1,          1,     1,         1098000,     440000,      213000,          1,          1) 
}
  if ( KeyRunYear==2020) {
    #             Cod     Whiting     Haddock      Saithe     Herring    Mackerel,  Sandeel   Nor. pout       Sprat      Plaice        Sole 
    TSBBlim<- c(    1,          1,          1,          1,          1,      1,         787000,     263000,      157000,          1,          1) 
    TSBBpa<-  c(    1,          1,          1,          1,          1,     1,         1098000,     440000,      213000,          1,          1) 
  }
  
} else {  # Baltic
   TSBBlim<- c(1,1,1)
   TSBBpa<-  c(1,1,1)
}
 

# Make an OP control object. File='OP.dat'
setwd(data.path)
source(file=file.path(prog.path.func,'hcr_op_batch_common.r'))

res<-make.OP.dat(my.area=my.area,my.last.year=my.last.year,first.year.output=first.year.output,
                 do.indicators=do.indicators,
                 stochastic.recruitment=stochastic.recruitment)
OP<-res[["OP"]]
SMS<-res[["SMS"]]

sp.name<-SMS@species.names

 OP@recruit.adjust.CV[1,]<-recruit.adjust.CV
 
 OP@recruit.adjust[1,]<-recruit.adjust.factor
 
if (my.area=='Baltic Sea' & adjustBaltic) {# adjustment was done when environmental S-R were used
 OP@recruit.adjust.CV[1,1]<-0     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
 OP@recruit.adjust.CV[1,2]<-1     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
 OP@recruit.adjust.CV[1,3]<-1     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
}

OP@F.or.C[]<-31
OP@output<-OP.output

if (!all(fac.other == 1)) {
  OP@other.predator[2,]<-fac.other.first.year;
  OP@other.predator[3,]<-fac.other.last.year;
  OP@other.predator[1,]<- fac.other;
} 

write.FLOP.control(OP,file='op.dat',nice=T)

if (length(FBlim.adjust)==1) FBlim.adjust<-rep(1,length(Fpa))
  
### op_trigger file
  nsp<-SMS@no.species
  n.other.pred<-sum(SMS@species.info[,'predator']==2)
  n.pred<-n.other.pred+sum(SMS@species.info[,'predator']==1)
  n.vpa<-nsp-n.other.pred
  n.vpa.pred<-sum(SMS@species.info[,'predator']==1)

OPT<-read.FLOPtrigger.control(file="op_trigger.dat",n.VPA=n.vpa,n.other.pred=n.other.pred) 
OPT@last.year<-my.last.year 
OPT@first.year<-first.year.output               
OPT@HCR[]<-HCR  
OPT@first.run.no<-first.run.no
OPT@first.iter.no<-first.no.iter.stoch
OPT@no.iter<-no.iter

OPT@trigger['T1',]<-Blim* FBlim.adjust
OPT@trigger['T2',]<-Bpa 
OPT@at.age.weighting<-1 
for (s in(1:n.vpa)) if (HCR[s]==22) {
  OPT@trigger['T1',s]<-TSBBlim[s]* FBlim.adjust[s] 
  OPT@trigger['T2',s]<-TSBBpa[s]; 
}   
write.FLOPtrigger.control(OPT,file="op_trigger.dat") 

                                                       
# MSFD and other options 
if (do.indicators) {
 MSFD<-FLOP.MSFD.control()

 if (my.area=='Baltic Sea') {
   MSFD@do.community.biomass.demersal<-1
   MSFD@community.biomass.demersal.ages[ ,2:3]<- -1     # cod (species 1) is a demersal species
   
   MSFD@do.community.biomass.small<-1 
   MSFD@community.biomass.small.ages['last.age',1]<- 1     # fish below 25 cm is small, that includes all herring and sprat, cod age 0-1 is small species

   MSFD@do.community.biomass.pelagic<-1 
   MSFD@community.biomass.pelagic.ages[,1]<- -1     # herring and sprat (species 2, 3) are pelagic species

   MSFD@do.community.biomass.forage<-1 
   MSFD@community.biomass.forage.ages[,1]<- -1     # herring and sprat (species 2, 3) are forage species

   MSFD@do.M2.bar<-1 
   MSFD@M2.bar.ages['last.age',1]<- 1     # cod age 0 and 1, and all ages for herring and sprat (species 2, 3)



   MSFD@do.community.F<-1
   MSFD@community.F.sp[]<-1   #include all species
   MSFD@do.community.M<-1
   MSFD@community.M.sp[]<-1   #include all species
   
   MSFD@do.life.expectancy <-1
   MSFD@life.expectancy.first.age[]<-0
    
   MSFD@do.community.life.expectancy<-1 
   MSFD@community.life.expectancy.options["first.age",]<-0   
   MSFD@community.life.expectancy.options["weighting",]<-1         

   MSFD@do.LFI<-1
   MSFD@LFI.sp[]<-1
   MSFD@LFI.age[]<-c(3,99,99) # only cod age 3+ is greather than 40 cm
 }
 
 if (my.area=='North Sea' & KeyRunYear==2014) {
    # 1 Fulmar 
    # 2 Guillemot 
    # 3 Her. Gull 
    # 4 Kittiwake 
    # 5 GBB. Gull 
    # 6 Gannet 
    # 7 Puffin 
    # 8 Razorbill 
    # 9 R. radiata 
    # 10 G. gurnards 
    # 11 W. mackerel 
    # 12 N. mackerel 
    # 13 W.horse mac 
    # 14 N.horse mac 
    # 15 Grey seal 
    # 16 H. porpoise 
    # 17 Cod 
    # 18 Whiting 
    # 19 Haddock 
    # 20 Saithe 
    # 21 Herring 
    # 22 Sandeel 
    # 23 Nor. pout 
    # 24 Sprat 
    # 25 Plaice 
    # 26 Sole 

   
# 2017 key run
  # 1 Fulmar 
  # 2 Guillemot 
  # 3 Her. Gull 
  # 4 Kittiwake 
  # 5 GBB. Gull 
  # 6 Gannet 
  # 7 Puffin 
  # 8 Razorbill 
  # 9 R. radiata 
  # 10 G. gurnards 
  # 11 W.horse mac 
  # 12 N.horse mac 
  # 13 Grey seal 
  # 14 H. porpoise 
  # 15 Hake 
  # 16 Cod 
  # 17 Whiting 
  # 18 Haddock 
  # 19 Saithe 
  # 20 Mackerel 
  # 21 Herring 
  # 22 N. sandeel 
  # 23 S. sandeel 
  # 24 Nor. pout 
  # 25 Sprat 
  # 26 Plaice 
  # 27 Sole 
  
   # THIS IS NOT UPDATED WITH 2017 key RUN SPECIES 
   
   MSFD@do.community.biomass.demersal<-1
   MSFD@community.biomass.demersal.ages['first.age' ,c(1:8,11:16,21,24)]<-  99  # exclude    
   MSFD@community.biomass.demersal.ages['last.age'  ,c(1:8,11:16,21,24)]<-  -1  # exclude 
      
   MSFD@do.community.biomass.small<-0  # not ready yet!!!!!!!!!!!!!!!!!!!!!!!
   MSFD@community.biomass.small.ages['last.age',c(1:8,11:16,21,24)]<- -1     # exclude    NEEDS UPDATE

   MSFD@do.community.biomass.pelagic<-1 
   MSFD@community.biomass.pelagic.ages['first.age',c(1:20,22:23,25:26)]<- 99     # exclude
   MSFD@community.biomass.pelagic.ages['last.age',c(1:20,22:23,25:26)]<- -1     # exclude

   MSFD@do.community.biomass.forage<-1 
   MSFD@community.biomass.forage.ages['first.age',c(1:20,25:26)]<- 99     # exclude
   MSFD@community.biomass.forage.ages['last.age',c(1:20,25:26)]<- -1     # exclude
    
#Average M2      age 0  age 1  age 2  age 3  age 4  age 5  age 6  age 7  age 8  age 9  age 10
#Cod             1.541  0.986  0.553  0.076  0.000  0.000  0.000  0.000  0.000  0.000  0.000
#Whiting         1.195  1.042  0.457  0.378  0.371  0.345  0.324  0.271  0.236
#Haddock         1.104  1.116  0.355  0.166  0.111  0.062  0.034  0.018  0.007  0.003  0.000
#Saithe          0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
#Herring         0.551  0.463  0.248  0.212  0.187  0.169  0.159  0.134
#Sandeel         0.782  0.822  0.678  0.520  0.516
#Nor. pout       1.130  1.728  1.428  1.250
#Sprat           0.647  1.165  0.891  0.625

   MSFD@do.M2.bar<-1 
   MSFD@M2.bar.ages['first.age',]<- c(0,0,0,-1,0,0,0,0,-1,-1)     # all ages for forage species, and >0.2 for other species
   MSFD@M2.bar.ages['last.age',] <- c(2,8,2,-1,3,4,3,3,-1,-1)     # all ages for forage species, and >0.2 for other species

   MSFD@do.community.mean.weight <-1 
   MSFD@community.mean.weight.sp[]<-1  #include all species

   MSFD@do.community.mean.weight.C <-1 
   MSFD@community.mean.weight.C.sp[]<-1  #include all species

   MSFD@do.mean.weight.C <-0 
   MSFD@mean.weight.C.sp[]<-1    #include all species       

   MSFD@do.community.F<-1
   MSFD@community.F.sp[]<-1   #include all species
   MSFD@do.community.M<-1
   MSFD@community.M.sp[]<-1   #include all species
   
   MSFD@do.life.expectancy <-1
   MSFD@life.expectancy.first.age[]<-0
    
   MSFD@do.community.life.expectancy<-1 
   MSFD@community.life.expectancy.options["first.age",]<-0   
   MSFD@community.life.expectancy.options["weighting",]<-1         

   MSFD@do.LFI<-1
   MSFD@LFI.sp[ ,c(1:16,18,21:24)]<- 0  # exclude  
   MSFD@LFI.age[]<-c(rep(99,16),2,99,5,2,99,99,99,99,10,8)  # first age considered as large fish (greather than 40 cm)
 }


if (my.area=='North Sea' & KeyRunYear==2014) {
  # 1 Fulmar 
  # 2 Guillemot 
  # 3 Her. Gull 
  # 4 Kittiwake 
  # 5 GBB. Gull 
  # 6 Gannet 
  # 7 Puffin 
  # 8 Razorbill 
  # 9 R. radiata 
  # 10 G. gurnards 
  # 11 W. mackerel 
  # 12 N. mackerel 
  # 13 W.horse mac 
  # 14 N.horse mac 
  # 15 Grey seal 
  # 16 H. porpoise 
  # 17 Hake 
  # 18 Cod  (17)
  # 19 Whiting (18)
  # 20 Haddock (19)
  # 21 Saithe  (20)
  # 22 Herring (21)
  # 23 Northern Sandeel 
  # 24 Southern Sandeel
  # 25 Nor. pout (23)
  # 26 Sprat (24)
  # 27 Plaice (25)
  # 28 Sole  (26)
  

 
  MSFD@do.community.biomass.demersal<-1
  MSFD@community.biomass.demersal.ages['first.age' ,c(1:8,11:16,22,26)]<-  99  # exclude    
  MSFD@community.biomass.demersal.ages['last.age'  ,c(1:8,11:16,22,26)]<-  -1  # exclude 
  
  MSFD@do.community.biomass.small<-0  # not ready yet!!!!!!!!!!!!!!!!!!!!!!!
  MSFD@community.biomass.small.ages['last.age',c(1:8,11:17,22,26)]<- -1     # exclude    NEEDS UPDATE
  
  MSFD@do.community.biomass.pelagic<-1 
  MSFD@community.biomass.pelagic.ages['first.age',c(1:20,22:23,22:28)]<- 99     # exclude
  MSFD@community.biomass.pelagic.ages['last.age',c(1:20,22:23,25:26)]<- -1     # exclude
  
  MSFD@do.community.biomass.forage<-1 
  MSFD@community.biomass.forage.ages['first.age',c(1:21,23:25,27:28)]<- 99     # exclude
  MSFD@community.biomass.forage.ages['last.age',c(1:21,23:25,27:28)]<- -1     # exclude

  MSFD@do.M2.bar<-1 
  MSFD@M2.bar.ages['first.age',]<- c(0,0,0,-1,0,0,0,0,0,-1,-1)     # all ages for forage species, and >0.2 for other species
  MSFD@M2.bar.ages['last.age',] <- c(2,8,2,-1,3,4,4,3,3,-1,-1)     # all ages for forage species, and >0.2 for other species
  
  MSFD@do.community.mean.weight <-1 
  MSFD@community.mean.weight.sp[]<-1  #include all species
  
  MSFD@do.community.mean.weight.C <-1 
  MSFD@community.mean.weight.C.sp[]<-1  #include all species
  
  MSFD@do.mean.weight.C <-0 
  MSFD@mean.weight.C.sp[]<-1    #include all species       
  
  MSFD@do.community.F<-1
  MSFD@community.F.sp[]<-1   #include all species
  MSFD@do.community.M<-1
  MSFD@community.M.sp[]<-1   #include all species
  
  MSFD@do.life.expectancy <-1
  MSFD@life.expectancy.first.age[]<-0
  
  MSFD@do.community.life.expectancy<-1 
  MSFD@community.life.expectancy.options["first.age",]<-0   
  MSFD@community.life.expectancy.options["weighting",]<-1         
  
  MSFD@do.LFI<-1
  # 1 Fulmar 
  # 2 Guillemot 
  # 3 Her. Gull 
  # 4 Kittiwake 
  # 5 GBB. Gull 
  # 6 Gannet 
  # 7 Puffin 
  # 8 Razorbill 
  # 9 R. radiata 
  # 10 G. gurnards 
  # 11 W. mackerel 
  # 12 N. mackerel 
  # 13 W.horse mac 
  # 14 N.horse mac 
  # 15 Grey seal 
  # 16 H. porpoise 
  # 17 Hake 
  # 18 Cod  (17)
  # 19 Whiting (18)
  # 20 Haddock (19)
  # 21 Saithe  (20)
  # 22 Herring (21)
  # 23 Northern Sandeel 
  # 24 Southern Sandeel
  # 25 Nor. pout (23)
  # 26 Sprat (24)
  # 27 Plaice (25)
  # 28 Sole  (26)
  
  
  MSFD@LFI.sp[ ,c(1:17,19,22:26)]<- 0  # exclude  
  MSFD@LFI.age[]<-c(rep(99,17),2,99,5,2,99,99,99,99,99,10,8)  # first age considered as large fish (greather than 40 cm)
}
 write.FLOP.MSFD.control(MSFD,file="op_msfd.dat") 
}



if (change.Other.pred.N) {
    othN<-scan(file=file.path(data.path,'op_other_n.in'),comment.char = "#",quiet = T )
    dim(othN)<-c(SMS@max.age.all+1,n.other.pred,SMS@last.season)
    dimnames(othN)=list( paste('a_',(0:SMS@max.age.all),sep=''),
                     sp.names[1:n.other.pred],
                     paste('q_',(1:SMS@last.season),sep=''))
}
                      

######### end user options #########


if (do.simulation) {
   
   if (file.exists(scenario.dir) & ask.me) {
       cat("Are you sure you want to make new simultaion for scenario\n ",scenario,"\n The directory exist and will be overwritten!\n Do you want to continue? (Y/N):")
       a<-readLines(n=1)
       if(a=='N' || a=='n') stop("Script stopped'") 
   }
    
  if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
  dir.create(scenario.dir,showWarnings = FALSE)

  cat(first.no.iter.stoch,'\n', file=file.path(scenario.dir,"op_seed.in"))
  
  n.runs<-dim(targetFs)[[1]]
  cat(n.runs,'\n', file=file.path(scenario.dir,"op_multargetf.in"))
  write.table(targetFs,row.names=F,col.names=F,file=file.path(scenario.dir,"op_multargetf.in"),append=T)


  OP.files<-c(op.command,"area_names.in","species_names.in","op.dat","op_trigger.dat","op_config.dat","op_msfd.dat","just_one.in",
      "op_consum.in","op_f.in","op_m1.in","op_m.in","op_n.in","op_propmat.in","op_prop_landed.in","op_size.in","op_wcatch.in","op_wsea.in",
      "op_growth_type1.in","op_consum_ab.in","op_other_n.in","op_exploitation.in","op_reference_points.in","covariance_rec.in","op_price.in",
      "op_ssb_rec_residuals.in","op_length_weight_relations.in",'op_eqsim.in','op_eqsim_stoch.in','op_n_proportion_m2.in')

  if (SMS@no.areas>1)  OP.files<-c(OP.files,"op_Stock_distribution.in")
  
  for (from.file in OP.files) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  sp.name<-SMS.control@species.names
 
  setwd(scenario.dir)
  
  if (adjust.ini.N) {
   ini.N<-matrix( scan(file="op_n.in",comment.char="#",quiet = T),nrow=n.vpa,byrow=T)
   #ini.N[1,]<-ini.N[1,]*2  # cod
   write.matrix(ini.N,file="op_n.in")
  }

  tot.run<-dim(targetFs)[[1]]
  fsp<-dim(targetFs)[[2]]  # number of species with variable F target

  if (ask.me) {
    cat("You have asked for",tot.run," runs with",no.iter,"iterations.\nDo you want to continue? (Y/N):")
    a<-readLines(n=1)
    if(a=='N') stop("Script stopped'")
    cat('OK\n')
  }
  MAMsp<-c("Grey seal","H. porpoise")
  MACsp<-c("W. mackerel","N. mackerel")
  BIRsp<-c("Fulmar","Guillemot","Her. Gull","Kittiwake","GBB. Gull","Gannet","Puffin","Razorbill")     
  
  #run OP command
  comm.sms<-paste( file.path(scenario.dir,op.command),"-maxfn 0 -nohess ",sep=" ")

  if (just.batch.file) {
    if (OS=='windows') bat.file<-"run_OP.bat"  else if (OS=='unix') bat.file<-"run_OP.sh" 
    if (OS=='windows') {
       cat("cd ",scenario.dir,"\n",comm.sms,' >ud.dat\n',file=file.path(scenario.dir,bat.file))
        #stop(paste("go to directory",scenario.dir,"and run script ",bat.file,"\n"))
       system(command=file.path(scenario.dir,bat.file), wait = F)
    }
    if (OS=='unix' & HPC ) {
      #run OP command
      comm.sms<-paste( op.command,"-maxfn 0 -nohess ",sep=" ")
      
      cat(paste(
      "#!/bin/sh",
      "# embedded options to qsub - start with #PBS",
      "# -- Name of the job ---",
      "#PBS -N SMS",
      "# –- specify queue --",
      "#PBS -q hpc",
      "# -- estimated wall clock time (execution time): hh:mm:ss --",
      paste("#PBS -l walltime=",formatC(round((n.runs/8000)+1), format="f",digits=0,w=2,flag='0'),":00:00",sep=''),
      "# –- number of processors/cores/nodes --",
      "#PBS -l nodes=1:ppn=1",
      "# –- user email address --",
      "# - PBS -M mv@aqua.dtu.dk",
      "# –- mail notification –-",
      "# - PBS -m abe",
      paste("cd ",scenario.dir,sep=''),
      paste(comm.sms," > ud.dat","\n"),
      sep='\n'),file=file.path(scenario.dir,bat.file))    
    }

  } else  {
    if (OS=='windows') shell(comm.sms, invisible = TRUE)
    cat("\ncommand: ",comm.sms,'\n')
    if (OS=='unix') system(comm.sms)
  }
}   # end do.simulations

setwd(data.path)                                        


if (!just.batch.file) {
  if (my.area=='North Sea' & KeyRunYear==2000)  spNames<-c('COD', 'WHG', 'HAD', 'POK', 'HER', 'SAN', 'NOR', 'SPR', 'PLE', 'SOL')
  if (my.area=='North Sea' & KeyRunYear==2014)  spNames<-c('COD', 'WHG', 'HAD', 'POK', 'HER', 'NSA','SSA', 'NOR', 'SPR', 'PLE', 'SOL')
  if (my.area=='North Sea' & KeyRunYear==2017)  spNames<-c('COD', 'WHG', 'HAD', 'POK', 'MAC', 'HER', 'NSA','SSA', 'NOR', 'SPR', 'PLE', 'SOL')
  if (my.area=='North Sea' & KeyRunYear==2020)  spNames<-c('COD', 'WHG', 'HAD', 'POK', 'MAC', 'HER', 'NSA','SSA', 'NOR', 'SPR', 'PLE', 'SOL')
  
  
    if (my.area=='Baltic Sea') spNames<-c('COD', 'HER', 'SPR')
  
  n.run<-dim(targetFs)[[1]]
  
  
  if (n.run >1) {
    if (read.condense) {
      cat('reading condensed data from directory:  ', scenario.dir,'\n')
      setwd(scenario.dir)
      condensed<-read.table('op_average_val.out',header=F)
      allnames<-c(c('value','yield', 'CWsum', 'Fcomb','Fbar', 'SSB', 'TSB', 'recruit','belowBlim','belowBpa', 'Species.n', 'run','iteration'),spNames)
      dimnames(condensed)[[2]]<-allnames
      save(condensed, file =file.path(scenario.dir, "condensed.RData"))
      setwd(data.path)
    } else load(file =file.path(scenario.dir, "condensed.RData"),verbose=T)
  
   if (read.condense) {     # f mult combinations
      setwd(scenario.dir)
      nr<-scan( "op_multargetf.in", nmax = 1,quiet = T)
      Fcomb<-data.frame(matrix(scan( "op_multargetf.in",skip=1),nrow=nr,ncol=length(spNames),byrow=T))
      dimnames(Fcomb)[[2]]<-spNames 
      Fcomb$run=first.run.no:(nr+first.run.no-1)
      save(Fcomb, file =file.path(scenario.dir, "Fcomb.RData"))
      setwd(data.path)
    } else load(file =file.path(scenario.dir, "Fcomb.RData"))
    
   if (read.indicators ) {
      setwd(scenario.dir)
      indi<-read.table( "op_indicator_system_avg.out",header=T)
      
      load(file =file.path(scenario.dir, "Fcomb.RData"),verbose=T)
       indi<-merge(indi,Fcomb)
      
      if  (my.area=='North Sea') {
        if (KeyRunYear==2014) indi$unique.id<-paste(indi$run,indi$iter,round(indi$COD,3),round(indi$WHG,3),round(indi$HAD,3),round(indi$POK,3),round(indi$HER,3),round(indi$NSA,3),round(indi$SSA,3),round(indi$NOR,3),round(indi$SPR,3),round(indi$PLE,3),round(indi$SOL,3),sep='')
        if (KeyRunYear==2017) indi$unique.id<-paste(indi$run,indi$iter,round(indi$COD,3),round(indi$WHG,3),round(indi$HAD,3),round(indi$POK,3),round(indi$MAC,3),round(indi$HER,3),round(indi$NSA,3),round(indi$SSA,3),round(indi$NOR,3),round(indi$SPR,3),round(indi$PLE,3),round(indi$SOL,3),sep='')
        if (KeyRunYear==2020) indi$unique.id<-paste(indi$run,indi$iter,round(indi$COD,3),round(indi$WHG,3),round(indi$HAD,3),round(indi$POK,3),round(indi$MAC,3),round(indi$HER,3),round(indi$NSA,3),round(indi$SSA,3),round(indi$NOR,3),round(indi$SPR,3),round(indi$PLE,3),round(indi$SOL,3),sep='')
        
        } else if  (my.area=='Baltic Sea') {
        indi$unique.id<-paste(indi$run,indi$iter,round(indi$COD,3),round(indi$HER,3),round(indi$SPR,3),sep='')
      }

      # get SSB and yield from the condensed data set
      if  (my.area=='North Sea') {
        if (KeyRunYear==2014) b<-aggregate(cbind(value,yield,CWsum,SSB,TSB)~  run+iteration+COD+WHG+HAD+POK+HER+NSA+SSA+NOR+SPR+PLE+SOL,data=condensed,sum)
        if (KeyRunYear==2017) b<-aggregate(cbind(value,yield,CWsum,SSB,TSB)~  run+iteration+COD+WHG+HAD+POK+MAC+HER+NSA+SSA+NOR+SPR+PLE+SOL,data=condensed,sum)
        if (KeyRunYear==2020) b<-aggregate(cbind(value,yield,CWsum,SSB,TSB)~  run+iteration+COD+WHG+HAD+POK+MAC+HER+NSA+SSA+NOR+SPR+PLE+SOL,data=condensed,sum)
        
        if (KeyRunYear==2014) b$unique.id<-paste(b$run,b$iteration,round(b$COD,3),round(b$WHG,3),round(b$HAD,3),round(b$POK,3),round(b$HER,3),round(b$NSA,3),round(b$SSA,3),round(b$NOR,3),round(b$SPR,3),round(b$PLE,3),round(b$SOL,3),sep='')
        if (KeyRunYear==2017) b$unique.id<-paste(b$run,b$iteration,round(b$COD,3),round(b$WHG,3),round(b$HAD,3),round(b$POK,3),round(b$MAC,3),round(b$HER,3),round(b$NSA,3),round(b$SSA,3),round(b$NOR,3),round(b$SPR,3),round(b$PLE,3),round(b$SOL,3),sep='')
        if (KeyRunYear==2020) b$unique.id<-paste(b$run,b$iteration,round(b$COD,3),round(b$WHG,3),round(b$HAD,3),round(b$POK,3),round(b$MAC,3),round(b$HER,3),round(b$NSA,3),round(b$SSA,3),round(b$NOR,3),round(b$SPR,3),round(b$PLE,3),round(b$SOL,3),sep='')
        b<-subset(b,select=c(unique.id,value,yield,CWsum,SSB,TSB))
        indi<-merge(indi,b,by="unique.id")
        indi$unique.id<-NULL
      }  else if  (my.area=='Baltic Sea') {
        b<-aggregate(cbind(value,yield,CWsum,SSB,TSB)~  run+iteration+COD+HER+SPR,data=condensed,sum)
        b$unique.id<-paste(b$run,b$iteration,round(b$COD,3),round(b$HER,3),round(b$SPR,3),sep='')
        b<-subset(b,select=c(unique.id,value,yield,CWsum,SSB,TSB))
        indi<-merge(indi,b,by="unique.id")
        indi$unique.id<-NULL
      } 
      save(indi, file =file.path(scenario.dir, "indicators.RData"))
      setwd(data.path)
    } else load(file =file.path(scenario.dir, "indicators.RData"),verbose=T)
  
    if (combine.scenario.run) {
      all.condensed<-NULL
      for (sc in scenario.dirs) {
        load(file =file.path(data.path,sc, "condensed.RData"))  #load condensed data set
        cat(sc,dim(condensed),"\n")
        all.condensed<-rbind(all.condensed,condensed)
        rm(condensed)
      }
      condensed<-all.condensed
      rm(all.condensed)
      save(condensed, file =file.path(data.path,scenario.dirs.out, "condensed.RData"))
      
      # F combinations
      all.Fcomb<-NULL
      for (sc in scenario.dirs) {
        load(file =file.path(data.path,sc, "Fcomb.RData"))  #load condensed data set
        cat(sc,dim(Fcomb),"\n")
        all.Fcomb<-rbind(all.Fcomb,Fcomb)
        rm(Fcomb)
      }
      Fcomb<-all.Fcomb
      rm(all.Fcomb)
      save(Fcomb, file =file.path(data.path,scenario.dirs.out, "Fcomb.RData"))
      
      # indicators 
      all.indi<-NULL
      for (sc in scenario.dirs) {
        load(file =file.path(data.path,sc, "indicators.RData"))  #load indi data set
        cat(sc,dim(indi),"\n")
        all.indi<-rbind(all.indi,indi)
        rm(indi)
      }
      indi<-all.indi
      rm(all.indi)
      save(indi, file =file.path(data.path,scenario.dirs.out, "indicators.RData"))
    }
    
    if (read.condense | combine.scenario.run) {
      source(file.path(prog.path.func,"hcr_op_condense_function.r"))
      a<-transform.condensed(a=condensed,my.area=my.area)

      save(a, file =file.path(scenario.dir, "a.RData"));  
    } else load(file =file.path(scenario.dir, "a.RData"))

  }  # end combine.scenario.run
  
  

  
  if ((do.plot.condense | ReduceTable ) & n.run >1) {
  
    if (my.area=='North Sea') {
        if (subset.out) { # make a subset of data set used to make plots
         if (KeyRunYear==2014) {
           #COD WHG HAD POK HER NSA SSA NOR SPR PLE SOL 
           bb<-subset.species
           a<-droplevels(subset(a, COD>=bb[1,1] &  COD<=bb[2,1] & 
                                  WHG>=bb[1,2] &  WHG<=bb[2,2] & 
                                  HAD>=bb[1,3] &  HAD<=bb[2,3] & 
                                  POK>=bb[1,4] &  POK<=bb[2,4] & 
                                  HER>=bb[1,5] &  HER<=bb[2,5] & 
                                  NSA>=bb[1,6] &  NSA<=bb[2,6] & 
                                  SSA>=bb[1,7] &  SSA<=bb[2,7] & 
                                  NOR>=bb[1,8] &  NOR<=bb[2,8] & 
                                  SPR>=bb[1,9] &  SPR<=bb[2,9] & 
                                  PLE>=bb[1,10] &  PLE<=bb[2,10] & 
                                  SOL>=bb[1,11] & SOL<=bb[2,11] )) 
         }
          if (KeyRunYear==2017) {
            #COD WHG HAD POK MAC HER NSA SSA NOR SPR PLE SOL 
            bb<-subset.species
            a<-droplevels(subset(a, COD>=bb[1,1] &  COD<=bb[2,1] & 
                                   WHG>=bb[1,2] &  WHG<=bb[2,2] & 
                                   HAD>=bb[1,3] &  HAD<=bb[2,3] & 
                                   POK>=bb[1,4] &  POK<=bb[2,4] & 
                                   MAC>=bb[1,5] &  MAC<=bb[2,5] & 
                                   HER>=bb[1,6] &  HER<=bb[2,6] & 
                                   NSA>=bb[1,7] &  NSA<=bb[2,7] & 
                                   SSA>=bb[1,8] &  SSA<=bb[2,8] & 
                                   NOR>=bb[1,9] &  NOR<=bb[2,9] & 
                                   SPR>=bb[1,10] &  SPR<=bb[2,10] & 
                                   PLE>=bb[1,11] &  PLE<=bb[2,11] & 
                                   SOL>=bb[1,12] & SOL<=bb[2,12] )) 
          }  
          if (KeyRunYear==2020) {
            #COD WHG HAD POK MAC HER NSA SSA NOR SPR PLE SOL 
            bb<-subset.species
            a<-droplevels(subset(a, COD>=bb[1,1] &  COD<=bb[2,1] & 
                                   WHG>=bb[1,2] &  WHG<=bb[2,2] & 
                                   HAD>=bb[1,3] &  HAD<=bb[2,3] & 
                                   POK>=bb[1,4] &  POK<=bb[2,4] & 
                                   MAC>=bb[1,5] &  MAC<=bb[2,5] & 
                                   HER>=bb[1,6] &  HER<=bb[2,6] & 
                                   NSA>=bb[1,7] &  NSA<=bb[2,7] & 
                                   SSA>=bb[1,8] &  SSA<=bb[2,8] & 
                                   NOR>=bb[1,9] &  NOR<=bb[2,9] & 
                                   SPR>=bb[1,10] &  SPR<=bb[2,10] & 
                                   PLE>=bb[1,11] &  PLE<=bb[2,11] & 
                                   SOL>=bb[1,12] & SOL<=bb[2,12] )) 
          }  
          
        }
    
        if (singleSp) {
            png(filename =file.path(used.scenario.dir,paste('single','.png',sep='')), width = 1600, height = 1200,units = "px", pointsize = 18, bg="white")
          sel_species<- 16:25
          par(mfcol=c(6,length(sel_species)))
          par(mar=c(2,4,3,2)) # c(bottom, left, top, right)
          a<-subset(a,Species.n %in% sel_species)

          ref<-Read.reference.points.OP(dir=data.path) 
          boxs <- defmacro(a, varsp, spname,spno,
                           expr={
                             boxplot(yield/1000~varsp ,data=a,show.names = T,ylab='Yield (1000t)',subset=Species.n==spno,main=spname)
                             boxplot(SSB/1000~varsp ,data=a,show.names = T,ylab='SSB (1000t)',subset=Species.n==spno)
                             abline(h=ref[spno-17,'Bpa']/1000,col='blue',lwd=2)
                             abline(h=ref[spno-17,'Blim']/1000,col='red',lwd=2)
                             
                             boxplot(Fbar~varsp ,data=a,show.names = T,ylab='Realized F',xlab='F',subset=Species.n==spno)     
                             boxplot(belowBlim~varsp ,data=a,show.names = T,ylab='prob(SSB<Blim) %',xlab='F',subset=Species.n==spno)
                             boxplot(belowBpa~varsp ,data=a,show.names = T,ylab='prob(SSB<Bpa) %',xlab='F',subset=Species.n==spno)
                             boxplot(recruit/1E6~varsp ,data=a,show.names = T,ylab='recruits (10^9)',xlab='F',subset=Species.n==spno)
                           })
          
          if (KeyRunYear==2014) {
            boxs(a, COD, 'Cod',18)
            boxs(a, WHG, 'Whiting',19)
            boxs(a, HAD, 'Haddock',20)
            boxs(a, POK, 'Saithe',21)
            boxs(a, HER, 'Herring',22)
            boxs(a, NSA, 'Northern sandeel',23)
            boxs(a, SSA, 'Southern sandeel',24)
            boxs(a, NOR, 'Norway pout',25)
            boxs(a, SPR, 'Sprat',26)
            cleanup()
          } else  if (KeyRunYear %in% c(2017,2020)) {
            print(head(a))
            print(summary(a))
            aa<<-a
            boxs(a, COD, 'Cod',16)
            boxs(a, WHG, 'Whiting',17)
            boxs(a, HAD, 'Haddock',18)
            boxs(a, POK, 'Saithe',19)
            boxs(a, MAC, 'Mackerel',20)
            boxs(a, HER, 'Herring',21)
            boxs(a, NSA, 'Northern sandeel',22)
            boxs(a, SSA, 'Southern sandeel',23)
            boxs(a, NOR, 'Norway pout',24)
            boxs(a, SPR, 'Sprat',25)
            cleanup()
          }
          trellis.device(device = png,filename =file.path(used.scenario.dir,paste('single_rec','.png',sep='')),
                         width = 1600, height = 1200,units = "px", pointsize = 25)
          print(head(a))
            
          print(xyplot(recruit/1000000~SSB/1000 |Species.n,data=a,
                 scales=list(relation='free'),
                 ylab='Recruits (billions)',xlab="SSB (1000 t)",
                 panel = function(x, y) {
                   panel.xyplot(x, y)
                   panel.loess(x, y, span=0.01,lwd=5,col='red')
                 }))
          
          cleanup()
        }
      
        do.plot.condense<-!singleSp  # do not make the usual plots if single species
      
        out.file<-'all'
        out.types<-c('Yield','SSB','Recruit','Fbar','belowBlim', 'belowBpa','riskBlim')
   aaa<-a
        source(file.path(prog.path.func,"fmsy_matrix_functions_plots.r"))
        if (do.plot.condense) {
          incl.sp.no<-(nsp-nsp.VPA+1):(nsp-2) # all vpa species except PLE and Sole
          spNames<-sp.names[incl.sp.no] 
          a<-subset(a,Species.n %in% incl.sp.no)
          
          cat("####### spnmes   #####################################  \n", spNames,"\n")
          spNames<-gsub('N. sandeel','NSa',spNames)
          spNames<-gsub('S. sandeel','SSa',spNames)
          spNames<-gsub('Nor. pout','NPo',spNames)
          if (subset.out) add.fn<-'_Reduc' else add.fn<-'' 
          cat("####### spnmes   #####################################  \n", spNames,"\n")
          print(head(a))
          print(summary(a))
          
          plot.matrix(tit='Yield',type=out.types[1],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn, scenario.dir=scenario.dir,a=a) 
          plot.matrix(tit='SSB',  type=out.types[2],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn,scenario.dir=scenario.dir,a=a)
          plot.matrix(tit='SSB',  type=out.types[2],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn,scenario.dir=scenario.dir,a=a,plot.ref=T)
          plot.matrix(tit='Recruits', type=out.types[3],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn,scenario.dir=scenario.dir,a=a)
          #plot.matrix(tit='Average F', type=out.types[4],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn,scenario.dir=scenario.dir,a=a)
          #plot.matrix(tit='Probability (%) of SSB below Blim', type=out.types[5],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn,scenario.dir=scenario.dir,a=a)          
          plot.matrix(tit='Probability (%) of SSB below Bpa', type=out.types[6],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn,scenario.dir=scenario.dir,a=a)          
          #plot.matrix(tit='Proportion (%) of scenarios with prob(SSB<Blim) > 5%', type=out.types[7],spNames=spNames,my.dev='png',out.file=out.file,add.to.file.name=add.fn,scenario.dir=scenario.dir,a=a)

          cat("\n\nRelative MSY:\n")
        }
        table.MSY(a,out.file=out.file,scenario.dir=scenario.dir)
        cat('\ntable 01 done\n')
        cat('justAboveBlim:');
        cat(justAboveBlim,'\n')   
        if (justAboveBlim & do.plot.condense) {
           cat('\nPlots, justAboveBlim\n')
           cat("\njustAboveBlim\n")

           dropKombBlim<-subset(a,probBelowBlim>riskLevels,select=c(run,iteration)) 
            
           if (dim(dropKombBlim)[[1]]>0) {
             dropKombBlim$drop<-T 
             dropKombBlim<-unique(dropKombBlim)
             MV.dropKombBlim<<-dropKombBlim
             
             a<-merge(x=a,y=dropKombBlim,all.x=T)
             a[is.na(a$drop),'drop']<-F
             a<-droplevels(subset(a,drop==F,select=c(-drop)))
             if (dim(a)[[1]]>1)  {
               out.file='aboveBlim'
               out.types<-c('Yield','SSB','Recruit','Fbar','belowBlim', 'belowBpa')
              source(file.path(prog.path.func,"FMSY_matrix_functions.r"))
              plot.matrix(tit='Yield',type=out.types[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a) 
              plot.matrix(tit='SSB',  type=out.types[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              plot.matrix(tit='Recruits', type=out.types[3],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              plot.matrix(tit='Average F', type=out.types[4],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              plot.matrix(tit='Probability (%) of SSB below Blim', type=out.types[5],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              cat("\n\nRelative MSY:\n")
              table.MSY(a,out.file=out.file,scenario.dir=scenario.dir)
             } else cat("\n\nAll combinations below Blim, no plotting done\n\n") 
           } 
         }

        if (justAboveBpa & do.plot.condense) {
           cat('\nPlots, justAboveBlpa\n')
           a$probBelowBpa<-a$belowBpa/years.in.average
           dropKomb<-subset(a,probBelowBpa>0.05,select=c(run,iteration)) 
            
           if (dim(dropKomb)[[1]]>0) {
             dropKomb$drop<-T 
             dropKomb<-unique(dropKomb)
       
             a<-merge(x=a,y=dropKomb,all.x=T)
             a[is.na(a$drop),'drop']<-F
             a<-droplevels(subset(a,drop==F,select=c(-drop)))
            if (dim(a)[[1]]>1)  {
               out.file='aboveBpa'
               out.types<-c('Yield','SSB','Recruit','Fbar','belowBlim', 'belowBpa')
      
              source(file.path(prog.path.func,"fmsy_matrix_functions.r"))
              plot.matrix(tit='Yield',type=out.types[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a) 
              plot.matrix(tit='SSB',  type=out.types[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              plot.matrix(tit='Recruits', type=out.types[3],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              plot.matrix(tit='Average F', type=out.types[4],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              plot.matrix(tit='Probability (%) of SSB below Blim', type=out.types[5],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a)
              cat("\n\nRelative MSY:\n")
              table.MSY(a,out.file=out.file,scenario.dir=scenario.dir)
             } else cat("\n\nAll combinations below Bpa, no plotting done\n\n") 
           } 
         }

           
           
       
    }  # end North Sea
     

    if (my.area=='Baltic Sea') {
       source(file.path(prog.path.func,"fmsy-matrix-baltic.r"))
    } # end Baltic Sea
    
    if (ReduceTable & ReduceOption=="default") {
           source(file.path(prog.path.func,"fmsy_matrix_functions_tables.r"))
           cat('\nStart to reduce Table\n')
           
          load(file =file.path(scenario.dir, "a.RData"))  # to get a fresh, non reduced version
           aa<-a
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombMSYfinal(aa,out.file='redu_MSY.out',app=(ii!=0),scenario.dir=scenario.dir,keepPro=keepPro); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='redu_MSY_detail',scenario.dir=scenario.dir,app=(ii!=1))
           }
           cat("\n",dim(aa),"\n")
           table.MSY(aa,out.file="redu_MSY_final",scenario.dir=scenario.dir)
           rm(a2)
           #rm(aa)

           # risk to Blim
           # aa<-a          Do not start with the full datat set
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombRiskfinal(aa,out.file='redu_MSY_and_Blim_continue.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='redu_MSY_and_Blim_continue_details',scenario.dir=scenario.dir,app=(ii!=1))
           }
           table.MSY(aa,out.file='redu_MSY_and_Blim_continue_final',scenario.dir=scenario.dir)
           rm(a2)
           rm(aa)
           
           # risk to Blim
           aa<-a          # start with the full datat set
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombRiskfinal(aa,out.file='redu_Blim_all.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='redu_Blim_all_details',scenario.dir=scenario.dir,app=(ii!=1))
           }
           table.MSY(aa,out.file='redu_Blim_all_final',scenario.dir=scenario.dir)
           rm(a2)
           rm(aa)

           # risk to Blim , using the 
           aa<-a          # start with the full datat set
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombRiskfinal(aa,out.file='redu_Blim_v2_all.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels,type=2); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='redu_Blim_v2_all_details',scenario.dir=scenario.dir,app=(ii!=1))
           }
           table.MSY(aa,out.file='redu_Blim_v2_all_final',scenario.dir=scenario.dir)
           rm(a2)
           rm(aa)
        }
    
   }
 
   if (ReduceTable & ReduceOption=="option12") {
    source(file.path(prog.path.func,"fmsy_matrix_functions_tables.r"))
    cat('\nStart to reduce Table Option12 \n')
    
    load(file =file.path(scenario.dir, "a.RData"))  # to get a fresh, non reduced version
    aa<-a # start with the full datat set
    
    # risk to Blim, preys
    
    goOn<<-T; 
    ii<-0
    preys<-seq(1:nsp)[SMS.control@species.info[,'prey']==1 & SMS.control@species.info[,'predator']==0]-(nsp-nsp.VPA)
    predators<-seq(1:nsp)[SMS.control@species.info[,'predator']==1 ] -(nsp-nsp.VPA)
    while (goOn) {
      a2<-reduceCombRiskfinal(aa,out.file='option1_A1.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels,excludeSp=predators); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option1_details_1',scenario.dir=scenario.dir,app=(ii!=1))
    }
    table.MSY(aa,out.file='option1_B1',scenario.dir=scenario.dir)
 
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombRiskfinal(aa,out.file='option1_A2.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels,excludeSp=0); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option1_details2',scenario.dir=scenario.dir,app=(ii!=1))
    }
    table.MSY(aa,out.file='option1_B2',scenario.dir=scenario.dir)
 
    if (saveOutput) {
      load(file =file.path(scenario.dir, "indicators.RData"),verbose=T)
      key1<-unique(data.frame(run=aa$run))
      indiNew<-merge(key1,indi,by='run')
      save(aa, file =file.path(scenario.dir, "option1.RData"))
      save(indiNew, file =file.path(scenario.dir, "indi1.RData"))
     } 
    
    
    
    a3<-aa # save the set for option 3
    
    # option 2, reduce to 95% MSY
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombMSYfinal(aa,out.file='option2_A1.out',app=(ii!=0),scenario.dir=scenario.dir,keepPro=keepPro); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option2_detail',scenario.dir=scenario.dir,app=(ii!=1))
    }
    cat("\n",dim(aa),"\n")
    table.MSY(aa,out.file="option2_B1",scenario.dir=scenario.dir)
    
    # Option2, and remove SSB below Blim
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombRiskfinal(aa,out.file='option2_A2.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels,excludeSp=0); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option2_details2',scenario.dir=scenario.dir,app=(ii!=1))
    }
    table.MSY(aa,out.file='option2_B2',scenario.dir=scenario.dir)
    if (saveOutput) {
      key1<-unique(data.frame(run=aa$run))
      indiNew<-merge(key1,indi,by='run')
      save(aa, file =file.path(scenario.dir, "option2.RData"))
      save(indiNew, file =file.path(scenario.dir, "indi2.RData"))
    } 
    
    
    # option 3, reduce to 50% MSY
    aa<-a3; rm(a3)  # use data from option 1
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombMSYfinal(aa,out.file='option3_A1.out',app=(ii!=0),scenario.dir=scenario.dir,keepPro=0.50); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option3_details1',scenario.dir=scenario.dir,app=(ii!=1))
    }
    cat("\n",dim(aa),"\n")
    table.MSY(aa,out.file="option3_B1",scenario.dir=scenario.dir)
    
    # Option3, and remove SSB below Bpa (begn with preys)
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombRiskfinal(aa,out.file='option3_A2.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels,excludeSp=predators,type=3); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option3_details2',scenario.dir=scenario.dir,app=(ii!=1))
    }
    table.MSY(aa,out.file='option3_B2',scenario.dir=scenario.dir)
   
    # Options3, and remove SSB below Bpa (for all species now)
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombRiskfinal(aa,out.file='option3_A3.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels,excludeSp=0,type=3); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option3_details3',scenario.dir=scenario.dir,app=(ii!=1))
    }
    table.MSY(aa,out.file='option3_B3',scenario.dir=scenario.dir)
    
    # option 3, and reduce to 95% MSY
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombMSYfinal(aa,out.file='option3_A4.out',app=(ii!=0),scenario.dir=scenario.dir,keepPro=0.95); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option3_details4',scenario.dir=scenario.dir,app=(ii!=1))
    }
    cat("\n",dim(aa),"\n")
    table.MSY(aa,out.file="option3_B4",scenario.dir=scenario.dir)
    if (saveOutput) {
      key1<-unique(data.frame(run=aa$run))
      indiNew<-merge(key1,indi,by='run')
      save(aa, file =file.path(scenario.dir, "option3.RData"))
      save(indiNew, file =file.path(scenario.dir, "indi3.RData"))
    } 
  
     
    # option 4, and reduce to 99.9% MSY
    goOn<<-T; 
    ii<-0
    while (goOn) {
      a2<-reduceCombMSYfinal(aa,out.file='option4_A1.out',app=(ii!=0),scenario.dir=scenario.dir,keepPro=0.99999); 
      if (goOn) aa<-a2
      ii<-ii+1 
      if (goOn & ReduceTableDetails) table.MSY(aa,out.file='option4_details1',scenario.dir=scenario.dir,app=(ii!=1))
    }
    cat("\n",dim(aa),"\n")
    table.MSY(aa,out.file="option4_B1",scenario.dir=scenario.dir)
    if (saveOutput) {
      key1<-unique(data.frame(run=aa$run))
      indiNew<-merge(key1,indi,by='run')
      save(aa, file =file.path(scenario.dir, "option4.RData"))
      save(indiNew, file =file.path(scenario.dir, "indi4.RData"))
    } 
    
    rm(a2)
    rm(aa)
    
   
  
} # end option12
   
  if (do.plot.indicators & n.run>1) {
      if (my.area=='North Sea') {
        
        if (subset.out) { # make a subset of data set used to make plots
          if (KeyRunYear==2014) {
            #COD WHG HAD POK HER NSA SSA NOR SPR PLE SOL 
            bb<-subset.species
            indi<-droplevels(subset(indi, COD>=bb[1,1] &  COD<=bb[2,1] & 
                                   WHG>=bb[1,2] &  WHG<=bb[2,2] & 
                                   HAD>=bb[1,3] &  HAD<=bb[2,3] & 
                                   POK>=bb[1,4] &  POK<=bb[2,4] & 
                                   HER>=bb[1,5] &  HER<=bb[2,5] & 
                                   NSA>=bb[1,6] &  NSA<=bb[2,6] & 
                                   SSA>=bb[1,7] &  SSA<=bb[2,7] & 
                                   NOR>=bb[1,8] &  NOR<=bb[2,8] & 
                                   SPR>=bb[1,9] &  SPR<=bb[2,9] & 
                                   PLE>=bb[1,10] &  PLE<=bb[2,10] & 
                                   SOL>=bb[1,11] & SOL<=bb[2,11] )) 
          }
          if (KeyRunYear==2017) {
            #COD WHG HAD POK MAC HER NSA SSA NOR SPR PLE SOL 
            bb<-subset.species
            indi<-droplevels(subset(indi, COD>=bb[1,1] &  COD<=bb[2,1] & 
                                      WHG>=bb[1,2] &  WHG<=bb[2,2] & 
                                      HAD>=bb[1,3] &  HAD<=bb[2,3] & 
                                      POK>=bb[1,4] &  POK<=bb[2,4] & 
                                      MAC>=bb[1,5] &  MAC<=bb[2,5] & 
                                      HER>=bb[1,6] &  HER<=bb[2,6] & 
                                      NSA>=bb[1,7] &  NSA<=bb[2,7] & 
                                      SSA>=bb[1,8] &  SSA<=bb[2,8] & 
                                      NOR>=bb[1,9] &  NOR<=bb[2,9] & 
                                      SPR>=bb[1,10] &  SPR<=bb[2,10] & 
                                      PLE>=bb[1,11] &  PLE<=bb[2,11] & 
                                      SOL>=bb[1,12] & SOL<=bb[2,12] )) 
          }  
          if (KeyRunYear==2020) {
            #COD WHG HAD POK MAC HER NSA SSA NOR SPR PLE SOL 
            bb<-subset.species
            indi<-droplevels(subset(indi, COD>=bb[1,1] &  COD<=bb[2,1] & 
                                      WHG>=bb[1,2] &  WHG<=bb[2,2] & 
                                      HAD>=bb[1,3] &  HAD<=bb[2,3] & 
                                      POK>=bb[1,4] &  POK<=bb[2,4] & 
                                      MAC>=bb[1,5] &  MAC<=bb[2,5] & 
                                      HER>=bb[1,6] &  HER<=bb[2,6] & 
                                      NSA>=bb[1,7] &  NSA<=bb[2,7] & 
                                      SSA>=bb[1,8] &  SSA<=bb[2,8] & 
                                      NOR>=bb[1,9] &  NOR<=bb[2,9] & 
                                      SPR>=bb[1,10] &  SPR<=bb[2,10] & 
                                      PLE>=bb[1,11] &  PLE<=bb[2,11] & 
                                      SOL>=bb[1,12] & SOL<=bb[2,12] )) 
          }   
          
        }
        
        
        
        
         source(file.path(prog.path.func,"plot_op_community_indicators_average_northsea.r"))
          if (KeyRunYear==2014) {
            n.species<-9
            my.sp.names<-sp.names[18:26]
          } else if (KeyRunYear==2017) {
            n.species<-10
            my.sp.names<-sp.names[16:27]
          }  else if (KeyRunYear==2020) {
          n.species<-10
          my.sp.names<-sp.names[16:27]
        }  
          #a<-droplevels(subset(a,(COD<=0.4 & POK<=0.4) & SPR<0.6))
          out.file='all_'
          if (subset.out) out.file<-paste(out.file,'Reduc_',sep='')
          print(summary(indi))
          plot.matrix(tit='System indicators',spNames=my.sp.names,my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=indi)
      
         if (justAboveBlim) {
           cat("\njustAboveBlim Indicators\n")
           if (dim(dropKombBlim)[[1]]>0) {
             dropKombBlim$iter<-dropKombBlim$iteration
             indi<-merge(x=indi,y=dropKombBlim,all.x=T)
             MV.indi1<<-indi
             indi[is.na(indi$drop),'drop']<-F
             indi<-droplevels(subset(indi,drop==F,select=c(-drop)))
             MV.indi2<<-indi
             if (dim(indi)[[1]]>1)  {
               out.file='aboveBlim_'
               plot.matrix(tit='System indicators',spNames=my.sp.names,my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=indi)
             }  else cat("\n\nAll combinations below Blim, no plotting done\n\n") 
           }
         }
      }    # end North Sea
      
      if (my.area=='Baltic Sea') source(file.path(prog.path.func,"plot_op_community_indicators_average_baltic.r"))
  }
 } # end just batch
 
  used.scenario.dir<<-scenario.dir
  scenario.dir<<-used.scenario.dir
  scenario.dir.simul<<-used.scenario.dir
 
  return(scenario.gem)
}  # end function

###################################
# function for fixing POK, PLE and SOL F
POK.PL.SOL<-function(HCR=HCR1,FBlim.adjust=1,stochastic.recruitment=1,recruit.adjust.CV=0,my.last.year=2061, years.in.average=5) {  
    # find F-values for POK, PLE and Sole
    if (KeyRunYear==2014) OP.simulate(scenario="POK",HCR=HCR,FBlim.adjust=FBlim.adjust,targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.20,0.45,0.01),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.05),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    if (KeyRunYear==2017) OP.simulate(scenario="POK",HCR=HCR,FBlim.adjust=FBlim.adjust,targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.20,0.45,0.01),MAC=seq(0.20,0.20,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.05),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    if (KeyRunYear==2020) OP.simulate(scenario="POK",HCR=HCR,FBlim.adjust=FBlim.adjust,targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.20,0.45,0.01),MAC=seq(0.20,0.20,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.05),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
  
    load(file =file.path(used.scenario.dir, "condensed.RData"))
    if (KeyRunYear==2014)  POK<-subset(condensed,Species.n==21)  else POK<-subset(condensed,Species.n==19)
    POK2<<-POK
    if (KeyRunYear==2014) OP.simulate(scenario="PLE",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.10,0.60,0.025),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    if (KeyRunYear==2017) OP.simulate(scenario="PLE",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),MAC=seq(0.20,0.20,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.10,0.60,0.025),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    if (KeyRunYear==2020) OP.simulate(scenario="PLE",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),MAC=seq(0.20,0.20,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.10,0.60,0.025),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    
    load(file =file.path(used.scenario.dir, "condensed.RData"))
    if (KeyRunYear==2014) PLE<-subset(condensed,Species.n==27) else PLE<-subset(condensed,Species.n==25)
    
     
    if (KeyRunYear==2014) OP.simulate(scenario="SOL",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.025),SOL=seq(0.10,0.60,0.025)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    if (KeyRunYear==2017) OP.simulate(scenario="SOL",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),MAC=seq(0.20,0.20,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.025),SOL=seq(0.10,0.60,0.025)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    if (KeyRunYear==2020) OP.simulate(scenario="SOL",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),MAC=seq(0.20,0.20,0.05),HER=seq(0.30,0.30,0.05),NSA=seq(0.25,0.25,0.05),SSA=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.025),SOL=seq(0.10,0.60,0.025)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV, do.plot.indicators=F)
    
    
    load(file =file.path(used.scenario.dir, "condensed.RData"))
    if (KeyRunYear==2014) SOL<-subset(condensed,Species.n==28) 
    if (KeyRunYear==2017) SOL<-subset(condensed,Species.n==27) 
    if (KeyRunYear==2020) SOL<-subset(condensed,Species.n==27) 
    
    
    allSp<-rbind(POK,PLE,SOL)
   
    if (stochastic.recruitment==0  | stochastic.recruitment==2 ) {
      allSp[allSp$belowBlim>0,'belowBlim']<-100
      allSp[allSp$belowBpa>0,'belowBpa']<-100
    }
    png(filename =file.path(used.scenario.dir,paste('POK_PLE_SOL','.png',sep='')), width = 700, height = 1100,units = "px", pointsize = 18, bg="white")
    par(mfcol=c(6,3))
    par(mar=c(2,4,3,2)) # c(bottom, left, top, right)

    if (KeyRunYear==2014) POK.PLE.SOL<-c(21,27,28)
    if (KeyRunYear==2017) POK.PLE.SOL<-c(19,26,27)
    if (KeyRunYear==2020) POK.PLE.SOL<-c(19,26,27)
    
    a<-subset(allSp,Species.n %in% POK.PLE.SOL)
    boxs <- defmacro(a, varsp, spname,spno,
      expr={
          boxplot(yield/1000~varsp ,data=a,show.names = T,ylab='Yield (1000t)',subset=Species.n==spno,main=spname)
          boxplot(SSB/1000~varsp ,data=a,show.names = T,ylab='SSB (1000t)',subset=Species.n==spno)
          boxplot(Fbar~varsp ,data=a,show.names = T,ylab='Realized F',xlab='F',subset=Species.n==spno)     
          boxplot(belowBlim~varsp ,data=a,show.names = T,ylab='prob(SSB<Blim) %',xlab='F',subset=Species.n==spno)
          boxplot(belowBpa~varsp ,data=a,show.names = T,ylab='prob(SSB<Bpa) %',xlab='F',subset=Species.n==spno)
          boxplot(recruit/1E6~varsp ,data=a,show.names = T,ylab='recruits (10^9)',xlab='F',subset=Species.n==spno)
        })
     boxs(a, POK, 'Saithe',POK.PLE.SOL[1])
     boxs(a, PLE,'Plaice',POK.PLE.SOL[2])
     boxs(a, SOL,'Sole',POK.PLE.SOL[3])


    cleanup()
}

  