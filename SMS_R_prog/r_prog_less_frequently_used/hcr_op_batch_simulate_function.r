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
 
          first.run.no=1,
          first.no.iter.stoch=1,  
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
          adjust.ini.N=T,     # adjust inital population size  (NS cod)
          
          read.condense=T,
          read.indicators=T,
          
          do.plot.indicators=T,
          do.plot.condense=T,
          ReduceTable=F,      # stepwise selection table of F from given criteria
          ReduceTableDetails=F,   # aoutput full (detalied results after each reduction)
          riskLevels=c(5,5,5,5,5,5,5,5), # Risk levels for SSB below Blim
          keepPro=0.95,  # keep F values which is factor keepPro of  MSY, used in stepwise reduction
          
          subset.out=F,       # make a subset of data set used to make plots
          justAboveBlim=F,    # make plot for Fcombinations where the risk to Blim <5%
          justAboveBpa=F,
          subset.species=matrix(c(rep(0,10),rep(2,10)),ncol=10,byrow=T),  # min and max F for inclusion in plots
          ask.me=F,
          targetFs=NA,   # combinations of F
          
           my.dev='png'
){


first.year.output<-my.last.year-years.in.average+1
cat("first year used in output and in average values:",first.year.output,"\nlast year:",my.last.year,"\n")


no.iter<-1  # for determenistic recruitment
if (stochastic.recruitment>0) no.iter<-no.iter.stoch

#####################################

#  runs are made in a separate dirictory

scenario<-paste(scenario,paste("HCR",HCR[1],sep=''),FBlim.adjust[1],paste("Rec",stochastic.recruitment,sep=''),ifelse(recruit.adjust.CV[1]==1 | recruit.adjust.CV[1]==2,"Recadj",""),my.last.year,sep='_')

scenario.gem<<-scenario
scenario.dir<-file.path(data.path,scenario)


ref<-Read.reference.points.OP(dir=data.path) 
Blim<-as.vector(ref[,'Blim'])
Bpa<-as.vector(ref[,'Bpa'])
Fpa<-as.vector(ref[,'Fpa'])


if (my.area=='North Sea'){
  #             Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole 
  TSBBlim<- c(    1,          1,          1,          1,          1,     787000,     263000,      157000,          1,          1) 
  TSBBpa<-  c(    1,          1,          1,          1,          1,    1098000,     440000,      213000,          1,          1) 
} else {
   TSBBlim<- c(1,1,1)
   TSBBpa<-  c(1,1,1)
}
 

# Make an OP control object. File='OP.dat'
setwd(data.path)
source(file=file.path(prog.path,'HCR_OP_batch_common.R'))

res<-make.OP.dat(my.area=my.area,my.last.year=my.last.year,first.year.output=first.year.output,
                 do.indicators=do.indicators,
                 stochastic.recruitment=stochastic.recruitment)
OP<-res[["OP"]]
SMS<-res[["SMS"]]

sp.name<-SMS@species.names

 OP@recruit.adjust.CV[1,]<-recruit.adjust.CV
 
 
if (my.area=='Baltic Sea' & adjustBaltic) {# adjustment was done when environmental S-R were used
 OP@recruit.adjust.CV[1,1]<-0     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
 OP@recruit.adjust.CV[1,2]<-1     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
 OP@recruit.adjust.CV[1,3]<-1     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
}

OP@F.or.C[]<-31
OP@output<-OP.output

write.FLOP.control(OP,file='OP.dat',nice=T)

if (length(FBlim.adjust)==1) FBlim.adjust<-rep(1,length(Fpa))
  
### OP_trigger file
  nsp<-SMS@no.species
  n.other.pred<-sum(SMS@species.info[,'predator']==2)
  n.pred<-n.other.pred+sum(SMS@species.info[,'predator']==1)
  n.vpa<-nsp-n.other.pred
  n.vpa.pred<-sum(SMS@species.info[,'predator']==1)

OPT<-read.FLOPtrigger.control(file="OP_trigger.dat",n.VPA=n.vpa,n.other.pred=n.other.pred) 
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
write.FLOPtrigger.control(OPT,file="OP_trigger.dat") 

                                                       
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
 
 if (my.area=='North Sea') {
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
}

write.FLOP.MSFD.control(MSFD,file="OP_MSFD.dat") 

if (change.Other.pred.N) {
    othN<-scan(file=file.path(data.path,'OP_Other_N.in'),comment.char = "#",quiet = T )
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

  cat(first.no.iter.stoch,'\n', file=file.path(scenario.dir,"OP_seed.in"))
  
  n.runs<-dim(targetFs)[[1]]
  cat(n.runs,'\n', file=file.path(scenario.dir,"OP_mulTargetF.in"))
  write.table(targetFs,row.names=F,col.names=F,file=file.path(scenario.dir,"OP_mulTargetF.in"),append=T)


  OP.files<-c("area_names.in","species_names.in","OP.dat","OP.exe","OP_trigger.dat","OP_config.dat","OP_MSFD.dat","just_one.in",
      "OP_consum.in","OP_F.in","OP_M1.in","OP_M.in","OP_N.in","OP_propmat.in","OP_prop_landed.in","OP_size.in","OP_wcatch.in","OP_wsea.in",
      "OP_growth_type1.in","OP_consum_ab.in","OP_other_N.in","OP_Exploitation.in","OP_reference_points.in","covariance_rec.in","OP_price.in",
      "OP_SSB_rec_residuals.in")

  if (SMS@no.areas>1)  OP.files<-c(OP.files,"OP_Stock_distribution.in")
  
  for (from.file in OP.files) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  sp.name<-SMS.control@species.names
 
  setwd(scenario.dir)
  
  if (adjust.ini.N) {
   ini.N<-matrix( scan(file="OP_N.in",comment.char="#",quiet = T),nrow=n.vpa,byrow=T)
   ini.N[1,]<-ini.N[1,]*2  # cod
   write.matrix(ini.N,file="OP_N.in")
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
  
  #run OP
  comm.sms<-paste( file.path(scenario.dir,"OP.exe"),"-maxfn 0 -nohess ",sep=" ")

  if (just.batch.file) {
    cat("cd ",scenario.dir,"\n",comm.sms,' >ud.dat\n',file=file.path(scenario.dir,"run.OP.bat"))
    #stop(paste("go to directory",scenario.dir,"and run script run_op.bat\n"))
    system(command=file.path(scenario.dir,"run.OP.bat"), wait = F)

  } else  shell(comm.sms, invisible = TRUE)
}   # end do.simulations

setwd(data.path)                                        


if (!just.batch.file) {
  if (my.area=='North Sea') spNames<-c('COD', 'WHG', 'HAD', 'POK', 'HER', 'SAN', 'NOR', 'SPR', 'PLE', 'SOL')
  if (my.area=='Baltic Sea') spNames<-c('COD', 'HER', 'SPR')
  
  n.run<-dim(targetFs)[[1]]
  
  
  if (n.run >1) {
    if (read.condense) {
      cat('reading condensed data from directory:  ', scenario.dir,'\n')
      setwd(scenario.dir)
      condensed<-read.table('OP_average_val.out',header=F)
      allnames<-c(c('value','yield', 'CWsum', 'Fcomb','Fbar', 'SSB', 'TSB', 'recruit','belowBlim','belowBpa', 'Species.n', 'run','iteration'),spNames)
      dimnames(condensed)[[2]]<-allnames
      save(condensed, file =file.path(scenario.dir, "condensed.RData"))
      setwd(data.path)
    } else load(file =file.path(scenario.dir, "condensed.RData"))
  
   if (read.condense) {     # f mult combinations
      setwd(scenario.dir)
      nr<-scan( "OP_mulTargetF.in", nmax = 1,quiet = T)
      Fcomb<-data.frame(matrix(scan( "OP_mulTargetF.in",skip=1),nrow=nr,ncol=length(spNames),byrow=T))
      dimnames(Fcomb)[[2]]<-spNames 
      Fcomb$run=first.run.no:(nr+first.run.no-1)
      save(Fcomb, file =file.path(scenario.dir, "Fcomb.RData"))
      setwd(data.path)
    } else load(file =file.path(scenario.dir, "Fcomb.RData"))
    
   if (read.indicators ) {
      setwd(scenario.dir)
      indi<-read.table( "OP_indicator_system_avg.out",header=T)
      
      load(file =file.path(scenario.dir, "Fcomb.RData"))
      #cat("dim Fcomb:\n")
      #print(dim(Fcomb))
      #print(head(Fcomb))
      #cat("dim ini\n")
      #print(dim(indi))
      #print(head(indi))
      indi<-merge(indi,Fcomb)
      #cat("dim indi after merge\n")
      #print(dim(indi))
      
      if  (my.area=='North Sea') {
        indi$unique.id<-paste(indi$run,indi$iter,round(indi$COD,3),round(indi$WHG,3),round(indi$HAD,3),round(indi$POK,3),round(indi$HER,3),round(indi$SAN,3),round(indi$NOR,3),round(indi$SPR,3),round(indi$PLE,3),round(indi$SOL,3),sep='')
      } else if  (my.area=='Baltic Sea') {
        indi$unique.id<-paste(indi$run,indi$iter,round(indi$COD,3),round(indi$HER,3),round(indi$SPR,3),sep='')
      }

      # get SSB and yield from the condensed data set
      if  (my.area=='North Sea') {
        b<-aggregate(cbind(value,yield,CWsum,SSB,TSB)~  run+iteration+COD+WHG+HAD+POK+HER+SAN+NOR+SPR+PLE+SOL,data=condensed,sum)
        b$unique.id<-paste(b$run,b$iteration,round(b$COD,3),round(b$WHG,3),round(b$HAD,3),round(b$POK,3),round(b$HER,3),round(b$SAN,3),round(b$NOR,3),round(b$SPR,3),round(b$PLE,3),round(b$SOL,3),sep='')
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
    } else load(file =file.path(scenario.dir, "indicators.RData"))
  
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
      source(file.path(prog.path,"HCR_OP_condense_function.r"))
      a<-transform.condensed(a=condensed,my.area=my.area)

      save(a, file =file.path(scenario.dir, "a.RData"));  
    } else load(file =file.path(scenario.dir, "a.RData"))

  }  # end combine.scenario.run
  
  

  
  if ((do.plot.condense | ReduceTable )& n.run >1) {
  
    if (my.area=='North Sea') {
        if (subset.out) {       # make a subset of data set used to make plots
           #COD WHG HAD POK HER SAN NOR SPR PLE SOL 
           bb<-subset.species
           a<-droplevels(subset(a, COD>=bb[1,1] &  COD<=bb[2,1] & 
                                   WHG>=bb[1,2] &  WHG<=bb[2,2] & 
                                   HAD>=bb[1,3] &  HAD<=bb[2,3] & 
                                   POK>=bb[1,4] &  POK<=bb[2,4] & 
                                   HER>=bb[1,5] &  HER<=bb[2,5] & 
                                   SAN>=bb[1,6] &  SAN<=bb[2,6] & 
                                   NOR>=bb[1,7] &  NOR<=bb[2,7] & 
                                   SPR>=bb[1,8] &  SPR<=bb[2,8] & 
                                   PLE>=bb[1,8] &  PLE<=bb[2,9] & 
                                   SOL>=bb[1,10] & SOL<=bb[2,10] )) 
         }
        out.file<-'all'
        out.types<-c('Yield','SSB','Recruit','Fbar','belowBlim', 'belowBpa','riskBlim')
   
        source(file.path(prog.path,"FMSY_matrix_functions_plots.r"))
        if (do.plot.condense) {
          plot.matrix(tit='Yield',type=out.types[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22) 
          plot.matrix(tit='SSB',  type=out.types[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
          plot.matrix(tit='Recruits', type=out.types[3],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
          plot.matrix(tit='Average F', type=out.types[4],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
          plot.matrix(tit='Probability (%) of SSB below Blim', type=out.types[5],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
          plot.matrix(tit='Proportion (%) of scenarios with prob(SSB<Blim) > 5%', type=out.types[7],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)

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
              source(file.path(prog.path,"FMSY_matrix_functions.r"))
              plot.matrix(tit='Yield',type=out.types[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22) 
              plot.matrix(tit='SSB',  type=out.types[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
              plot.matrix(tit='Recruits', type=out.types[3],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
              plot.matrix(tit='Average F', type=out.types[4],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
              plot.matrix(tit='Probability (%) of SSB below Blim', type=out.types[5],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
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
      
              source(file.path(prog.path,"FMSY_matrix_functions.r"))
              plot.matrix(tit='Yield',type=out.types[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22) 
              plot.matrix(tit='SSB',  type=out.types[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
              plot.matrix(tit='Recruits', type=out.types[3],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
              plot.matrix(tit='Average F', type=out.types[4],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
              plot.matrix(tit='Probability (%) of SSB below Blim', type=out.types[5],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=a,first.sc=22)
              cat("\n\nRelative MSY:\n")
              table.MSY(a,out.file=out.file,scenario.dir=scenario.dir)
             } else cat("\n\nAll combinations below Bpa, no plotting done\n\n") 
           } 
         }

           
           
       
    }  # end North Sea
     

    if (my.area=='Baltic Sea') {
       source(file.path(prog.path,"FMSY-matrix-Baltic.R"))
    } # end Baltic Sea
    
    if (ReduceTable) {
           source(file.path(prog.path,"FMSY_matrix_functions_tables.r"))
           cat('\nStart to reduce Table\n')
           
          load(file =file.path(scenario.dir, "a.RData"))  # to get a fresh, non reduced version
           aa<-a
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombMSYfinal(aa,out.file='MSY_redu.out',app=(ii!=0),scenario.dir=scenario.dir,keepPro=keepPro); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='MSY_redu_detail',scenario.dir=scenario.dir,app=(ii!=1))
           }
           cat("\n",dim(aa),"\n")
           table.MSY(aa,out.file="MSY_redu_final",scenario.dir=scenario.dir)
           rm(a2)
           #rm(aa)

           # risk to Blim
           # aa<-a          Do not start with the full datat set
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombRiskfinal(aa,out.file='MSY_and_Blim_redu_continue.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='MSY_and_Blim_redu_continue_details',scenario.dir=scenario.dir,app=(ii!=1))
           }
           table.MSY(aa,out.file='MSY_and_Blim_redu_continue_final',scenario.dir=scenario.dir)
           rm(a2)
           rm(aa)
           
           # risk to Blim
           aa<-a          # start with the full datat set
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombRiskfinal(aa,out.file='Blim_redu_all.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='Blim_redu_all_details',scenario.dir=scenario.dir,app=(ii!=1))
           }
           table.MSY(aa,out.file='Blim_redu_all_final',scenario.dir=scenario.dir)
           rm(a2)
           rm(aa)

           # risk to Blim , using the 
           aa<-a          # start with the full datat set
           goOn<<-T; 
           ii<-0
           while (goOn) {
             a2<-reduceCombRiskfinal(aa,out.file='Blim_redu_v2_all.out',app=(ii!=0),scenario.dir=scenario.dir,riskLevels=riskLevels,type=2); 
             if (goOn) aa<-a2
             ii<-ii+1 
             if (goOn & ReduceTableDetails) table.MSY(aa,out.file='Blim_redu_v2_all_details',scenario.dir=scenario.dir,app=(ii!=1))
           }
           table.MSY(aa,out.file='Blim_redu_v2_all_final',scenario.dir=scenario.dir)
           rm(a2)
           rm(aa)
        }
    
   }
  
   
  if (do.plot.indicators & n.run>1) {
      if (my.area=='North Sea') {
         source(file.path(prog.path,"plot_OP_community_indicators_average_NorthSea.R"))
          n.species<-8
          #a<-droplevels(subset(a,(COD<=0.4 & POK<=0.4) & SPR<0.6))
          out.file='all_'
          plot.matrix(tit='System indicators',spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=indi,first.sc=22)

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
               plot.matrix(tit='System indicators',spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir,a=indi,first.sc=22)
             }  else cat("\n\nAll combinations below Blim, no plotting done\n\n") 
           }
         }
      }    # end North Sea
      
      if (my.area=='Baltic Sea') source(file.path(prog.path,"plot_OP_community_indicators_average_Baltic.R"))
  }
 } # end just batch
 
  used.scenario.dir<<-scenario.dir
  scenario.dir<<-used.scenario.dir
  scenario.dir.simul<<-used.scenario.dir

}  # end function

###################################
# function for fixing POK, PLE and SOL F
POK.PL.SOL<-function(HCR=HCR1,FBlim.adjust=1,stochastic.recruitment=1,recruit.adjust.CV=0) {  
    # find F-values for POK, PLE and Sole
    OP.simulate(scenario="POK",HCR=HCR,FBlim.adjust=FBlim.adjust,targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.20,0.60,0.05),HER=seq(0.30,0.30,0.05),SAN=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.05),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV)
    load(file =file.path(used.scenario.dir, "condensed.RData"))
    POK<-subset(condensed,Species.n==20)
    POK2<<-POK
    OP.simulate(scenario="PLE",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),HER=seq(0.30,0.30,0.05),SAN=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.20,0.60,0.025),SOL=seq(0.50,0.50,0.05)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV)
    load(file =file.path(used.scenario.dir, "condensed.RData"))
    PLE<-subset(condensed,Species.n==25)
    
    OP.simulate(scenario="SOL",HCR=HCR,FBlim.adjust=FBlim.adjust, targetFs=expand.grid(COD=seq(0.45,0.45,0.05),WHG=seq(0.30,0.30,0.05),HAD=seq(0.35,0.35,0.05),POK=seq(0.30,0.30,0.05),HER=seq(0.30,0.30,0.05),SAN=seq(0.25,0.25,0.05),NOR=seq(0.25,0.25,0.05),SPR=seq(0.30,0.30,0.05),PLE=seq(0.50,0.50,0.025),SOL=seq(0.20,0.60,0.025)),ask.me=ask.me,my.area=my.area,my.last.year=my.last.year, years.in.average= years.in.average,stochastic.recruitment=stochastic.recruitment,no.iter.stoch=no.iter.stoch,recruit.adjust.CV=recruit.adjust.CV)
    load(file =file.path(used.scenario.dir, "condensed.RData"))
    SOL<-subset(condensed,Species.n==26)
    
    allSp<-rbind(POK,PLE,SOL)
   
    if (stochastic.recruitment==0  | stochastic.recruitment==2 ) {
      allSp[allSp$belowBlim>0,'belowBlim']<-100
      allSp[allSp$belowBpa>0,'belowBpa']<-100
    }
    png(filename =file.path(used.scenario.dir,paste('POK_PLE_SOL','.png',sep='')), width = 700, height = 1100,units = "px", pointsize = 18, bg="white")
    par(mfcol=c(6,3))
    par(mar=c(2,4,3,2)) # c(bottom, left, top, right)
    a<-subset(allSp,Species.n %in% c(20,25,26))
    
    boxs <- defmacro(a, varsp, spname,spno,
      expr={
          boxplot(yield/1000~varsp ,data=a,show.names = T,ylab='Yield (1000t)',subset=Species.n==spno,main=spname)
          boxplot(SSB/1000~varsp ,data=a,show.names = T,ylab='SSB (1000t)',subset=Species.n==spno)
          boxplot(Fbar~varsp ,data=a,show.names = T,ylab='Realized F',xlab='F',subset=Species.n==spno)     
          boxplot(belowBlim~varsp ,data=a,show.names = T,ylab='prob(SSB<Blim) %',xlab='F',subset=Species.n==spno)
          boxplot(belowBpa~varsp ,data=a,show.names = T,ylab='prob(SSB<Bpa) %',xlab='F',subset=Species.n==spno)
          boxplot(recruit/1E6~varsp ,data=a,show.names = T,ylab='recruits (10^9)',xlab='F',subset=Species.n==spno)
        })
     boxs(a, POK, 'Saithe',20)
     boxs(a, PLE,'Plaice',25)
     boxs(a, SOL,'Sole',26)


    cleanup()
}

  