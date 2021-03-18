rm(list = ls())
suppressPackageStartupMessages(library(shiny))
library(shinyhelper)
suppressMessages(library(DT))
library(readr)
suppressMessages(library(dplyr))
library(ggplot2)
suppressMessages(library(wordcloud))
#library(radarchart)
library(RColorBrewer)
library("cowplot")
library("shinyWidgets")

#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)


source("dragables.R")


test<-FALSE

## Global
data_dir <- "Data"
help_dir <-"helpfiles"
OS<- .Platform$OS.type  #operating system

# reset option files to default values
file.copy(file.path(data_dir,'op_config_master.dat'),file.path(data_dir,'op_config.dat'),overwrite = TRUE)
file.copy(file.path(data_dir,'op_exploitation_master.in'),file.path(data_dir,'op_exploitation.in'),overwrite = TRUE)


# control objects for predictions
data.path<-data_dir # used by the control objects
source("flsms.control.r") # to handle file SMS.dat with options for running SMS in hindcast and producing data for forecast

SMS<-read.FLSMS.control(file='sms.dat',dir=data_dir)
#get information from SMS run
n.species.tot <- SMS@no.species  # number of species including "other predators"
n.pred.other<-sum(SMS@species.info[,'predator']==2) #number of "other predators"
n.VPA<-n.species.tot-n.pred.other #number of species with analytical assessment
n.pred<-n.pred.other+sum(SMS@species.info[,'predator']==1) # number of predators
n.fleet <- n.VPA   # in this case it is just the number of species, one "fleet" per species
stq_year<-SMS@last.year.model
fy_year_hist<-SMS@first.year.model
n.seasons<-SMS@last.season
first.age<-SMS@first.age
max.age<-SMS@max.age.all
n.age<-max.age-first.age+1
spNames<-SMS@species.names

firstVPA<-n.species.tot-n.VPA+1   #first species with analytical assessment
VPA.spNames<-spNames[firstVPA:n.species.tot]
spOtherNames<-c('Other.food',spNames)
other.spNames<-spNames[1:n.pred.other]
  
# group predator names for eaten biomass
pred_format<-read.csv(file.path(data_dir,'pred_format.csv'),header=TRUE)
pp<-unique(subset(pred_format,select=c(new,new_no,group)))
pp<-pp[order(pp$new_no),]
predPreyFormat<-pp$new

recruitMode<-c('Determenistic','Stochastic')[1]

my.colors<-c('grey','red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta',
                         'limegreen','pink','darkorange3','aquamarine','beige','darkslategray','brown1','blueviolet','chocolate1' )


  
predPreyFormat<-c('Humans',levels(predPreyFormat))

# options for predictions, reset from master version
source("flop.control.r")

OP<-read.FLOP.control(file="op_master.dat",path=data_dir,n.VPA=n.VPA,n.other.pred=n.pred.other,n.pred=n.pred)
OP.trigger<-read.FLOPtrigger.control(file="op_trigger_master.dat",path=data_dir,n.VPA=n.VPA,n.other.pred=n.pred.other)

# Units (multiplier) in output
plotUnits<- c(Yield=0.001,          Fbar=1,   SSB=0.001,          TSB=0.001,           Recruits=0.001,        DeadM=0.001)
plotLabels<-c(Yield='(1000 tonnes)',Fbar=" ", SSB='(1000 tonnes)',TSB='(1000 tonnes)', Recruits='(millions)', DeadM='(1000 tonnes)')
roundUnits<- c(Yield=1,             Fbar=3,   SSB=1,              TSB=1,               Recruits=0,            DeadM=1)

# read status quo values (= values in the terminal hindcast SMS model year)  
status_quo<-read_csv(file=file.path(data_dir,'status_quo.csv'),col_types = cols()) %>%
  mutate(Rec=Rec*plotUnits['Recruits'],SSB=SSB*plotUnits['SSB'],TSB=TSB*plotUnits['TSB'],SOP=SOP*plotUnits['Yield'],
         Yield=Yield*plotUnits['Yield'],mean.F=mean.F*plotUnits['Fbar'],Eaten=Eaten*plotUnits['DeadM'])

# read hindcast SMS values  
histAnnoM<-read_csv(file=file.path(data_dir,'hist_anno_M.csv'),col_types = cols()) %>%mutate(Species=spNames[Species.n])
histCondensed<-read_csv(file=file.path(data_dir,'hist_condensed.csv'),col_types = cols())%>%mutate(Species=spNames[Species.n]) %>%
  mutate(Recruits=Recruits*plotUnits['Recruits'],SSB=SSB*plotUnits['SSB'],TSB=TSB*plotUnits['TSB'],Yield=Yield*plotUnits['Yield'],
         Fbar=Fbar*plotUnits['Fbar'],DeadM1=DeadM1*plotUnits['DeadM'],DeadM2=DeadM2*plotUnits['DeadM'])

histEaten <- read_csv(file=file.path(data_dir,'who_eats_whom_historical.csv'),col_types = cols()) %>%
              mutate(Predator=factor(Predator,levels=predPreyFormat),Prey=factor(Prey,levels=predPreyFormat),eatenW=eatenW*plotUnits['DeadM'])


refPoints<-matrix(scan(file.path(data_dir,"op_reference_points.in"),quiet = TRUE, comment.char = "#"),ncol=4,byrow=TRUE)
rownames(refPoints)<-VPA.spNames
colnames(refPoints)<-c('Flim','Fpa','Blim','Bpa')
refPoints[,3:4] <- refPoints[,3:4]* plotUnits['SSB']

explPat<-scan(file.path(data_dir,"op_exploitation.in"),quiet = TRUE, comment.char = "#")
#explPat<-scan(file.path(data_dir,"op_f.in"),quiet = TRUE, comment.char = "#")
explPat<-array(explPat,dim=c(n.age,n.VPA,n.seasons),dimnames=list(paste('age',as.character(first.age:max.age)),VPA.spNames,paste0('Q_',1:n.seasons)))
annExplPat<-apply(explPat,c(1,2),sum) # annual exploitation pattern
Fages<-SMS@avg.F.ages
Fages
#avExplPat<-sapply(rownames(Fages),function(sp) mean(annExplPat[as.character(Fages[sp,1]:Fages[sp,2]),sp]))

last.age<-SMS@species.info[VPA.spNames,'last-age']
first.age




# SMS output values in the last year
base_SSB<-stqSSB<-status_quo$SSB
base_F<-stqF<-status_quo$mean.F
#stqF<-status_quo$sum.q.F
base_Yield<-stqYield<-status_quo$Yield
base_Rec<-stqRec<-status_quo$Rec

# write status quo F
#cat("1\n",base_F,"\n",file=file.path(data_dir,"op_multargetf.in")) # write F values

# read various setting for options files
hcr_ini<-read.csv(file.path(data_dir,'HCR_ini.csv'),header=TRUE)

### change option values (could have been done in the master files!)
OP@rec.noise['lower',]<-hcr_ini$noise.low
OP@rec.noise['upper',]<-hcr_ini$noise.high
OP@recruit.adjust.CV[1,]<-hcr_ini$rec.adjust.CV.single
OP@recruit.adjust[1,]<-hcr_ini$rec.adjust.single
#
OP.trigger@Ftarget['init',]<-base_F
OP.trigger@trigger['T1',]<-hcr_ini$T1
OP.trigger@trigger['T2',]<-hcr_ini$T2
OP.trigger@HCR[1,]<-1
  

# write option files to be used
write.FLOP.control(OP,file=file.path(data_dir,"op.dat"),nice=TRUE)
write.FLOPtrigger.control(OP.trigger,file="op_trigger.dat",path=data_dir, nice=TRUE)


fleetNames<-paste0('fl_',VPA.spNames) # just this special case where we have no fleets

#recruitment parameters from op_config.dat
rec<-readLines(file.path(data_dir,"op_config.dat"))
found<-grep("#model alfa  beta std info1 info2",rec)
rec<-scan(file.path(data_dir,"op_config.dat"),skip=found,comment.char = "#",nlines=n.VPA,quiet = TRUE)
rec<-matrix(rec,nrow=n.VPA,byrow=TRUE)
colnames(rec)<-c('model','a','b','s','o1','o2')
rownames(rec)<-VPA.spNames

# maximum recruits
max_rec<-rep(0,n.VPA);names(max_rec)<-VPA.spNames
i<-rec[,'model']==100;max_rec[i]<-exp(rec[i,'a'])*rec[i,'b']  # Hockey stick
i<-rec[,'model']==1;  max_rec[i]<-rec[i,'a']/(rec[i,'b']*exp(1))  # Ricker
i<-rec[,'model']==2;  max_rec[i]<-rec[i,'a']/rec[i,'b']  # B & H
i<-rec[,'model']==3;  max_rec[i]<-exp(rec[i,'a'])  # GM
i<-OP@recruit.adjust.CV==2; max_rec[i]<-max_rec[i]*exp((rec[i,'s']^2)/2)
max_rec<-max_rec*plotUnits['Recruits']

# values for baselines option lists
bsF<-list(Names=list('No change',paste0('F(',stq_year,')'),'Most recent results'),Values=list(0,1,2))
bsSSB<-list(Names=list('No change',paste0('SSB(',stq_year,')'),'Most recent results'),Values=list(0,1,2))
bsYield<-list(Names=list('No change',paste0('Yield(',stq_year,')'),'Most recent results'),Values=list(0,1,2))
bsRec<-list(Names=list('No change',paste0('Recruitment(',stq_year,')'),'Most recent results','Maximum recrutiment'),Values=list(0,1,2,3))


F_mult<-1.0  

Fvalues<-stqF*F_mult

oldwd<-getwd()

# command file for executing the OP program to make a prediction
cmd <- paste0('cd "', file.path(oldwd,data_dir), '" &&  "./op" -maxfn 0 -nohess > ud.dat')
## cat("cd ",file.path(oldwd,data_dir),"\n '.\op' -maxfn 0 -nohess > ud.dat\n",  file=file.path(oldwd,data_dir,"run_op.bat"))

do_baseLine<-function(){
  baseLine=cbind(SSB=base_SSB,Fbar=base_F,Yield=base_Yield,Recruits=base_Rec)
  rownames(baseLine)<-VPA.spNames
  return(baseLine)
}


updateExplPatttern<-function(explPat) {
  # change factor in annual exploitation pattern
  
  # test annExplPat[,'Cod']; annExplPat[6:9,'Cod']<-annExplPat[6:9,'Cod']*0.2; annExplPat[,'Cod']
  
  change<-annExplPat/apply(explPat,c(1,2),sum) 
  change[is.na(change)]<-1.0
  for (q in (1:n.seasons)) explPat[,,q]<-explPat[,,q]*change
  
  # rescale to 1
  avExplPat<-sapply(rownames(Fages),function(sp) mean(annExplPat[paste('age',as.character(Fages[sp,1]:Fages[sp,2])),sp]))
  for (sp in rownames(Fages)) explPat[,sp,]<-explPat[,sp,] /avExplPat[sp]
  
  # recalculate annual exploitation pattern
  annExplPat<<-apply(explPat,c(1,2),sum) 

  return(explPat)
}



op.n<-0 #counter for calls to op.exe, used for tests only

# call to the OP program
do_OP<-function(readResSimple=TRUE,readResDetails=FALSE,readResStom=FALSE,writeOption=FALSE,writeExplPat=FALSE,source='') {
  op.n<<-op.n+1
  cat(op.n, "call source:", source, " readResSimple:",readResSimple," readResDetails:",readResDetails," readResStom:",readResStom," writeOption:",writeOption,"  writeExplPat:",writeExplPat,'\n')
  
   # write the F values
   Fvalues<-OP.trigger@Ftarget['init',]
   cat("1\n",Fvalues,"\n",file=file.path(data_dir,"op_multargetf.in")) # write F values

   if (writeOption) {  #write option files
     write.FLOP.control(OP,file="op.dat",path=data_dir,nice=TRUE,writeSpNames=FALSE)   
     write.FLOPtrigger.control(OP.trigger,file="op_trigger.dat",path=data_dir,nice=FALSE,writeSpNames=FALSE)
     doWriteOptions<<-FALSE
   } 
   
   if (writeExplPat){
     explPat<<-updateExplPatttern(explPat)

     out<-file.path(data_dir,'op_exploitation.in')
     cat("# exploitation pattern\n",file=out)
     for (q in (1:n.seasons)) {
       cat(paste("# quarter",q,'\n'),file=out,append=TRUE)
       write.table(t(explPat[,,q]),col.names=FALSE, row.names=FALSE,file=out,append=TRUE)
     }
     doWriteExplPattern<<-FALSE
   }  
   
   # run the script
   ##system2(command=file.path(oldwd,data_dir,"run_OP.bat"), wait = TRUE,stdout = file.path(oldwd,data_dir,"Run_OP_out.dat"))
   if(OS == "windows") shell(cmd)
   if(OS == "unix") system(cmd)
     
   
   doRunModel<<-FALSE
  
  #read the results
  if (readResSimple) {
    a<-read.table(file.path(data_dir,'op_condensed.out'),header=T)
    a<-data.frame(Species.n=a$Species.n,Yield=a$yield*plotUnits['Yield'],Fbar=a$Fbar*plotUnits['Fbar'], SSB=a$SSB*plotUnits['SSB'], TSB=a$TSB*plotUnits['TSB'],Recruits=a$recruit*plotUnits['Recruits'])
  
    
    #format for ggradar
    b<-t(a)
    colnames(b)<-VPA.spNames
    b<-mutate(as_tibble(b),variable=rownames(b)) %>%select(c("variable",all_of(VPA.spNames))) %>%subset(variable!="Species.n") 
    a$Species<-VPA.spNames
    a<-select(as_tibble(a),Species,Yield,Fbar,SSB,TSB,Recruits)
  } 
  
  if (readResDetails)  {
    d<-read.table(file.path(data_dir,'op_condensed_long.out'),header=T)
    d1<-data.frame(Year=d$Year, Species=spNames[d$Species.n], Species.n=d$Species.n,
                   Yield=d$yield*plotUnits['Yield'],Fbar=d$Fbar*plotUnits['Fbar'], 
                   SSB=d$SSB*plotUnits['SSB'], TSB=d$TSB*plotUnits['SSB'],Recruits=d$recruit*plotUnits['Recruits'], 
                   DeadM1=(d$DeadM-d$DeadM2)*plotUnits['DeadM'], DeadM2=d$DeadM2*plotUnits['DeadM'])
    if (recruitMode=='Stochastic'){
      a<-subset(d1,Year>=max(termYear+1,(termYear-10)),select= -Species)
      a<-aggregate(cbind(Yield,Fbar,SSB,TSB,Recruits,DeadM1,DeadM2)~Species.n,data=d1,FUN=mean)
      #format for ggradar
      b<-t(a)
      colnames(b)<-VPA.spNames
      b<-mutate(as_tibble(b),variable=rownames(b)) %>%select(c("variable",all_of(VPA.spNames))) %>%subset(variable!="Species.n") 
      a$Species<-VPA.spNames
      a<-select(as_tibble(a),Species,Yield,Fbar,SSB,TSB,Recruits)
    }
    d<-read.table(file.path(data_dir,'op_anno_M.out'),header=T)
    d2<-data.frame(Year=d$Year, Species=spNames[d$Species.n], Species.n=d$Species.n,Age=d$Age, M2=d$M2)
  } else {d1<-'no data'; d2<-'No data'}
  
  if (readResStom){
    s<-read.table(file.path(data_dir,'op_summary.out'),header=T)
    s$N.bar<-s$N*(1-exp(-s$Z))/s$Z
    s$Species<-spNames[s$Species.n]
    s<-subset(s, select=c(Species,Year,Quarter,Species.n,Age,M2,N.bar,west))
    s<-data.frame(s,deadM2=s$M2*s$N.bar*s$west,deadM=s$M*s$N.bar*s$west)
    s<-subset(s,select=c(Species, Year, Quarter, Species.n, Age, M2,deadM2))

    M2<-read.table(file.path(data_dir,'op_part_m2.out'),header=T)
    M2$Area<-NULL
    M2<-data.frame(Predator=spNames[M2$Predator.no],Prey=spOtherNames[M2$Prey.no+1],M2) 
    
    M2<-merge(x=s,y=M2, by.x = c("Year","Quarter","Species","Age"), by.y = c("Year","Quarter","Prey","Prey.age"))
    M2$eatenW<- M2$deadM2*M2$Part.M2/M2$M2
    
    M2$Prey<-M2$Species
    M2$Prey.age<-M2$Age
    M2$tot.M2.prey<-M2$M2
    
    bbb<-droplevels(aggregate(list(eatenW=M2$eatenW),list(Year=M2$Year, Predator=M2$Predator,Prey=M2$Prey,Prey.no=M2$Prey.no),sum))
    bbb$eatenW<-bbb$eatenW*plotUnits['DeadM']

    s<-merge(x=bbb,y=pred_format,by.x='Prey',by.y='old',all.x=TRUE)
    s$Prey<-s$new; s$new<-NULL
    s$Prey.no<-s$new_no; s$new_no<-NULL
    
    s<-merge(x=s,y=pred_format,by.x='Predator',by.y='old',all.x=TRUE)
    
    s<-aggregate(s$eatenW,list(s$new,s$Year,s$new_no,s$Prey,s$Prey.no),sum)
    names(s)<-c("Predator","Year","Predator.no","Prey","Prey.no","eatenW")
    
    human<-data.frame(Predator='Humans',Year=d1$Year,Predator.no=0,Prey=d1$Species,Prey.no=d1$Species.n,eatenW=d1$Yield)
    s<-bind_rows(s,human)
 
    # make unique format/factors  for predator and preys
    prey<-unique(data.frame(no=s$Prey.no,Species=s$Prey))
    pred<-unique(data.frame(no=s$Predator.no,Species=s$Predator))


    prey<-prey[order(prey$no,decreasing = FALSE),]
    prey<-prey$Species

    pred<-pred[order(pred$no,decreasing = FALSE),]
    pred<-pred$Species
    
     
    s<- mutate(as_tibble(s),Predator=factor(Predator,levels=predPreyFormat),Prey=factor(Prey,levels=predPreyFormat))
    
    predPrey<-lapply(pred,function(x) {a<-filter(s,Predator==x) %>% distinct(Prey);as.character(unlist(a))})
    names(predPrey)<-pred
    
    
  } else  { s<-'No data';pred<-'No data'; prey<-'No data'; predPrey<-'No data'}
  return(list(options=list(readResSimple=readResSimple,readResDetails=readResDetails,readResStom=readResStom,source=source),
                a=a,b=b,detail_sum=d1,detail_M2=d2,detail_eaten=s,pred=pred,prey=prey,predPrey=predPrey))
}

source('make_plots.R',local=TRUE)


get_terminal_year<-function(OP){
  return(OP@last.year)
}

termYear<-get_terminal_year(OP)

get_other_predators<-function(){
  First.year<-rep(1.0,length(other.spNames)) ;names(First.year)<-other.spNames
  Last.year<-Total.change<-First.year
  
  for (sp in other.spNames)  {
    if (OP@other.predator['first',sp]== -1)   First.year[sp]<-stq_year+1 else First.year[sp] <- OP@other.predator['first',sp]
    if (OP@other.predator['second',sp]== -1)  Last.year[sp]<-OP@other.predator['second',sp]<- termYear else  Last.year[sp] <- OP@other.predator['second',sp]
    if ((OP@other.predator['first',sp]== -1) || (OP@other.predator['second',sp]== -1)) Total.change[sp]<- OP@other.predator['factor',sp]** (OP@other.predator['second',sp]-OP@other.predator['first',sp]+1) else Total.change[sp]<-1
  }
  
  (data.frame(Predator=other.spNames,
                    change=OP@other.predator['factor',], 
                     First.year=as.integer(First.year), 
                    Last.year=as.integer(Last.year),
                    Total.change=Total.change , stringsAsFactors = FALSE))
}
other_predators<-get_other_predators()

put_other_predators<-function(a,OP){
  OP@other.predator['factor',]<-a$change
  for (sp in other.spNames){
    if (a[sp,'change']==1) OP@other.predator['first',sp]<- -1  else OP@other.predator['first',sp] <- a[sp,"First.year"]
    if (a[sp,'change']==1) OP@other.predator['second',sp]<- -1 else OP@other.predator['second',sp]<- a[sp,"Last.year"] 
  }
  return(OP)
}

# test put_other_predators(other_predators,OP)
  
hcrlab = c("Fixed F", "F from SSB", "F from TSB")
hcr <- data.frame(val = hcrlab)
hcrval<-c(1,2,22);names(hcrval)<-hcrlab 

get_op_Fmodel<-function(){
  HCR<-OP.trigger@HCR
  trigger<-OP.trigger@trigger*plotUnits['SSB']
  Ftarget<-OP.trigger@Ftarget['init',]
  return(data.frame(Species=VPA.spNames,target.F=Ftarget, HCR=names(hcrval[match(HCR[1,],hcrval)]),T1=trigger[1,],T2=trigger[2,],stringsAsFactors = FALSE))
}
Foption_tab<-get_op_Fmodel()


put_op_Fmodel<-function(a,OP.trigger) {
  OP.trigger@HCR[1,] <- hcrval[a$HCR]
  OP.trigger@Ftarget['init',]<-a$target.F
  OP.trigger@trigger[1,]<-a$T1/plotUnits['SSB']
  OP.trigger@trigger[2,]<-a$T2/plotUnits['SSB']
  return(OP.trigger)
}

doRunModel<-TRUE  # flag for re-running the prediction model
doWriteOptions<-TRUE  # flag for writing option files for the prediction model
doWriteExplPattern<- FALSE # flag for writing exploitation pattern file (op_exploitation.in)

# icons for HCR options
hcr$img = c(
  sprintf("<img src='fixed_F.png' width=100px><div class='jhr'>%s</div></img>", hcr$val[1]),
  sprintf("<img src='AR_F_SSB.png' width=100px><div class='jhr'>%s</div></img>", hcr$val[2]),
  sprintf("<img src='AR_F_TSB.png' width=100px><div class='jhr'>%s</div></img>", hcr$val[3])
)
oldFvals<-rep(1.0,n.fleet)


# Data for table with the most important data
makeResTable<-function(x){
  a<-data.frame(Species=VPA.spNames,
                F_base=round(x$baseLine[,'Fbar'],3),
                F_new=round(x$out$a[,'Fbar'],roundUnits['Fbar']),
                F_change=(x$out$a[,'Fbar']-x$baseLine[,'Fbar'])/x$baseLine[,'Fbar'],
                Yield_base=round(x$baseLine[,'Yield'],3),
                Yield_new=round(x$out$a[,'Yield'],roundUnits['Yield']),
                Y_change=(x$out$a[,'Yield']-x$baseLine[,'Yield'])/x$baseLine[,'Yield'],
                SSB_base=round(x$baseLine[,'SSB'],roundUnits['SSB']),
                SSB_new=round(x$out$a[,'SSB'],roundUnits['SSB']),
                SSB_change=(x$out$a[,'SSB']-x$baseLine[,'SSB'])/x$baseLine[,'SSB'],
                rec_base=round(x$baseLine[,'Recruits'],roundUnits['Recruits']),
                rec_new=round(x$out$a[,'Recruits'],roundUnits['Recruits']))

  colnames(a)<-c("Species",'F base',                                  paste0('F(',termYear,')'),                         'F change',
                           paste0('Yield base',plotLabels['Yield']),  paste0('Yield(',termYear,')', plotLabels['Yield']),'Yield change',
                           paste0('SSB base', plotLabels['SSB']),     paste0('SSB(',termYear,') ',  plotLabels['SSB']),  'SSB change',
                           paste0('Rec. base',plotLabels['Recruits']),paste0('Rec(',termYear,') ',  plotLabels['Recruits']))
  return(a)
}

if (test) { # simple predictions
  res<-  list(out=do_OP(),Fmulti=rep(F_mult,n.fleet),baseLine=do_baseLine(),source='test')
  res
  plot_radar_all(res)
  plot_one(res,type='Fbar',plot.legend = TRUE)
  plot_one(res,type='Yield')
  plot_one(res,type='Recruits')
  plot_one(res,type='SSB')
  
  makeResTable(res)
}



if (test) { # detailed predictions
  res<-  list(out=do_OP(readResDetails=TRUE,readResStom=TRUE),Fmulti=rep(F_mult,n.fleet),baseLine=do_baseLine(),source='test')
  res
 
  plot_who_eats(res$out$detail_eaten)
  plot_summary(res,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),years=c(0,5000),species='Cod',splitLine=FALSE,incl.reference.points=FALSE) 
  plot_summary(res,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),years=c(0,5000),species='Mackerel',splitLine=FALSE,incl.reference.points=FALSE) 
  
  makeResTable(res)
}


  
sliders <- div()
for (i in (1:n.fleet)) {
  sliders <- tagAppendChild(sliders, sliderInput(inputId = paste0("F.",fleetNames[i]),
                                                 label = paste(fleetNames[i]),
                                                 min = 0.25, max = 4.0, value = 1.0, step = 0.05))
}


ui <- navbarPage(title = "SMS",
        tabPanel(title='ReadMe',
                 radioButtons(inputId = 'language',label='Select language',choices=c('Danish','English')),
                 conditionalPanel("input.language=='English'",includeMarkdown(file.path(help_dir, "SMS-intro.md"))),
                 conditionalPanel("input.language=='Danish'",includeMarkdown(file.path(help_dir, "SMS-intro_DK.md")))
                 
        ),
        tabPanel(title='Simple predictions',
               tabsetPanel(id='simple_predict',        
                   tabPanel(title = "Predictions",
                    column(3,
                          checkboxInput("effcontrolAll", "Same factor for all fleets?", value = TRUE) %>%
                            helper(colour = "green", type = "markdown",content = "SameFactor"),
                          conditionalPanel("input.effcontrolAll==1", sliderInput("F.all", "F factor",
                                                       min = 0.5, max = 2.0, value = 1, step = 0.05)),
                          conditionalPanel("input.effcontrolAll==0",sliders),
                        
                          br(),
                          downloadButton(outputId = "radarPlots1", label = "Download the plot")
                    ),     
                    column(4,
                         plotOutput(outputId = "F_plot1") %>%
                           helper(colour = "green", type = "markdown",content = "radar",size = "l"),
                         plotOutput(outputId = "Yield_plot1")
                    ),
                    column(4,
                         plotOutput(outputId = "rec_plot1"),
                         plotOutput(outputId = "SSB_plot1")
                    )
                   ),
                   tabPanel(title='Change Baseline',
                    column(3,
                      br(),
                      radioButtons(inputId = 'bas_F_s',label='Baseline for F',choiceNames=bsF$Names,choiceValues = bsF$Values) %>%
                        helper(colour = "green", type = "markdown",content = "BaseLineF"),
                      radioButtons(inputId = 'bas_Rec_s',label='Baseline for recruitment',choiceNames=bsRec$Names,choiceValues = bsRec$Values),
                      radioButtons(inputId = 'bas_Yield_s',label='Baseline for yield',choiceNames=bsYield$Names,choiceValues = bsYield$Values),
                      radioButtons(inputId = 'bas_SSB_s',label='Baseline for SSB',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values),
                      br(),
                      downloadButton(outputId = "radarPlots2", label = "Download the plot")),
                    column(4,
                           plotOutput(outputId = "F_plot3"),
                           plotOutput(outputId = "Yield_plot3")
                    ),
                    column(4,
                           plotOutput(outputId = "rec_plot3"),
                           plotOutput(outputId = "SSB_plot3")
                    )
                    
                   ),
                   tabPanel(title='Table output',br(),DTOutput('tableOut1'))
              )
        ),
        tabPanel(title='Detailed predictions',
              tabsetPanel(id='detailed_predict',        
                   tabPanel(title='Options', 
                    column(4, 
                        # for picture in pickerInput   
                         tags$head(tags$style("
                           .jhr{
                           display: inline;
                           vertical-align: middle;
                           padding-left: 10px;
                           }")),
                          br(),
                        wellPanel(radioButtons(inputId='Option', label='Select option', choices=list('Final year','F model','Other predators','Exploitation pattern',"Recruitment")))%>%
                           helper(colour = "green", type = "markdown",content = "Option"),
                        conditionalPanel("input.Option=='Recruitment'",
                             wellPanel(
                                radioButtons(inputId = 'recDetSto',label='Recruitment variability ',choices=list('Determenistic','Stochastic')),
                             ) %>% helper(colour = "green", type = "markdown",content = "Recruitment"),
                             conditionalPanel("input.recDetSto=='Stochastic'",textOutput("stoch_explain"))
                        ),
                        
                        
                        conditionalPanel("input.Option=='F model'",
                          wellPanel(
                           selectInput(inputId="HCR.sp", label="Species",choices=VPA.spNames),
                           pickerInput(inputId = "HCR",label = "Harvest Control Rule",choices = hcr$val,choicesOpt = list(content = hcr$img)),
                           numericInput(inputId="target.F",label="Target F",value=Foption_tab[1,'target.F'],min=0,max=2,step=0.01),
                           splitLayout(
                            conditionalPanel("input.HCR!=' 1: Fixed F'",numericInput(inputId="T1",label=paste("T1",plotLabels['SSB']),value=Foption_tab[1,'T1'],min=0,step=1)),
                            conditionalPanel("input.HCR!=' 1: Fixed F'",numericInput(inputId="T2",label=paste("T2",plotLabels['SSB']),value=Foption_tab[1,'T2'],min=0,step=1))
                           )) %>% helper(colour = "green", type = "markdown",content = "Fmodel")
                          ),
                         conditionalPanel("input.Option=='Other predators'",
                                          selectInput(inputId="OtherSp", label="Other predator",choices=other.spNames),
                                          sliderInput(inputId="OtherFirst",label="First year for change",min = stq_year+1, max = termYear, value = stq_year+1, step =1),
                                          sliderInput(inputId="OtherSecond",label="last year for change",min = stq_year+1, max = termYear, value = termYear, step =1),
                                          numericInput(inputId="OtherFactor",label="Change factor per year, (e.g. 1.1 means a 10 % increase per year)",value=1 ,min=-2,step=0,1)
                         ),
                                          
                        conditionalPanel("input.Option=='Exploitation pattern'",
                                         selectInput(inputId="exSpecies", label="Species",choices=VPA.spNames)),
                                         
                                         
                         conditionalPanel("input.Option=='Final year'",sliderInput(inputId="finalYear",label="Final year in prediction",min = stq_year+1, max = stq_year+100, value = termYear, step =1)%>%
                          helper(colour = "green", type = "markdown",content = "finalYear")),
                         conditionalPanel("input.Option=='F model'",actionButton(inputId="updateOptionTable", "Update option table")),
                         conditionalPanel("input.Option=='Other predators'",actionButton(inputId="updateOptionTableOther", "Update option table"))
                           ),
                    conditionalPanel("input.Option=='F model'",column(7,br(),tableOutput(outputId="HCRtable1"))),
                    conditionalPanel("input.Option=='Other predators'",column(7,br(),tableOutput(outputId="Othertable"))),
                    conditionalPanel("input.Option=='Exploitation pattern'",column(7,
                                       br(),h3('Drag  the individual bar to change relativ F at age'),br(),DragableChartOutput("testdrag", width = "440px")))
                    
                  ) ,
                  tabPanel(title='Results',
                           
                           column(4,   
                             br(),br(),
 
                           actionButton(inputId="doRunDetailed",label="Push to update prediction",icon("refresh")),
                           br(),br(),br(),
                           downloadButton(outputId = "radarPlots3", label = "Download the plot")),
                
                           column(4,
                                  plotOutput(outputId = "F_plot2"),
                                  plotOutput(outputId = "Yield_plot2")
                           ),
                           column(4,
                                  plotOutput(outputId = "rec_plot2"),
                                  plotOutput(outputId = "SSB_plot2")
                           )
                  ),
                  tabPanel(title='Change Baseline',
                      column(4, 
                          br(), 
                          radioButtons(inputId = 'bas_F_d',label='Baseline for F',choiceNames=bsF$Names,choiceValues = bsF$Values),
                          radioButtons(inputId = 'bas_Rec_d',label='Baseline for recruitment',choiceNames=bsRec$Names,choiceValues = bsRec$Values),
                          radioButtons(inputId = 'bas_Yield_d',label='Baseline for yield',choiceNames=bsYield$Names,choiceValues = bsYield$Values),
                          radioButtons(inputId = 'bas_SSB_d',label='Baseline for SSB',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values),
                          br(),
                          downloadButton(outputId = "radarPlots4", label = "Download the plot")
                      ),
                      column(4,
                             plotOutput(outputId = "F_plot4"),
                             plotOutput(outputId = "Yield_plot4")
                      ),
                      column(4,
                             plotOutput(outputId = "rec_plot4"),
                             plotOutput(outputId = "SSB_plot4")
                      )
                      
                  ),
                 tabPanel(title='Table output',br(),
                          radioButtons(inputId = 'tabOpt',label='Select table',choices=c('Results','Options')),
                             conditionalPanel("input.tabOpt=='Results'",column(8,DTOutput('tableOut2'))),
                             conditionalPanel("input.tabOpt=='Options'",column(8,tableOutput(outputId="HCRtable2"))),
                 ),
                 
                 tabPanel(title="Results by year",
                      column(2,br(),
                         selectInput(inputId="sumSpecies", label="Select Species:",choices=VPA.spNames),
                         sliderInput(inputId="firstY",label="First year output",value=stq_year+1,min=fy_year_hist,max=termYear,step=1),
                         sliderInput(inputId="lastY",label="Last year output",value=termYear,min=fy_year_hist+5,max=termYear,step=1),
                         radioButtons(inputId = 'inclRef',label='Include reference points',choices=c('yes','no')),
                         br(), downloadButton(outputId = "downSumPlots", label = "Download the plot")
                       #  wellPanel( 
                      #     sliderInput(inputId = 'pixx',label='Width plot', value=1200,min=100,max=2000,step=100),
                      #     sliderInput(inputId = 'pixy',label='Height plot', value=750,min=100,max=2000,step=100)
                      #   )
                        ),
                      column(10,  plotOutput(outputId = "summary_plot"))
                  ),
                 tabPanel(title = "Who eats whom", 
                      column(3,
                          br(),
                          wellPanel(
                            selectInput(inputId="whoPred", "Select a predator:",'all predators'),
                            selectInput(inputId="whoPrey", "Select a prey:",'all preys')
                          ),
                          radioButtons(inputId="whoHuman",label='Include humans as "predator"',choices = c('Excl. catch','Incl. catch')),
                         # radioButtons(inputId="whoOtherFood",label='Include "other foods"',choices = c('Excl. other','Incl. other')),
                          radioButtons(inputId="whoPredPrey",label='select value for stacking',choices = c('by prey','by predator')),
                          wellPanel(
                            sliderInput(inputId="firstYwho",label="First year output",value=stq_year+1,min=fy_year_hist,max=termYear,step=1),
                            sliderInput(inputId="lastYwho",label="Last year output",value=termYear,min=fy_year_hist+5,max=termYear,step=1)
                          ),
                       ),
                      column(9,br(),plotOutput(outputId = "whoEats_plot")),
                      downloadButton(outputId = "downWhoEats", label = "Download the plot")
                )
            ))
)

 server <- function(input, output, session) {
 
   res <- reactiveValues(rv = list(out=do_OP(readResSimple=TRUE,writeOption=doWriteOptions,source='init'),Fmulti=rep(F_mult,n.fleet),baseLine=do_baseLine()))   

   # uses 'helpfiles' directory by default
   # in this example, we do not use the withMathJax parameter to render formulae
   observe_helpers(withMathJax = FALSE)
   

   output$F_plot1     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot1 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot1   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot1   <- renderPlot({ plot_one(res$rv,type='Recruits') })

   output$F_plot2     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot2 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot2   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot2   <- renderPlot({ plot_one(res$rv,type='Recruits') })
   
   output$F_plot3     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot3 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot3   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot3   <- renderPlot({ plot_one(res$rv,type='Recruits') })

   output$F_plot4     <- renderPlot({ plot_one(res$rv,type='Fbar')     })
   output$Yield_plot4 <- renderPlot({ plot_one(res$rv,type='Yield')    })
   output$SSB_plot4   <- renderPlot({ plot_one(res$rv,type='SSB')      })
   output$rec_plot4   <- renderPlot({ plot_one(res$rv,type='Recruits') })
   
   output$stoch_explain <- renderText({paste('Constant Fishing mortalities will not work for stochastic recruitment. You have to defined Harvest Control Rules in the "F-model" option above,',
                                             'starting with the default values')})
   
   #output$summary_plot <-renderPlot({   if (res$rv$out$options$readResDetails) plot_summary(res$rv,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),
  #                                  years=c(input$firstY,input$lastY),species=input$sumSpecies,splitLine=FALSE,incl.reference.points= (input$inclRef=='yes'))},
  #                                  width = 1350, height=750,units = "px", pointsize = 25, bg = "white")
   output$summary_plot <-renderPlot({   if (res$rv$out$options$readResDetails) {sumPlot<<- plot_summary_new(res=res$rv,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),
                                                                                            years=c(input$firstY,input$lastY),species=input$sumSpecies,splitLine=FALSE,
                                                                                            incl.reference.points= (input$inclRef=='yes'));sumPlot}},
                                                             width = 1350, height=700,units = "px", pointsize = 25, bg = "white")
   
   output$whoEats_plot <- renderPlot({whoPlot<<-plot_who_eats(res$rv$out$detail_eaten,pred=input$whoPred,prey=input$whoPrey,predPrey=input$"whoPredPrey",
                                                     years=c(input$firstYwho,input$lastYwho),exclHumans=(input$whoHuman=='Excl. catch'));whoPlot})
   
   output$downSumPlots <- downloadHandler(
     filename =  function() {paste0("Summary_",input$sumSpecies,'.png')},
     content = function(file) {
         ggsave(file,plot=sumPlot,width = 25,height = 15,units='cm')
     }
   )
   
   output$downWhoEats <- downloadHandler(
     filename =  function() {paste0("Who_",input$whoPred,'_',input$whoPrey,'.png')},
     content = function(file) {
       ggsave(file,plot=whoPlot,width = 26,height = 15,units='cm')
     }
    )
   
   ######### there must be a smarter way to do the same thing 
   output$radarPlots1 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv)) 
       dev.off()
     }
   )
   
   output$radarPlots2 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv)) 
       dev.off()
     }
   )
   
   output$radarPlots3 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv)) 
       dev.off()
     }
   )
   
   output$radarPlots4 <- downloadHandler(
     filename = "radar_myPlot.png",
     content = function(file) {
       png(filename=file,width = 700,height = 700,units='px')
       print(plot_radar_all(res$rv)) 
       dev.off()
     }
   )
   ###########
   
   output$tableOut1 <- renderDT(
     DT::datatable(makeResTable(res$rv),rownames=FALSE, filter ="none",options=list(pageLength = n.VPA))  %>% 
       formatPercentage(columns=c(4,7,10),digits=1))  
     
   output$tableOut2 <- renderDT(
       DT::datatable(makeResTable(res$rv),rownames=FALSE, filter ="none",options=list(pageLength = n.VPA))  %>% 
         formatPercentage(columns=c(4,7,10),digits=1)) 
 
   output$HCRtableIn1 <- renderDT(
     DT::datatable(df_opt(),rownames=FALSE,filter ="none",editable = list(target = "row", disable = list(columns = c(1))),options=list(pageLength = n.VPA)) 
   ) 
   
   output$HCRtableIn2<- renderDT(
     DT::datatable(df_opt(),rownames=FALSE,filter ="none",editable = list(target = "row", disable = list(columns = c(1))),options=list(pageLength = n.VPA)) 
   ) 
   
   output$statusRun<-renderText(paste("doRunModel",doRunModel ,ifelse(doRunModel,'Prediction has not been updated','Up to data prediction')))

   
   output$testdrag <- renderDragableChart({
    as.vector( annExplPat[paste('age',as.character(first.age:last.age[input$exSpecies])),input$exSpecies])
    }, labels = rownames(annExplPat)
   )
   
  
   
   
   #result from testdrag ??
   observeEvent(input$rv, {
     annExplPat[paste('age',as.character(first.age:last.age[input$exSpecies])),input$exSpecies]<<-input$rv
     doWriteExplPattern<<-TRUE
     updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("refresh"))
   })
   
  #####
   
 
   
  df <- eventReactive(input$updateOptionTable || input$Option=='F model', {
   if (input$Option=='F model') {
      b<-Foption_tab
      b[b$Species== input$HCR.sp,'HCR']<-input$HCR
      b[b$Species== input$HCR.sp,'target.F']<-input$target.F 
      b[b$Species== input$HCR.sp,'T1']<-input$T1
      b[b$Species== input$HCR.sp,'T2']<-input$T2     
      Foption_tab<<-b
      doWriteOptions<<-TRUE
      doRunModel<<-TRUE
      OP.trigger<<-put_op_Fmodel(b,OP.trigger) #update OP_trigger
      return(b)
   }
  })
 
  

  otherDf <- eventReactive(input$updateOptionTableOther || input$Option=='Other predators',{
    #cat(input$updateOptionTableOther,input$Option,input$OtherFactor,input$OtherFirst,input$OtherSecond,'\n')
     if (input$Option=='Other predators') {
       b<-other_predators
       b[b$Predator== input$OtherSp,'Total.change']<- input$OtherFactor**(input$OtherSecond-input$OtherFirst+1)
       
       b[b$Predator== input$OtherSp,'change']<-input$OtherFactor
       b[b$Predator== input$OtherSp,'First.year']<-input$OtherFirst
       b[b$Predator== input$OtherSp,'Last.year']<-input$OtherSecond
       other_predators<<-b
       doWriteOptions<<-TRUE
       doRunModel<<-TRUE
       OP<<-put_other_predators(b,OP) #update OP
       return(b)
     }
   })
   
   
  df2 <- eventReactive( input$tabOpt=='Options', {return(Foption_tab)})

   # make a new prediction with detailed output
   doUpdateDetails<-function(){
     OP@output<<-25  #both condensed and annual output
     res$rv$out<-do_OP(readResSimple=TRUE,readResDetails=TRUE,writeOption=TRUE, writeExplPat=doWriteExplPattern,source='push Detailed')
     updateActionButton(session, inputId="doRunDetailed", label = 'Prediction is updated',icon = character(0))
   }

   # make a new prediction with detailed output and partial M2
   doUpdateDetailsM2<-function(){
     showModal(modalDialog("Doing a complex prediction run, please wait a few seconds", footer=NULL))
     OP@output<<-26  #both condensed and annual and quarterly  output and M2
     res$rv$out<-do_OP(readResSimple=TRUE,readResDetails=TRUE,readResStom=TRUE,writeOption=TRUE,writeExplPat=doWriteExplPattern,source='push DetailedM2')
     removeModal()
     updateSelectInput(session,inputId="whoPred",choices=c('all predators',tail(res$rv$out$pred,-1)))
     updateSelectInput(session,inputId="whoPrey",choices=c('all preys',res$rv$out$prey))
   }
  
   observeEvent(input$whoPred,{if (input$whoPred !='all predators') updateSelectInput(session,inputId="whoPrey",choices=c('all preys',res$rv$out$predPrey[[input$whoPred]])) })
   
   
   observeEvent(input$whoHuman,{if(input$whoHuman=='Excl. catch') choi<-c('all predators',tail(res$rv$out$pred,-1)) else choi<-c('all predators',res$rv$out$pred);
                                updateSelectInput(session,inputId="whoPred",choices=choi)})
   
  
   observeEvent(input$detailed_predict,{
     if (input$detailed_predict=='Results by year') doUpdateDetails() else if (input$detailed_predict=='Who eats whom') doUpdateDetailsM2()
    })
   
   observeEvent(input$doRunDetailed, {doUpdateDetails()})
  
   updateFoption_single<-function(sp){
     #sp<-input$HCR.sp
     updateNumericInput(session,inputId="target.F",value=Foption_tab[sp,'target.F'])
     updateNumericInput(session,inputId="T1",value=Foption_tab[sp,'T1'])
     updateNumericInput(session,inputId="T2",value=Foption_tab[sp,'T2'])
     updatePickerInput(session,inputId ="HCR",selected=Foption_tab[sp,'HCR'])
   }
     
   
  observeEvent(input$HCR.sp,{updateFoption_single(input$HCR.sp)})
  

  output$HCRtable1<- renderTable(df(),digits=3)
  output$HCRtable2<- renderTable(df2(),digits=3)
  
  output$Othertable<- renderTable(otherDf())
  
  observeEvent(input$F.all, {
     val <- input$F.all
     vals<-sapply(paste0("F.", fleetNames), function(item) input[[item]]) 
     if (input$effcontrolAll) purrr::walk(paste0("F.", fleetNames), function(id) updateSliderInput(session, id, value = val))
   },ignoreInit = TRUE)

 
  observeEvent(input$firstY,{
    updateSliderInput(session,inputId="lastY",min=input$firstY+5)
  })
   
  observeEvent(input$firstYwho,{
    updateSliderInput(session,inputId="lastYwho",min=input$firstYwho+5)
  })
  

  observeEvent(input$updateOptionTableOther,{
    updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("refresh"))
  })
  
  observeEvent(input$updateOptionTable,{
    updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("refresh"))
  })
  

  
  observe({
    # simple predictions  
   vals<-sapply(paste0("F.", fleetNames), function(item) input[[item]])
   if (any(vals!=oldFvals)) {
      res$rv$Fmulti<-vals
      OP@output<<-20  # condensed output
      OP.trigger@Ftarget['init',]<<-vals*stqF
     res$rv$out<-do_OP(readResSimple=TRUE,writeOption=doWriteOptions,source='simple prediction')
     oldFvals<<-vals
   }

   #detailed predictions, change of terminal year
    if (input$finalYear != termYear){
       termYear<<-input$finalYear
       doWriteOptions<<-TRUE
       doRunModel<<-TRUE
       updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("refresh"))
       OP@last.year<<-termYear
       OP.trigger@last.year<<-termYear
       updateSliderInput(session,inputId="lastY",max=input$finalYear)
       updateSliderInput(session,inputId="lastYwho",max=input$finalYear)
    }

   if (input$recDetSto != recruitMode) {
     doWriteOptions<<-TRUE
     doRunModel<<-TRUE
     updateActionButton(session, inputId="doRunDetailed", label = 'Push to update prediction',icon = icon("refresh"))
     
     if (input$recDetSto=='Determenistic') {
       OP@stochastic.recruitment[1,]<<- rep(0,n.VPA)
       OP@recruit.adjust[1,]<<-hcr_ini$rec.adjust.single
       OP@recruit.adjust.CV[1,]<<- hcr_ini$rec.adjust.CV.single
       OP.trigger@HCR[1,]<<- 1
       
     } else if (input$recDetSto=='Stochastic') {
       OP@stochastic.recruitment[1,]<<- rep(1,n.VPA)
       OP@recruit.adjust.CV[1,]<<- rep(0,n.VPA)
       OP@recruit.adjust[1,]<<-hcr_ini$rec.adjust
       
       OP.trigger@HCR[1,]<<-hcr_ini$HCR
       OP.trigger@Ftarget['init',]<<-hcr_ini$Ftarget
       
       Foption_tab<<-get_op_Fmodel()
       updateFoption_single(input$HCR.sp)
     }
     recruitMode<<-input$recDetSto 
    
   } #end observe
  
 
   
   if (input$Option>0)   updateRadioButtons(session=session,inputId = 'tabOpt',selected='Results')

   ###### change Baseline in plots
   baseline_handle<-function(h){
     v<-paste0(c("bas_F_","bas_SSB_","bas_Yield_","bas_Rec_"),h)
     vv<-sapply(v, function(item) input[[item]]) 
     if (vv[1] >"0") {
       if (vv[1]=="1") res$rv$baseLine[,'Fbar']<-stqF
       if (vv[1]=="2") res$rv$baseLine[,'Fbar']<-unlist(res$rv$out$a[,"Fbar"])
       updateRadioButtons(session=session,inputId = v[1],choiceNames=bsF$Names,choiceValues = bsF$Values)
     }
     
     if (vv[2] >"0")   {
       if (vv[2]=="1")  res$rv$baseLine[,'SSB']<-stqSSB
       if (vv[2]=="2")  res$rv$baseLine[,'SSB']<-unlist(res$rv$out$a[,"SSB"])
       updateRadioButtons(session=session,inputId = v[2],choiceNames=bsSSB$Names,choiceValues = bsSSB$Values)
     }
     if (vv[3] >"0")   {
       if (vv[3]=="1") res$rv$baseLine[,'Yield']<-stqYield
       if (vv[3]=="2") res$rv$baseLine[,'Yield']<-unlist(res$rv$out$a[,"Yield"])
       updateRadioButtons(session=session,inputId = v[3],choiceNames=bsYield$Names,choiceValues = bsYield$Values)
     }
     if (vv[4] >"0"   ) {
       if(vv[4]=="1") res$rv$baseLine[,'Recruits']<-stqRec
       if(vv[4]=="2") res$rv$baseLine[,'Recruits']<-unlist(res$rv$out$a[,"Recruits"])
       if(vv[4]=="3") res$rv$baseLine[,'Recruits']<-max_rec
       updateRadioButtons(session=session,inputId = v[4],choiceNames=bsRec$Names,choiceValues = bsRec$Values)
     }
   }
   
  
   if (input$simple_predict  =="Change Baseline") baseline_handle(h='s')   ###### change simple Baseline
   if (input$detailed_predict=="Change Baseline") baseline_handle(h='d')   ######  change detailed Baseline

  })


}

shinyApp(ui = ui, server = server)
