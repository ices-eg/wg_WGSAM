rm(list = ls())
suppressPackageStartupMessages(library(shiny))
suppressMessages(library(DT))
library(readr)
#library(stringr)
suppressMessages(library(dplyr))
library(ggplot2)
#library(ggtext) 
#library(ggpubr) #combined plot (not that good)
suppressMessages(library(wordcloud))


#library(radarchart)

library("shinyWidgets")

#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)

# source("dragables.R")

test<-FALSE

## Globals
data_dir <- "Data"
OS<- .Platform$OS.type  #operating system

# reset option files to default values
file.copy(file.path(data_dir,'op_config_master.dat'),file.path(data_dir,'op_config.dat'),overwrite = TRUE)


# control objects for predictions
data.path<-data_dir # used by the control objects
source("flsms.control.r") # to handle file SMS.dat with options for running SMS in hindcast and producing data for forecast

SMS<-read.FLSMS.control(file='sms.dat',dir=data_dir)
#get information from SMS run
n.species.tot <- SMS@no.species  # number of species including "other predators"
n.pred.other<-sum(SMS@species.info[,'predator']==2) #number of "other predators"
n.VPA<-n.species.tot-n.pred.other #number of species with analyttical assessment
n.pred<-n.pred.other+sum(SMS@species.info[,'predator']==1) # number of predators
n.fleet <- n.VPA   # in this case it is just the number of species, one "fleet" per species
stq_year<-SMS@last.year.model
fy_year_hist<-SMS@first.year.model
#spNames<-str_replace_all(SMS@species.names, fixed(" "), "")  #remove spaces
spNames<-SMS@species.names

firstVPA<-n.species.tot-n.VPA+1   #first species with analytical assessment
VPA.spNames<-spNames[firstVPA:n.species.tot]
spOtherNames<-c('Other.food',spNames)

# group predator names for eaten biomass
pred_format<-read.csv(file.path(data_dir,'pred_format.csv'),header=TRUE)
pp<-unique(subset(pred_format,select=c(new,new_no)))
pp<-pp[order(pp$new_no),]
predPreyFormat<-pp$new



# options for predictions, reset from master version
source("flop.control.r")
OP<-read.FLOP.control(file="op_master.dat",path=data_dir,n.VPA=n.VPA,n.other.pred=n.pred.other,n.pred=n.pred)
OP.trigger<-read.FLOPtrigger.control(file="op_trigger_master.dat",path=data_dir,n.VPA=n.VPA,n.other.pred=n.pred.other)


# Units (multiplier) in output
plotUnits<- c(Yield=0.001,          Fbar=1,   SSB=0.001,          TSB=0.001,           Recruits=0.001,        DeadM=0.001)
plotLabels<-c(Yield='(1000 tonnes)',Fbar=" ", SSB='(1000 tonnes)',TSB='(1000 tonnes)', Recruits='(millions)', DeadM='(1000 tonnes)')
roundUnits<- c(Yield=1,             Fbar=3,   SSB=1,              TSB=1,               Recruits=0,            DeadM=1)

# read status quo values (= values in the terminal SMS model year)  
status_quo<-read_csv(file=file.path(data_dir,'status_quo.csv'),col_types = cols()) %>%
  mutate(Rec=Rec*plotUnits['Recruits'],SSB=SSB*plotUnits['SSB'],TSB=TSB*plotUnits['TSB'],SOP=SOP*plotUnits['Yield'],
         Yield=Yield*plotUnits['Yield'],mean.F=mean.F*plotUnits['Fbar'],Eaten=Eaten*plotUnits['DeadM'])

# read hindcast SMS values  
histAnnoM<-read_csv(file=file.path(data_dir,'hist_anno_M.csv'),col_types = cols())%>%mutate(Species=spNames[Species.n])
histCondensed<-read_csv(file=file.path(data_dir,'hist_condensed.csv'),col_types = cols())%>%mutate(Species=spNames[Species.n]) %>%
  mutate(Recruits=Recruits*plotUnits['Recruits'],SSB=SSB*plotUnits['SSB'],TSB=TSB*plotUnits['TSB'],Yield=Yield*plotUnits['Yield'],
         Fbar=Fbar*plotUnits['Fbar'],DeadM1=DeadM1*plotUnits['DeadM'],DeadM2=DeadM2*plotUnits['DeadM'])



# SMS output values in the last year
base_SSB<-stqSSB<-status_quo$SSB
base_F<-stqF<-status_quo$mean.F
#stqF<-status_quo$sum.q.F
base_Yield<-stqYield<-status_quo$Yield
base_Rec<-stqRec<-status_quo$Rec


# change option values (could have been done in the master file!)
OP.trigger@Ftarget['init',]<-stqF
OP.trigger@Ftarget['lower',]<-0
OP.trigger@Ftarget['higher',]<-3


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

# values for baselines options
bsF<-list(Names=list('No change',paste0('F(',stq_year,')'),'Most recent results'),Values=list(0,1,2))
bsSSB<-list(Names=list('No change',paste0('SSB(',stq_year,')'),'Most recent results'),Values=list(0,1,2))
bsYield<-list(Names=list('No change',paste0('Yield(',stq_year,')'),'Most recent results'),Values=list(0,1,2))
bsRec<-list(Names=list('No change',paste0('Recruitment(',stq_year,')'),'Most recent results','Maximum recrutiment'),Values=list(0,1,2,3))


F_mult<-1.0  

Fvalues<-stqF*F_mult

oldwd<-getwd()

# command file for executing the OP program
if (OS== "windows") cat("cd ",file.path(oldwd,data_dir),"\n","op.exe -maxfn 0 -nohess   >ud.dat\n",  file=file.path(oldwd,data_dir,"run_op.bat"))
if (OS=="unix") {}


do_baseLine<-function(){
  baseLine=cbind(SSB=base_SSB,Fbar=base_F,Yield=base_Yield,Recruits=base_Rec)
  rownames(baseLine)<-VPA.spNames
  return(baseLine)
}

op.n<-0 #counter for calls to op.exe, used for tests only

# call to the OP program
do_OP<-function(readResSimple=TRUE,readResDetails=FALSE,readResStom=FALSE,WriteOption=FALSE,source='') {
  op.n<<-op.n+1
  cat(op.n, "call source:", source, " readResSimple:",readResSimple," readResDetails:",readResDetails," readResStom:",readResStom," WriteOption:",WriteOption,'\n')
  
   # write the F values
   Fvalues<-OP.trigger@Ftarget['init',]
   cat("1\n",Fvalues,"\n",file=file.path(data_dir,"op_multargetf.in")) # write F values

   if (WriteOption) {  #write option files
     write.FLOP.control(OP,file="op.dat",path=data_dir,nice=TRUE,writeSpNames=FALSE)   
     write.FLOPtrigger.control(OP.trigger,file="op_trigger.dat",path=data_dir,nice=FALSE,writeSpNames=FALSE)
     doWriteOptions<<-FALSE
   } 
   # run the script
   system2(command=file.path(oldwd,data_dir,"run_OP.bat"), wait = TRUE,stdout = file.path(oldwd,data_dir,"Run_OP_out.dat")) 
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
                   SSB=d$SSB*plotUnits['SSB'], Recruits=d$recruit*plotUnits['Recruits'], 
                   DeadM1=(d$DeadM-d$DeadM2)*plotUnits['DeadM'], DeadM2=d$DeadM2*plotUnits['DeadM'])
    
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
    
    #b<-subset(M2,select=c( Year, Quarter, Predator,Predator.age, Prey, Prey.age,Prey.no, eatenW, Part.M2,tot.M2.prey))
    #bb<-droplevels(aggregate(list(eatenW=b$eatenW),list(Year=b$Year, Quarter=b$Quarter, Predator=b$Predator,Prey=b$Prey),sum))
    bbb<-droplevels(aggregate(list(eatenW=M2$eatenW),list(Year=M2$Year, Predator=M2$Predator,Prey=M2$Prey,Prey.no=M2$Prey.no),sum))
    bbb$eatenW<-bbb$eatenW*plotUnits['DeadM']

    s<-merge(x=bbb,y=pred_format,by.x='Prey',by.y='old',all.x=TRUE)
    s$Prey<-s$new; s$new<-NULL
    s$Prey.no<-s$new_no; s$new_no<-NULL
    
    s<-merge(x=s,y=pred_format,by.x='Predator',by.y='old',all.x=TRUE)
    
    s<-aggregate(s$eatenW,list(s$new,s$Year,s$new_no,s$Prey,s$Prey.no),sum)
    names(s)<-c("Predator","Year","Predator.no","Prey","Prey.no","eatenW")
    
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


# Radar plot function
plot_one<-function(x,type='Yield',plot.legend = TRUE) {
  a1 <- filter(x$out$b,variable==type)
  a1[,2:(n.fleet+1)]<- a1[,2:(n.fleet+1)]/x$baseLine[,type]
  
  a2<- a1
  a2[,2:(n.fleet+1)]<- 1
  a<-bind_rows(a1,a2)
  tit<-ifelse(type=='Fbar'," F",paste0(' ',type))
  a[1,'variable']<-tit
  a[2,'variable']<-'Baseline'
  gmax<-max( a[,2:(n.fleet+1)])
  ggradar(a,grid.max=gmax,grid.min=0,plot.title='',plot.legend=plot.legend,legend.position='top',legend.text.size = 18)
}

plot_all<-function(res) {
  p_Fbar<-plot_one(res,type='Fbar')
  p_Yield<-plot_one(res,type='Yield')
  p_Recruits<-plot_one(res,type='Recruits')
  p_SSB<-plot_one(res,type='SSB')
  
  figure <- ggarrange(p_Fbar, p_Yield, p_Recruits,p_SSB,
                      labels = NULL,
                      ncol = 2, nrow = 2)
  figure
}



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
#put_op_Fmodel(Foption_tab,OP.trigger)

doRunModel<-TRUE  # flag for re-running the prediction model
doWriteOptions<-TRUE  # flag for writing option files for the prediction model

hcr$img = c(
  sprintf("<img src='fixed_F.png' width=100px><div class='jhr'>%s</div></img>", hcr$val[1]),
  sprintf("<img src='AR_F_SSB.png' width=100px><div class='jhr'>%s</div></img>", hcr$val[2]),
  sprintf("<img src='AR_F_TSB.png' width=100px><div class='jhr'>%s</div></img>", hcr$val[3])
)

get_terminal_year<-function(OP){
  return(OP@last.year)
}

termYear<-get_terminal_year(OP)
oldFvals<-rep(1.0,n.fleet)


# Data for table with the most important data
makeResTable<-function(x){
  a<-data.frame(Species=VPA.spNames,
                F_stq=round(stqF,3),
                F_new=round(x$out$a[,'Fbar'],roundUnits['Fbar']),
                F_change=(x$out$a[,'Fbar']-stqF)/stqF,
                Yield_stq=round(stqYield,roundUnits['Yield']),
                Yield_new=round(x$out$a[,'Yield'],roundUnits['Yield']),
                Y_change=(x$out$a[,'Yield']-stqYield)/stqYield,
                SSB_stq=round(stqSSB,roundUnits['SSB']),
                SSB_new=round(x$out$a[,'SSB'],roundUnits['SSB']),
                SSB_change=(x$out$a[,'SSB']-stqSSB)/stqSSB,
                rec_stq=round(stqRec,roundUnits['Recruits']),
                rec_new=round(x$out$a[,'Recruits'],roundUnits['Recruits']))

  colnames(a)<-c("Species",paste0('F(',stq_year,')'),paste0('F(',termYear,')'),'F change',
                           paste0('Yield(',stq_year,') ',plotLabels['Yield']),
                           paste0('Yield(',termYear,') ', plotLabels['Yield']),'Yield change',
                           paste0('SSB(',stq_year,') ',plotLabels['SSB']),
                           paste0('SSB(',termYear,') ',plotLabels['SSB']),'SSB change',
                           paste0('Rec(',stq_year,') ',plotLabels['Recruits']),
                           paste0('Rec(',termYear,') ',plotLabels['Recruits']))
  return(a)
}


if (test) { # simple predictions
  res<-  list(out=do_OP(),Fmulti=rep(F_mult,n.fleet),baseLine=do_baseLine(),source='test')
  res
  plot_one(res,type='Fbar',plot.legend = TRUE)
  plot_one(res,type='Yield')
  plot_one(res,type='Recruits')
  plot_one(res,type='SSB')
  
  plot_all(res)
  makeResTable(res)
}


plot_summary<-function(res,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),years=c(0,5000),species='Cod',splitLine=FALSE,incl.reference.points=FALSE,nox=2,noy=3) {
  a<-subset(res$out$detail_sum,Year>=years[1] & Year<=years[2] & Species %in% species)
  a2<-subset(histCondensed,Year>=years[1] & Year<=years[2] & Species %in% species)
  a<-bind_rows(a,a2)%>% arrange(Year)
  
  b<-subset(res$out$detail_M2,Year>=years[1] & Year<=years[2] & Species %in% species)
  b2<-subset(histAnnoM,Year>=years[1] & Year<=years[2] & Species %in% species)
  b<-bind_rows(b,b2) %>% arrange(Year,Age)
  
  #X11(width=11, height=8, pointsize=12)
  par(mfcol=c(nox,noy))
  par(mar=c(3,4,3,2))
  

  if ("SSB" %in% ptype) {
    plot(a$Year,a$SSB,type='b',lwd=3,xlab='',ylab=plotLabels['SSB'],main=paste(species,'SSB',sep=', '),ylim=c(0,max(a$SSB)))
    if (splitLine) abline(v=SMS.control@last.year.model,lty=2, col='red')
    #if (incl.reference.points) if (Blim>0) abline(h=Blim,lty=2,lwd=2)
    #if (incl.reference.points) if (Bpa>0) abline(h=Bpa,lty=3,lwd=2)
  }
  
  if ("Recruits" %in% ptype) {
    y<-tapply(a$Recruits,list(a$Year),sum)
    barplot(y,space=1,ylab=plotLabels['Recruits'],xlab='',main=paste('Recruitment',sep=', '),ylim=c(0,max(y)))
    if (splitLine) abline(v=stq_year,lty=2, col='red')
  }  
 
  if ("Fbar" %in% ptype) {
    plot(a$Year,a$Fbar,type='b',lwd=3,xlab='',ylab=plotLabels['Fbar'],main="Fishing mortality (F)",ylim=c(0,max(a$Fbar)))
    if (splitLine) abline(v=SMS.control@last.year.model,lty=2, col='red')
    #if (incl.reference.points) if (ref[sp,"Flim"]>0) abline(h=ref[sp,"Flim"],lty=2,lwd=2)
    #if (incl.reference.points) if (ref[sp,"Fpa"]>0) abline(h=ref[sp,"Fpa"],lty=3,lwd=2)
  } 

    if ("Yield" %in% ptype) {
    y<-tapply(a$Yield,list(a$Year),sum)
    barplot(y,space=1,ylab=plotLabels['Yield'],xlab='',main='Catch',ylim=c(0,max(y)))
    if (splitLine) abline(v=stq_year,lty=2, col='red')
  }  
  
  if ("Dead" %in% ptype) {
    Yield<-tapply(a$Yield,list(a$Year),sum)
    deadM1<-tapply(a$DeadM1,list(a$Year),sum)
    deadM2<-tapply(a$DeadM2,list(a$Year),sum)
    barplot(rbind(Yield,deadM1,deadM2),space=1,main='Biomass removed\ndue to F, M1 and M2',ylab=plotLabels['DeadM'],
            col=c('red','green','plum'))

    if (splitLine) abline(v=stq_year,lty=2, col='red')
  }  
  
  if ("M2" %in% ptype) if (any(b$M2>0)) {
    M2<-tapply(b$M2,list(b$Year,b$Age),sum)
     y<-as.numeric(dimnames(M2)[[1]])
      plot(y,M2[,1],main=paste("M2 at age"),xlab="",ylab=plotLabels['M2'],
           type='l',lwd=1.5,ylim=c(0,max(M2,na.rm=T)))
      for (a in (2:(dim(M2)[2]))) if(max(M2[,a],na.rm=T)>0.001) lines(y,M2[,a],lty=a,col=a,lwd=2)
      if (splitLine) abline(v=SMS.control@last.year.model,lty=2, col='red')
    }
}



plot_who_eats<-function(x,pred,prey,predPrey='by prey',years=c(0,5000)){
  
  x<-filter(x,Year>=years[1] & Year<=years[2])
  
  if (prey != 'all preys') {
    tit<-paste(prey, "eaten by"); 
    x<-filter(x,Prey==prey) 
  } else tit<-'All preys eaten by'
  
  if (pred !='all predators') {
     x<-filter(x,Predator==pred); 
     tit<-paste(tit,"by",pred)
   } else tit<-paste(tit,'by all predators')
  
  scale_fill_pp <- function(...){
    ggplot2:::manual_scale(
      'fill', 
      values = setNames(rainbow(length(predPreyFormat)), predPreyFormat), 
      ...
    )
  }
  
  if (predPrey=='by prey') {
    x <-aggregate(eatenW~Year+Prey,data=x,sum)
    p<-ggplot(x, aes(x = Year, y = eatenW, fill = Prey))  

  } else   if (predPrey=='by predator') {
    x <-aggregate(eatenW~Year+Predator,data=x,sum)
    p<-ggplot(x, aes(x = Year, y = eatenW, fill = Predator)) 
  }
  p +  geom_bar(stat = "identity") + 
       scale_fill_pp() +
       labs(x="", y = paste("Biomass eaten",plotLabels['DeadM'])) +
       ggtitle(tit) +
       theme_minimal() +
       theme(plot.title = element_text(size = 18, face = "bold")) 
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
        tabPanel(title='ReadMeE',  
            h2('Velkommen til ...')),     
        tabPanel(title='Simple predictions',
               tabsetPanel(id='simple_predict',        
                   tabPanel(title = "Predictions",
                    column(3,
                          checkboxInput("effcontrolAll", "Same factor for all fleets?", value = TRUE),
                          conditionalPanel("input.effcontrolAll==1", sliderInput("F.all", "F factor",
                                                       min = 0.5, max = 2.0, value = 1, step = 0.05)),
                          conditionalPanel("input.effcontrolAll==0",sliders),
                    ),     
                    column(4,
                         plotOutput(outputId = "F_plot1"),
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
                      radioButtons(inputId = 'bas_F_s',label='Baseline for F',choiceNames=bsF$Names,choiceValues = bsF$Values),
                      radioButtons(inputId = 'bas_Rec_s',label='Baseline for recruitment',choiceNames=bsRec$Names,choiceValues = bsRec$Values),
                      radioButtons(inputId = 'bas_Yield_s',label='Baseline for yield',choiceNames=bsYield$Names,choiceValues = bsYield$Values),
                      radioButtons(inputId = 'bas_SSB_s',label='Baseline for SSB',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values)),
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
                    column(5, 
                        # for picture in pickerInput   
                         tags$head(tags$style("
                           .jhr{
                           display: inline;
                           vertical-align: middle;
                           padding-left: 10px;
                           }")),
                          br(),
                          wellPanel(radioButtons(inputId='Option', label='Select option', choices=list('Final year','F model','Other pedators'))),
                          wellPanel(
                            conditionalPanel("input.Option=='F model'",selectInput(inputId="HCR.sp", label="Species",choices=VPA.spNames)),
                            conditionalPanel("input.Option=='F model'",pickerInput(inputId = "HCR",
                                      label = "Harvest Control Rule",
                                      choices = hcr$val,
                                      choicesOpt = list(content = hcr$img))),
                           conditionalPanel("input.Option=='F model'"                           ,numericInput(inputId="target.F",label="Target F",value=Foption_tab[1,'target.F'],min=0,max=2,step=0.01)),
                           splitLayout(
                            conditionalPanel("input.Option=='F model' & input.HCR!=' 1: Fixed F'",numericInput(inputId="T1",label=paste("T1",plotLabels['SSB']),value=Foption_tab[1,'T1'],min=0,step=1)),
                            conditionalPanel("input.Option=='F model' & input.HCR!=' 1: Fixed F'",numericInput(inputId="T2",label=paste("T2",plotLabels['SSB']),value=Foption_tab[1,'T2'],min=0,step=1))
                         ),
                         conditionalPanel("input.Option=='Other pedators'",column(8,DTOutput(outputId="HCRtableIn"))),
                         
                          conditionalPanel("input.Option=='Final year'",sliderInput(inputId="finalYear",label="Last year in prediction",min = stq_year+1, max = stq_year+100, value = termYear, step =1)),
                          conditionalPanel("input.Option=='F model'",actionButton(inputId="updateOptionTable", "Update option table")))
                           ),
                    conditionalPanel("input.Option=='F model'",column(7,br(),tableOutput(outputId="HCRtable"))),
                  ),
                  tabPanel(title='Results',
                           column(4,   
                             br(),br(),
 
                           actionButton(inputId="doRunDetailed",label="Push to update prediction",icon("refresh")),
                            ),
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
                          radioButtons(inputId = 'bas_SSB_d',label='Baseline for SSB',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values)
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
                 tabPanel(title="Results by year",
                      column(2,br(),
                         selectInput(inputId="sumSpecies", label="Select Species:",choices=VPA.spNames),
                         sliderInput(inputId="firstY",label="First year output",value=stq_year+1,min=fy_year_hist,max=termYear,step=1),
                         sliderInput(inputId="lastY",label="Last year output",value=termYear,min=fy_year_hist+5,max=termYear,step=1),
                         br(),
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
                          radioButtons(inputId="whoPredPrey",label='select value for stacking',choices = c('by prey','by predator')),
                          wellPanel(
                             sliderInput(inputId="firstYwho",label="First year output",value=stq_year+1,min=fy_year_hist,max=termYear,step=1),
                            sliderInput(inputId="lastYwho",label="Last year output",value=termYear,min=fy_year_hist+5,max=termYear,step=1)
                          ),
                       ),
                      column(9,br(),plotOutput(outputId = "whoEats_plot"))
                )
                 
            ))

)

 server <- function(input, output, session) {
   res <- reactiveValues(rv = list(out=do_OP(readResSimple=TRUE,WriteOption=doWriteOptions,source='init'),Fmulti=rep(F_mult,n.fleet),baseLine=do_baseLine()))   
 
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
   
   output$all_plot   <- renderPlot({ plot_all(res$rv) })
   
   output$summary_plot <-renderPlot({   if (res$rv$out$options$readResDetails) plot_summary(res$rv,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),
                                    years=c(input$firstY,input$lastY),species=input$sumSpecies,splitLine=FALSE,incl.reference.points=FALSE)},
                                    width = 1350, height=750,units = "px", pointsize = 25, bg = "white")
   
   output$whoEats_plot <- renderPlot({ plot_who_eats(res$rv$out$detail_eaten,pred=input$whoPred,prey=input$whoPrey,predPrey=input$"whoPredPrey",
                                                     years=c(input$firstYwho,input$lastYwho))})
   
   output$tableOut1 <- renderDT(
     DT::datatable(makeResTable(res$rv),rownames=FALSE, filter ="none",options=list(pageLength = n.VPA))  %>% 
       formatPercentage(columns=c(4,7,10),digits=1)  
   ) 
   
   
   output$HCRtableIn<- renderDT(
     DT::datatable(df_opt(),rownames=FALSE,filter ="none",editable = list(target = "row", disable = list(columns = c(1))),options=list(pageLength = n.VPA)) 
   ) 

   output$statusRun<-renderText(paste("doRunModel",doRunModel ,ifelse(doRunModel,'Prediction has not been updated','Up to data prediction')))
   
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
 
   # make a new prediction with detailed output
   doUpdateDetails<-function(){
     OP@output<<-25  #both condensed and annual output
     res$rv$out<-do_OP(readResSimple=TRUE,readResDetails=TRUE,WriteOption=TRUE,source='push Detailed')
     updateActionButton(session, inputId="doRunDetailed", label = 'Prediction is updated',icon = character(0))
   }

   # make a new prediction with detailed output and partial M2
   doUpdateDetailsM2<-function(){
     showModal(modalDialog("Doing a complex prediction run, please wait a few seconds", footer=NULL))
     OP@output<<-26  #both condensed and annual and quarterly  output and M2
     res$rv$out<-do_OP(readResSimple=TRUE,readResDetails=TRUE,readResStom=TRUE,WriteOption=TRUE,source='push DetailedM2')
     removeModal()
     
     updateSelectInput(session,inputId="whoPred",choices=c('all predators',res$rv$out$pred))
     updateSelectInput(session,inputId="whoPrey",choices=c('all preys',res$rv$out$prey))
   }
  
    observeEvent(input$whoPred,{if (input$whoPred !='all predators') updateSelectInput(session,inputId="whoPrey",choices=c('all preys',res$rv$out$predPrey[[input$whoPred]])) })
  
   
   observeEvent(input$detailed_predict,{
     if (input$detailed_predict=='Results by year') doUpdateDetails() else if (input$detailed_predict=='Who eats whom') doUpdateDetailsM2()
    })
   
   observeEvent(input$doRunDetailed, {doUpdateDetails()})
   
   
  observeEvent(input$HCR.sp,{
   sp<-input$HCR.sp
   updateNumericInput(session,inputId="target.F",value=Foption_tab[sp,'target.F'])
   updateNumericInput(session,inputId="T1",value=Foption_tab[sp,'T1'])
   updateNumericInput(session,inputId="T2",value=Foption_tab[sp,'T2'])
   hcr<-input$HCR
   updatePickerInput(session,inputId = "HCR",selected=hcr)
  })
  

  output$HCRtable<- renderTable(df(),digits=3)
  
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
  
  # output$testdrag <- renderDragableChart({
  #   as.numeric(res$rv$Fmulti)
  # }, labels = fleetNames, sameval = input$effcontrolAll)
  
  
  observe({
  #cat(input$simple_predict,'\n')
   
    
   # simple predictions  
   vals<-sapply(paste0("F.", fleetNames), function(item) input[[item]])
   if (any(vals!=oldFvals)) {
      res$rv$Fmulti<-vals
      OP@output<<-20  # condensed output
      OP.trigger@Ftarget['init',]<<-vals*stqF
     res$rv$out<-do_OP(readResSimple=TRUE,WriteOption=doWriteOptions,source='simple prediction')
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
       cat("input$finalYear:",input$finalYear,'\n')
    }
   
     ###### change simple Baseline
     if (input$bas_F_s >0) {
       if (input$bas_F_s==1) res$rv$baseLine[,'Fbar']<-stqF
       if (input$bas_F_s==2) res$rv$baseLine[,'Fbar']<-unlist(res$rv$out$a[,"Fbar"])
       updateRadioButtons(session=session,inputId = 'bas_F_s',choiceNames=bsF$Names,choiceValues = bsF$Values)
     }
   
     if (input$bas_SSB_s >0)   {
       if (input$bas_SSB_s==1)  res$rv$baseLine[,'SSB']<-stqSSB
       if (input$bas_SSB_s==2)  res$rv$baseLine[,'SSB']<-unlist(res$rv$out$a[,"SSB"])
       updateRadioButtons(session=session,inputId = 'bas_SSB_s',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values)
     }
     if (input$bas_Yield_s >0)   {
       if (input$bas_Yield_s==1) res$rv$baseLine[,'Yield']<-stqYield
       if (input$bas_Yield_s==2) res$rv$baseLine[,'Yield']<-unlist(res$rv$out$a[,"Yield"])
       updateRadioButtons(session=session,inputId = 'bas_Yield_s',choiceNames=bsYield$Names,choiceValues = bsYield$Values)
     }
     
     if (input$bas_Rec_s >0   ) {
       if(input$bas_Rec_s==1) res$rv$baseLine[,'Recruits']<-stqRec
       if(input$bas_Rec_s==2) res$rv$baseLine[,'Recruits']<-unlist(res$rv$out$a[,"Recruits"])
       if(input$bas_Rec_s==3) res$rv$baseLine[,'Recruits']<-max_rec
       updateRadioButtons(session=session,inputId = 'bas_Rec_s',choiceNames=bsRec$Names,choiceValues = bsRec$Values)
     }
    
   ######  change detailed baseLine
   if (input$bas_F_d >0) {
     if (input$bas_F_d==1) res$rv$baseLine[,'Fbar']<-stqF
     if (input$bas_F_d==2) res$rv$baseLine[,'Fbar']<-unlist(res$rv$out$a[,"Fbar"])
     updateRadioButtons(session=session,inputId = 'bas_F_d',choiceNames=bsF$Names,choiceValues = bsF$Values)
   }
   
   if (input$bas_SSB_d >0)   {
     if (input$bas_SSB_d==1)  res$rv$baseLine[,'SSB']<-stqSSB
     if (input$bas_SSB_d==2)  res$rv$baseLine[,'SSB']<-unlist(res$rv$out$a[,"SSB"])
     updateRadioButtons(session=session,inputId = 'bas_SSB_d',choiceNames=bsSSB$Names,choiceValues = bsSSB$Values)
   }
   if (input$bas_Yield_d >0)   {
     if (input$bas_Yield_d==1) res$rv$baseLine[,'Yield']<-stqYield
     if (input$bas_Yield_d==2) res$rv$baseLine[,'Yield']<-unlist(res$rv$out$a[,"Yield"])
     updateRadioButtons(session=session,inputId = 'bas_Yield_d',choiceNames=bsYield$Names,choiceValues = bsYield$Values)
   }
   
   if (input$bas_Rec_d >0   ) {
     if(input$bas_Rec_d==1) res$rv$baseLine[,'Recruits']<-stqRec
     if(input$bas_Rec_d==2) res$rv$baseLine[,'Recruits']<-unlist(res$rv$out$a[,"Recruits"])
     if(input$bas_Rec_d==3) res$rv$baseLine[,'Recruits']<-max_rec
     updateRadioButtons(session=session,inputId = 'bas_Rec_d',choiceNames=bsRec$Names,choiceValues = bsRec$Values)
   }
   ####
 # if (input$doRunDetailed){
#       res$rv$Fmulti<-vals
#       res$rv$out<-do_OP(vals*stqF,readResSimple=TRUE,WriteOption=TRUE,source='Detailed')
#  }
     
  })


}

shinyApp(ui = ui, server = server)
