
memory.limit(size = 85000)

############################
## North Sea or Baltic Sea runs
my.area<-c('North Sea','Baltic Sea')[1]   

##########  Choose arae (above) and one of the following scenarios
 scens<-c('HCR_1_deter_noadjust_',    # 1 Fixed F (HCR=1),  wide F range and large F steps, and NO recruitment median to mean adjustment 
          'HCR_1_deter_adjust_')      # 2 Fixed F (HCR=1),  wide F range and large F steps, and recruitment median to mean adjustment 

 

sce<-scens[2]        # Make your choice

batch<-F  # with batch==T several runs are made with condensed output, while batch==F makes one run with detalied output

##################################################################################

automatic.settings<-T   # set options automaticacly form sceenario name (sce) 

# Stochastic recruitment.
# 0=No,  S-R parameters from OP_config.dat
# 1=Yes  S-R parameters and variance from OP_config.dat
# 2=Yes, S-R parameters from OP_config.dat and variance-covariance matrix from file covariance_Rec.in
# 3=Yes, S-R parameters from OP_config.dat and residuals from OP_SSB_rec_residuals.in

stochastic.recruitment<-1
if (automatic.settings) {
 if (grepl('_stoch_',sce)) stochastic.recruitment<-3 else if (grepl('_deter_',sce)) stochastic.recruitment<-0
}


# adjust recruitment with half of the variance (factor exp(-(CV^2)/2) option adjust.recruit.CV
# 0=no adjustment\n",
# 1=stochastic recruitment, do adjustment such that average recruitment becomes equal to the median (downward adjustment)\n",
# 2=determenistic recruitment, do ajustment such that recruitment becomes equal to the mean of the log-normal distribution (upward adjustment)\n", file=file,append=T,sep="")
recruit.adjust.CV<-0        
if (automatic.settings) {
 if (grepl("_noadjust_",sce)) recruit.adjust.CV<-0
 if (grepl("_adjust_",sce)) {
   if (stochastic.recruitment==0) recruit.adjust.CV<-2 else  recruit.adjust.CV<-1
 }
}


source(file=file.path(prog.path,'HCR_OP_batch_simulate_function.R'))
 
if (my.area=='North Sea') { 
  #                     Cod     Whiting     Haddock      Saithe     Herring     N Sandeel   S Sandeel Nor. pout       Sprat      Plaice        Sole 
  HCR1<-            c(    1,          1,          1,          1,          1,          1,          1,        1,          1,          1,          1) 
  HCR2<-            c(    2,          2,          2,          2,          2,         22,         22,        22,         22,          2,          2) 
  my.FBlim.adjust<- c(    0,          0,          0,          0,          0,          1,          1,         1,          1,          0,          0)     # used for HCR2 to simulate escapement startegy for short lived sp
  my.FBlim.adjust3<-c(    1,          1,          1,          1,          1,          1,          1,         1,          1,          0,          0)     # used for HCR3
} else if (my.area=='Baltic Sea') {
  #                     Cod       Herring        Sprat   
  HCR1<-            c(    1,          1,          1) 
  HCR2<-            c(    2,          2,          2)  
  my.FBlim.adjust<- c(    0,          0,          0)
  my.FBlim.adjust3<-c(    1,          1,          1)
}

local.HCR<-HCR1        
if (automatic.settings) {
 if (grepl("HCR_1",sce)) local.HCR<-HCR1
 if (grepl("HCR_2",sce)) local.HCR<-HCR2
}
if (automatic.settings) {
  cat('\nstochastic.recruitment is set to:',  stochastic.recruitment,' from scenario name:',sce,'\n') 
  cat('\nrecruit.adjust.CV is set to:', recruit.adjust.CV,' from scenario name:',sce,'\n') 
  cat('\nHCR is set to :', local.HCR,' from scenario name:',sce,'\n') 
}


first.year.output<-2014
do.indicators<-F

if (stochastic.recruitment==0) { 
  my.last.year<-2081; years.in.average<-2;   no.iters<-1 
} else {
  my.last.year<-2111; years.in.average<-100; 
  no.iters<-c(2,2,2,2,2)  # number of iterations in each batch run by CPUE kernel (max 5 kernels)
}
no.iter.stoch<-sum(no.iters)


# scenarios

#a<-data.frame(COD=0.4,WHG=0.05,HAD=0.3,POK=0.4,HER=0.25,NSA=0.30,SSA=0.3,NOP=0.3,SPR=0.3,PLE=.25,SOL=0.20,sp.change='all',change=1.00)
a<-data.frame(COD=0.4,WHG=0.15,HAD=0.3,POK=0.4,HER=0.25,NSA=0.30,SSA=0.4,NOP=0.4,SPR=0.9,PLE=.25,SOL=0.20,sp.change='all',change=1.00)

# FSQ
#a<-data.frame(COD=0.25,WHG=0.16,HAD=0.16,POK=0.30,HER=0.30,NSA=0.22,SSA=0.44,NOP=0.12,SPR=0.26,PLE=0.25,SOL=0.22,sp.change='all',change=1.00)
nsp<-dim(a)[[2]]-2


b<-a

do_change<-function(change=1,baseLine) {
  for (i in (1:nsp)) {
     b2<-baseLine
     b2[1,i]<-b2[1,i]*change; b2$sp.change<-sp.names[first.VPA+i-1];b2$change<-change
     if (i==1)  b<-b2 else b<-rbind(b,b2)  
  }
  return(b)
}
if (batch) {
  change=1.1
  bb<-rbind(a,do_change(change=change,baseLine=a))
  b<-a; b[1,1:nsp]<-b[1,1:nsp]*change; b$change<-change
  bb<-rbind(bb,b)
  
  change=1.25
  bb<-rbind(bb,do_change(change=change,baseLine=a))
  b<-a; b[1,1:nsp]<-b[1,1:nsp]*change; b$change<-change
  bb<-rbind(bb,b)
  
  
  change=0.9
  bb<-rbind(bb,do_change(change=change,baseLine=a))
  b<-a; b[1,1:nsp]<-b[1,1:nsp]*change; b$change<-change
  bb<-rbind(bb,b)
  
  change=0.75
  bb<-rbind(bb,do_change(change=change,baseLine=a))
  b<-a; b[1,1:nsp]<-b[1,1:nsp]*change; b$change<-change
  bb<-rbind(bb,b)
  
  
  bb$run<-1:dim(bb)[[1]]
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  bout<-subset(bb,select=c(COD,WHG,HAD,POK,HER,NSA,SSA,NOP,SPR,PLE,SOL))
} else bout<-subset(b,select=c(COD,WHG,HAD,POK,HER,NSA,SSA,NOP,SPR,PLE,SOL))

cat(dim(bout)[[1]],'\n',file='OP_multargetF.in')
write.table(bout,file='OP_multargetF.in',col.names=F,row.names=F,append=T)


# Make an OP control object. File='OP.dat'
#setwd(data.path)
source(file=file.path(prog.path,'HCR_OP_batch_common.R'))

res<-make.OP.dat(my.area=my.area,my.last.year=my.last.year,first.year.output=first.year.output,
                 do.indicators=do.indicators,
                 stochastic.recruitment=stochastic.recruitment)
OP<-res[["OP"]]
SMS<-res[["SMS"]]

sp.name<-SMS@species.names

OP@recruit.adjust.CV[1,]<-recruit.adjust.CV


OP@F.or.C[]<-31
if (batch) OP@output<-14 else  OP@output<-15

write.FLOP.control(OP,file='OP.dat',nice=T)



### OP_trigger file
nsp<-SMS@no.species
n.other.pred<-sum(SMS@species.info[,'predator']==2)
n.pred<-n.other.pred+sum(SMS@species.info[,'predator']==1)
n.vpa<-nsp-n.other.pred
n.vpa.pred<-sum(SMS@species.info[,'predator']==1)

OPT<-read.FLOPtrigger.control(file="OP_trigger.dat",n.VPA=n.vpa,n.other.pred=n.other.pred) 
OPT@last.year<-my.last.year 
OPT@first.year<-first.year.output               
OPT@HCR[]<-1 
OPT@first.run.no<-1
OPT@first.iter.no<-1
OPT@no.iter<-1

#OPT@trigger['T1',]<-Blim* FBlim.adjust
#OPT@trigger['T2',]<-Bpa 
OPT@at.age.weighting<-0 
  
write.FLOPtrigger.control(OPT,file="OP_trigger.dat") 


system(paste(file.path(data.path,"op.exe"), " -nox -maxfn 0 -nohess"),show.output.on.console =T)

if (batch) {
  spNames<-sp.names[first.VPA:nsp]
  condensed<-read.table('OP_average_val.out',header=F)
  allnames<-c(c('value','yield', 'CWsum', 'Fcomb','Fbar', 'SSB', 'TSB', 'recruit','belowBlim','belowBpa', 'Species.n', 'run','iteration'),spNames)
  dimnames(condensed)[[2]]<-allnames
  
  a<-merge(bb,condensed)
  a<-a[order(a$run,a$Species.n),] 
  a$Species<-sp.names[a$Species.n]
  a$value<-a$belowBlim<-a$belowBpa<-a$iteration<-a$Fcomb<-a$TSB<-NULL
  head(a,20)
  
  print(xyplot(yield~Fbar| Species,data=a,scales = "free"))
  print(xyplot(SSB~Fbar| Species,data=a,scales = "free"))
  
  aa<-rbind(tapply(a$SSB,list(paste(a$Species.n,a$Species)),min),
            tapply(a$SSB,list(paste(a$Species.n,a$Species)),max),
            tapply(a$SSB,list(paste(a$Species.n,a$Species)),mean))
  rownames(aa)<-c('min','max','mean')
  round(t(aa))
  
  a<-subset(a,select=c(run,COD,	WHG,	HAD,	POK,	HER,	NSA,	SSA,	NOP,	SPR,	PLE,	SOL,	sp.change,	change,	yield,	CWsum,	Fbar,	SSB,	recruit,	Species.n,		Species))
  write.csv(a,file=file.path(data.path,'Amoebae.csv'),row.names=F)
}