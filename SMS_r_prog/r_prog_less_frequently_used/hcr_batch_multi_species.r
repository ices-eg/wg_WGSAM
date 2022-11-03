# this script makes multispecies projections given
#    1) a range of F targets by species or
#    2) fixed trigger points (T1 and T2) and a range of F targets by species

################
scenario<-"T1-T2"        # HCR 10

stochastic.recruitment<-T
simulate.assessment<-T              # base TAC on a simulated assessmentsimulate.assessment=T), or assume perfect knowledge (simulate.assessment=F)
use.TAC.constraint<-F
use.TAC.constraint.cod<-F
use.TAC.constraint.cod.Icelandic<-T
use.cap<-F

scen.name<-'run01'; targetFs<-expand.grid(COD=0.3,HER=0.16,SPR=0.35); use.TAC.constraint.cod<-T    # run 1
#scen.name<-'run02'; targetFs<-expand.grid(COD=0.66,HER=0.26,SPR=0.46)    # run 2
#scen.name<-'run03'; targetFs<-expand.grid(COD=0.45,HER=0.26,SPR=0.40); use.TAC.constraint<-T    # run 3
#################################

if (F) {
  scenario<-"constantF"     # HCR 1  no noise on recruitment

  stochastic.recruitment<-F
  simulate.assessment<-F              # base TAC on a simulated assessmentsimulate.assessment=T), or assume perfect knowledge (simulate.assessment=F)
  use.TAC.constraint<-F
  #scen.name<-"scan-all"; targetFs<-expand.grid(COD=seq(0,1.0,0.1),HER=seq(0,0.4,0.05),SPR=seq(0,0.6,0.05))
  scen.name<-"scan-narrow-10"; targetFs<-expand.grid(COD=seq(0.4,0.7,0.05),HER=seq(0.2,0.3,0.02),SPR=seq(0.3,0.5,0.02))
}
##############

# Make HCR control object
no.years.in.average<-20  # for calc of mean west, weca and propmat
SMS.control<-read.FLSMS.control()

########################################
##  N in the beginning of the period or N bar for calculation of M2 (option use.Nbar)
#  0=use N in the beginning of the time step (default)
#  1=use N bar
SMS.control@use.Nbar<-0
SMS.control@M2.iterations<- 4    ## Maximum M2 iterations (option M2.iterations) in case of use.Nbar=1
SMS.control@max.M2.sum2<-1e-6    ## convergence criteria (option max.M2.sum2) in case of use.Nbar=1

SMS.control@VPA.mode<-2   # multispecies mode

sp.name<-SMS.control@species.names

# make HCR_option.dat object
HCR<-FLSMS.predict.control(
    first.prediction.year=SMS.control@last.year.model+1,
    no.species=4,
    no.other.predators=1,
    species.names=c('dummyOTH',sp.names))


HCR@years.wsea["first-year",]<-   SMS.control@last.year.model-no.years.in.average+1
HCR@years.weca["first-year",]<-   SMS.control@last.year.model-no.years.in.average+1
HCR@years.propmat["first-year",]<-SMS.control@last.year.model-no.years.in.average+1

if (stochastic.recruitment) {
   HCR@rec.noise["lower",]<- -2
   HCR@rec.noise["upper",]<-  2
} else {
   HCR@rec.noise["lower",]<-  0
   HCR@rec.noise["upper",]<-  0
}

HCR@obs.noise["lower",]<- -2
HCR@obs.noise["upper",]<-  2

#     COD, HER, SPR
T1<-c( 50, 200, 200)*1000      # SSB that gives F=0 (let us call them Blim)
T2<-c(150, 600, 600)*1000      # SSB that gives F=F target
HCR@Trigger['T1',]<-T1
HCR@Trigger['T2',]<-T2
if (scenario=="constantF") {
  HCR@HCR[1,]<-c(1,1,1)
} else if (scenario=="T1-T2" ){
  HCR@HCR[1,]<-c(10,10,10)
}
HCR@read.rec.SSB.parm<-0             # 0=use estimated SSB-R parameters, 1= read in
if (stochastic.recruitment) HCR@no.MCMC.iterations<-3 else   HCR@no.MCMC.iterations<-1 #no of iterations

HCR@recruit.adjust.CV[1,1]<-1     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
HCR@recruit.adjust.CV[1,2]<-1     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment
HCR@recruit.adjust.CV[1,3]<-1     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment

HCR@inter.year[,]<-1            # No. of intermediate years (1 | 2)
HCR@last.prediction.year<-2050
HCR@age.group.output[2]  <-1   # M2

if (simulate.assessment) {
  HCR@HCR.F.TAC[]<-1              # Use TAC

  if (use.TAC.constraint) {
     HCR@TAC.constraints[,1]<-c(0.85, 1.15) # cod  TAC constraint
     HCR@TAC.constraints[,2]<-c(0.85, 1.15) # herring
     HCR@TAC.constraints[,3]<-c(0.80, 1.20) # sprat
  }

  if (use.TAC.constraint.cod) {
     HCR@TAC.constraints[,1]<-c(0.85, 1.15) # cod  TAC constraint
  }

  if (use.TAC.constraint.cod.Icelandic) {
     HCR@TAC.constraints[,1]<-c(-50, -50) # cod  TAC constraint from the Icelandic model 
  }

  if (use.cap) HCR@F.cap[]<-c(0.8,0.4,0.8)
  
  
  HCR@assessment[,1]<- c(1,1,0.2,1)   # cod  Assessment uncertanties
  HCR@assessment[,2]<- c(1,1,0.2,1)   # herring
  HCR@assessment[,3]<- c(1,1,0.2,1)   # Sprat

  HCR@intermediate.TAC[,1]<-c( 68100, 77000) # cod  TAC two first years
  HCR@intermediate.TAC[,2]<-c(120000, 92000) # herring
  HCR@intermediate.TAC[,3]<-c(310000,242000) # sprat

  HCR@intermediate.F[]<- -1

  HCR@inter.year[,]<-2            # No. of intermediate years
  HCR@inter.F.TAC[]<-1
} else {
  HCR@HCR.F.TAC[]<-0              # Use F
  HCR@inter.year[,]<-1            # No. of intermediate years
}



first.year.in.mean<-HCR@last.prediction.year-11    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-HCR@last.prediction.year-1


dim(targetFs)

source(file.path(prog.path,"make_psv.R"))
########################################################################
# user options
nox<-2; noy<-3;
paper<-T   # if paper==T output on file, else screen
cleanup()

SMS.option<-" "

do.simulation<-T     # do the simulations and create data files for plots
read.condense<-T
do.plot.condense<-F
read.detailed<-T
cutof.year.detailed<-2009
do.plot.detailed<-F

include.probability<-T

bio.interact<-T

######### end user options #########



outdir<-file.path(data.path,scenario)

#  files for output
ssb.out<-'HCR_SSB.dat'
yield.out<-'HCR_yield.dat'
F.out<-'HCR_F.dat'
prob.out<-'HCR_prob.dat'

ssb.out.all<-'mcout_SSB.out.all'
yield.out.all<-'mcout_yield.out.all'
F.out.all<-'mcout_mean_F.out.all'
prob.out.all<-'HCR_prob.dat.all'
recruit.out.all<-'mcout_recruit.out.all'

#  runs are made in a seperate dirictory
if (stochastic.recruitment) scenario<-paste(scenario,'stoc_rec',sep='_')
if (simulate.assessment) scenario<-paste(scenario,'assess',scen.name,sep='_')

scenario.dir<-file.path(data.path,scenario)


if (do.simulation) {


  if (file.exists(scenario.dir)) unlink(scenario.dir,recursive = T)
  dir.create(scenario.dir,showWarnings = FALSE)

  SMS.files.single<-c("area_names.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                      "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                      "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                      "proportion_M_and_F_before_spawning.in","proportion_landed.in",
                      "Exploitation_pattern.in","covariance_N.in","HCR_options.dat","sms.dat",
                      "SMS.exe")

  for (from.file in SMS.files.single) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  SMS.files.multi<-c("run_ms3.dat","alk_stom.in","consum.in","Length_weight_relations.in","lsea.in","N_haul_at_length.in",
                     "natmor1.in","other_food.in","season_overlap.in","stom_pred_length_at_sizecl.in","stom_struc_at_length.in",
                     "stomcon_at_length.in","stomlen_at_length.in","stomweight_at_length.in","pred_prey_size_range_param.in",
                     "incl_stom.in","temperature.in")

  if (bio.interact) for (from.file in SMS.files.multi) {
    to.file<-file.path(scenario.dir,from.file)
    file.copy(from.file, to.file, overwrite = TRUE)
  }

  write.FLSMS.control(SMS.control,file="SMS.dat",path=scenario.dir,write.multi=TRUE,nice=TRUE)

  sp.name<-SMS.control@species.names
 
  percentiles<-c(0.05,0.25,0.50,0.75,0.95)

  setwd(scenario.dir)

  # headers in output files
  heading<-"iter"
  cat(paste(heading,"Species.n "),file=ssb.out)
  cat(paste("SSB",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=ssb.out,append=T); cat('\n',file=ssb.out,append=T) 
  cat(paste(heading,"Species.n "),file=yield.out)
  cat(paste("y",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=yield.out,append=T); cat('\n',file=yield.out,append=T) 
  cat(paste(heading,"Species.n "),file=F.out)
  cat(paste("F",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=F.out,append=T); cat('\n',file=F.out,append=T)    
  cat(paste(heading,"Species.n "," p.T1 p.T2 \n"),file=prob.out)

  tot.run<-dim(targetFs)[[1]]
  fsp<-dim(targetFs)[[2]]  # number of species with variable F target

  print(paste("You have asked for",tot.run," runs. Do you want to continue? (Y/N):"))
  if (tot.run> 10) a<-readLines(n=1) else a<-'Y'
  if(a=='N') stop("Script stopped'")
  print('OK')

  firstTime<-Sys.time()
  iter<-1
  for (iter in (1:tot.run)) {
      cat(paste("run no.:",iter, "out of a total of",tot.run,"runs\n"))
      if (iter>1) cat("Expected landing time:",format(firstTime+(Sys.time()-firstTime)*tot.run/iter, "%a %b %d %X "),"\n")
      HCR@constant.F[1,]<- unlist(c(targetFs[iter,]))
      print(HCR@constant.F[1,])

      HCR@Trigger.a.b["FT1a",]<-0.001
      HCR@Trigger.a.b["FT1b",]<-0.0
      HCR@Trigger.a.b["FT12a",]<-0.0
      HCR@Trigger.a.b["FT12b",]<-HCR@constant.F[1,]/ (HCR@Trigger["T2",]-HCR@Trigger["T1",])
      HCR@Trigger.a.b["FT2a",]<-HCR@constant.F[1,]
      HCR@Trigger.a.b["FT2b",]<-0.0
    
      write.FLSMS.predict.control(HCR,SMS.control,file='HCR_options.dat',path=scenario.dir,nice=T)

      #run SMS
      shell(paste( file.path(scenario.dir,"sms.exe"),"-mceval -ind run_ms3.dat",sep=" "), invisible = TRUE)

      condense_file<-function(filename){
        file<-file.path(scenario.dir,filename)
        a<-read.table(file,header=TRUE)
        a<-subset(a,Year>cutof.year.detailed)
        a<-data.frame(a,iter=iter)
      
        file<-paste(file,".all",sep='')
        if (iter==1) write.table(a, file =file, row.names = F,col.names = T,quote = F) else write.table(a, file =file, row.names =F,col.names =F, quote =F,append=T)
      }
      
      condense_file("mcout_SSB.out")
      condense_file("mcout_recruit.out")
      condense_file("mcout_mean_F.out")
      condense_file("mcout_yield.out")

      a<-Read.MCMC.SSB.rec.data(dir=scenario.dir)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)

      b<-tapply(a$SSB,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(iter ,i,' '),file=ssb.out,append=TRUE)
         cat(b[[i]],file=ssb.out,append=TRUE)
         cat('\n',file=ssb.out,append=TRUE)
      }
     
      dummy<-by(a,list(a$Species.n),function(x) {
        q<-tapply(x$SSB,list(x$Year,x$Repetion,x$Iteration),sum)
        sp<-x[1,"Species.n"]
        q[q>T2[sp]]<-0
        q[q>0]<-1
        p.T2<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])

        q<-tapply(x$SSB,list(x$Year,x$Repetion,x$Iteration),sum)      
        q[q>T1[sp]]<-0
        q[q>0]<-1
        p.T1<-sum(q)/(dim(q)[1]*dim(q)[2]*dim(q)[3])
        cat(paste(iter,sp,p.T1,p.T2,'\n'),file=prob.out,append=TRUE)
      })

      a<-Read.MCMC.F.yield.data(dir=scenario.dir)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      b<-tapply(a$Yield,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(iter,i,' '),file=yield.out,append=TRUE)
         cat(b[[i]],file=yield.out,append=TRUE)
         cat('\n',file=yield.out,append=TRUE)
      }

      b<-tapply(a$mean.F,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(iter,i,' '),file=F.out,append=TRUE)
         cat(b[[i]],file=F.out,append=TRUE)
         cat('\n',file=F.out,append=TRUE)
      } 
  }
}   # end do.simulations


if (read.condense) {

  setwd(scenario.dir)
  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  proba<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)
    
  a<-merge(ssb,yield)
  a<-merge(a,proba)
  a<-merge(a,fi)
  
  b<-data.frame(iter=1:dim(targetFs)[[1]],targetFs)
  condensed<-merge(a,b)
  save(condensed, file =file.path(scenario.dir, "condensed.RData"))

  setwd(data.path)
} else load(file =file.path(scenario.dir, "condensed.RData"))
 
 
if (read.detailed) {

  setwd(scenario.dir)
  Years<- (2010:HCR@last.prediction.year)
  ssb<-read.table(ssb.out.all,header=TRUE)
  ssb<-subset(ssb,Year %in% Years)
  
  cat("Year iter p.T2 p.T1 Species.n\n",file=prob.out.all)
  dummy<-by(ssb,list(ssb$Species.n),function(x) {
        q<-tapply(x$SSB,list(x$Repetion,x$Iteration,x$Year,x$iter),sum)
        sp<-x[1,"Species.n"]
        q[q>T2[sp]]<-0
        q[q>0]<-1
        p.T2<-apply(q,c(3,4),sum)/ (dim(q)[1]*dim(q)[2])
 
        q<-tapply(x$SSB,list(x$Repetion,x$Iteration,x$Year,x$iter),sum)
        q[q>T1[sp]]<-0
        q[q>0]<-1
        p.T1<-apply(q,c(3,4),sum)/ (dim(q)[1]*dim(q)[2])

        a<-arr2dfny(p.T2,name="p.T2")
        b<-arr2dfny(p.T1,name="p.T1")
        a<-data.frame(merge(a,b),Species.n=sp)
        names(a)[1:2]<-list("Year", "iter")
        
        write.table(a,row.names=F,col.names=F,quote=F,file=prob.out.all,append=T)
   })
      
  #proba<-read.table(prob.out.all,header=TRUE)
  
  yield<-read.table(yield.out.all,header=TRUE)
  yield<-subset(yield,Year %in% Years)
 
  fi<-read.table(F.out.all,header=TRUE)
  fi<-subset(fi,Year %in% Years)

  rec<-read.table(recruit.out.all,header=TRUE)
  rec<-subset(rec,Year %in% Years)

  a<-merge(ssb,yield)
  a<-merge(a,rec)
  a<-merge(a,fi)
  b<-data.frame(iter=1:dim(targetFs)[[1]],targetFs)

  detailed<-merge(b,a)
  print(summary(detailed))
  save(detailed, file =file.path(scenario.dir, "detailed.RData"))

  setwd(data.path)

 } else load(file =file.path(scenario.dir, "detailed.RData"))


############################
library(mgcv)

a<-subset(detailed, Year==HCR@last.prediction.year,select=c(-Repetion, -Iteration,-iter))

# a<-subset(detailed, Year==HCR@last.prediction.year & COD>0.2 & HER>=0.3 &HER <=0.35 & SPR>=0.3 & SPR<=0.35 ,select=c(-Repetion, -Iteration,-iter))

cleanup()
plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=8, height=8, pointsize=12)
  if (dev=='wmf') win.metafile(filename = file.path(scenario.dir,paste(out,'.wmf',sep='')), width=8, height=8, pointsize=12)
  if (dev=='png') png(filename =file.path(scenario.dir,paste(out,'.png',sep='')), width = 1200, height = 1200,units = "px", pointsize = 25, bg = "white")
  if (dev=='pdf') pdf(file =file.path(scenario.dir,paste(out,'.pdf',sep='')), width = 8, height = 8,pointsize = 12,onefile=FALSE)
}

my.dev<-'screen'
plotfile(dev=my.dev,out='a_SSB')
 plotfile(dev=my.dev,out='a_yield')

par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
by(a,list(a$Species.n),function(x) {

  x$y<-x$yield/1000
  ylab<-paste(sp.names[x[1,'Species.n']], "Yield (1000 t)")
  #x$y<-x$SSB/1000
  #ylab<-paste(sp.names[x[1,'Species.n']], "SSB (1000 t)")

  plot(x$COD,x$y,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  if (length(unique(a$COD))>=4) lines(smooth.spline(x$COD,x$y),lty=1,lwd=2,col='red')
  plot(x$HER,x$y,xlab='F Herring',ylab=ylab)
  if (length(unique(a$HER))>=4) lines(smooth.spline(x$HER,x$y),lty=1,lwd=2,col='red')
  plot(x$SPR,x$y,xlab='F Sprat',ylab=ylab)
  if (length(unique(a$SPR))>=4) lines(smooth.spline(x$SPR,x$y),lty=1,lwd=2,col='red')
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-yield')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  x$y<-x$yield/1000
  ylab<-paste(sp.names[x[1,'Species.n']], "Yield (1000 t)")
  #x$y<-x$SSB/1000
  #ylab<-paste(sp.names[x[1,'Species.n']], "SSB (1000 t)")

  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-SSB')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  x$y<-x$SSB/1000
  ylab<-paste(sp.names[x[1,'Species.n']], "SSB (1000 t)")

  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-recruit')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  maxRec<-max(x$rec)
  if (maxRec<1E3) {  ylab<-paste(sp.names[x[1,'Species.n']], "Recruit (thousands)"); x$y<-x$rec }
  else if (maxRec<1E6) {  ylab<-paste(sp.names[x[1,'Species.n']], "Recruit (millions)"); x$y<-x$rec/1000 }
  else if (maxRec<1E9) {  ylab<-paste(sp.names[x[1,'Species.n']], "Recruit (billions)"); x$y<-x$rec/1000000 }

  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()



# find optimum
sel.year<-2040:HCR@last.prediction.year
a<-subset(detailed, Year %in% (sel.year) & COD>0,select=c(-Repetion, -Iteration,-iter) )
a<-aggregate(yield~Species.n+COD+HER+SPR,mean,data=a)

yield.weighting<-c(10, 1.5, 1.0)
b<-data.frame(a,val=a$yield*yield.weighting[a$Species.n])
b<-aggregate(val~COD+HER+SPR,sum,data=b)
b<-b[order(b$val,decreasing=T),]
head(b,10)

if (F) { # compare with a SMS-OP run
  g<-Read.OP.condensed()
  g<-subset(g,Year >=2040)
  g<-data.frame(g,val=g$yield*yield.weighting[g$Species.n])
  g<-aggregate(val~Species.n,mean,data=g)
  g
  sum(g$val)
}


if (T) {

 #### plot function
 my.dev<-function(filen) { 
 	  win.metafile(filename = filen, width=14,height=14,restoreConsole = TRUE, pointsize=12)
  }

if (do.plot.detailed) {

 PlotYears<- c(2015,2015, 2020, 2030, 2040)
 ### Yield
 a<-subset(detailed,Year %in% PlotYears & Species.n==1,select=c(-iter,-Iteration,-Repetion))
 a<-aggregate(a,list(a$COD,a$HER,a$SPR,a$Year,a$Species.n),mean)

 max(a$yield)/1000
 
my.fontsize<-12
my.dev("TAC_SMS2.wmf")

my.dev<-'screen'
plotfile(dev=my.dev,out='contour_Yield')

    print(contourplot(yield/1000 ~ as.factor(COD) * as.factor(SPR) | paste(as.factor(Year),HER),  data = a,
             cuts = 10, region = TRUE,
             at=seq(0,100,10),
            # at=seq(200,500,50),
 
              xlab = "F cod target",  par.settings=list(fontsize=list(text=my.fontsize)),
              ylab = "% F sprat target", col.regions=rev(heat.colors(1000)), pretty=T,
              main = paste(my.stock.dir,": TAC"))             
  ) 
  #if (paper) cleanup()
  
 ### SSB
 a<-subset(res,Year>2008 ,select=c(SSB,targetF,Freduction,Year,Trigger2,TACconstraint))
 #tapply(a$SSB,list(a$targetF,a$Freduction,a$Year,a$Trigger2,a$TACconstraint),mean)
 min(a$SSB)/1000 
 max(a$SSB)/1000   
 a<-aggregate(a,list(a$targetF,a$Freduction,a$Year,a$Trigger2,a$TACconstraint),mean)
 max(a$SSB)/1000 
 resTAC<-data.frame(a,Year.fac=as.factor(a$Year),targetF.fac=as.factor(a$targetF), 
    Freduction.fac=as.factor(a$Freduction),TACconstraint.fac=as.factor(a$TACconstraint), Trigger2.fac=as.factor(a$Trigger2/1000))
 my.dev("SSB_SMS.wmf")   
 print(contourplot(SSB/1000 ~ targetF * Freduction | Year.fac*Trigger2.fac,  data = resTAC, 
             cuts = 10, region = TRUE,at=seq(1400,2800,100),
              xlab = "F target",  par.settings=list(fontsize=list(text=my.fontsize)),
              ylab = "% F reduction", col.regions=rev(heat.colors(1000)), pretty=T,
              main = paste(my.stock.dir,": SSB"))             
  ) 
 
 if (paper) cleanup()
}  

if (do.plot.condense) {

 a<-read.table(prob.out.all,header=TRUE)
 a<-subset(a,(Year %in% PlotYears))
 a<-data.frame(a,Year.fac=as.factor(a$Year),targetF.fac=as.factor(a$targetF), 
    Freduction.fac=as.factor(a$Freduction),TACconstraint.fac=as.factor(a$TACconstraint), Trigger2.fac=as.factor(a$Trigger2/1000))
 #print(summary(a))
 #a<-subset(a,TACconstraint==45) 
 
  
my.dev("Plim_SMS.wmf") 
 
 #  tapply(a$p.T1,list(a$targetF,a$Freduction,a$Year,a$Trigger2,a$TACconstraint),max)
max(a$p.T1)

 print(contourplot(p.T1 ~ targetF * Freduction | Year.fac*Trigger2.fac, data = a,
              region = TRUE,at=seq(0,0.6,0.05),
              xlab = "F target",  par.settings=list(fontsize=list(text=my.fontsize)),
              ylab = "% F reduction", col.regions=rev(heat.colors(1000)), pretty=T,
              main = paste(my.stock.dir,"Prob(SSB<=Blim)"))
  ) 
  
 if (paper) cleanup()
   
  # user options
 nox<-1; noy<-1;
 if (paper) dev<-"wmf" else dev<-"screen"
 newplot(dev,nox,noy,dir=data.path,filename=paste("Batch2_",sep=''),Portrait=T);
  xlab.title<-'Cod F'

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names
 
  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  prob<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)

  a<-merge(ssb,yield)
  a<-merge(a,prob)
  a<-merge(a,fi)
 
} #end do.plot.condense

}

