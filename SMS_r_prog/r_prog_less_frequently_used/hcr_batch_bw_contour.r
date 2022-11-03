# user options
nox<-2; noy<-3;
paper<-T   # if paper==T output on file, else screen
cleanup()

SMS.option<-" "

first.year.in.mean<-2020    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-2029

do.simulation<-T     # do the simulations and create data files for plots

read.condense<-T
do.plot.condense<-T

read.detailed<-T
cutof.year.detailed<-2008
do.plot.detailed<-T         

include.probability<-T


######### end user options #########



#do.HCR.batch<-function() {

#Read refence points from file reference_points.in
ref<-Read.reference.points()
blim<-ref[,"Blim"]
bpa<-ref[,"Bpa"]
T1<-blim
T2<-bpa

#  files for output
ssb.out<-'HCR_SSB.dat'
yield.out<-'HCR_yield.dat'
F.out<-'HCR_F.dat'
prob.out<-'HCR_prob.dat'

ssb.out.all<-'mcout_SSB.out.all'
yield.out.all<-'mcout_yield.out.all'
F.out.all<-'mcout_mean_F.out.all'
prob.out.all<-'HCR_prob.dat.all'

if (do.simulation) {

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat.gem')

  sp.name<-control@species.names
 
  percentiles<-c(0.025,0.05,0.10,0.25,0.50,0.75,0.95,0.975)

  # headers in output files
  heading<-"targetF Freduction TACconstraint Trigger2"
  cat(paste(heading,"Species.n "),file=ssb.out)
  cat(paste("SSB",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=ssb.out,append=T); cat('\n',file=ssb.out,append=T) 
  cat(paste(heading,"Species.n "),file=yield.out)
  cat(paste("y",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=yield.out,append=T); cat('\n',file=yield.out,append=T) 
  cat(paste(heading,"Species.n "),file=F.out)
  cat(paste("F",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=F.out,append=T); cat('\n',file=F.out,append=T)    
  cat(paste(heading,"Species.n "," p.T1 p.T2 \n"),file=prob.out)


  iter<-0
  
  HCR@no.MCMC.iterations<-1   #no of iterations
  HCR@last.prediction.year<-2030 
  
  targetFs<-seq(0.18,0.35,0.02)
  Freductions<-c(20,30, 40, 50)
  TAC2<-       c(800, 750, 700,600)*1000
  
  #Freductions<-50
  #TACconstraints<-seq(15,45,10)
  TACconstraints<-0
  Trigger2s<-seq(2500000,3000000,250000)
  
   i<-0
   for (targetF in (targetFs)) for (Freduction in (Freductions)) for (TACconstraint in (TACconstraints)) for (Trigger2 in (Trigger2s)) i<-1+i
   print(paste("You have asked for",i,"runs. Do you want to continue? (Y/N):")) 
   a<-readLines(n=1)
   if(a=='N') stop("Script stopped'")
   print('OK')
   tot.rep<-i
    
  for (targetF in (targetFs)) for (Freduction in (Freductions)) for (TACconstraint in (TACconstraints)) for (Trigger2 in (Trigger2s)){
      iter<-iter+1
      print(paste("targetF:",targetF," Freduction:",Freduction," TACconstraint:",TACconstraint," Trigger2;",Trigger2))
      print(paste("run no.:",iter, "out of a total of",tot.rep,"runs")) 
      
      HCR@constant.F[1]<-targetF
      
     HCR@intermidiate.TAC['second',1]<-TAC2[grep(Freduction,Freductions) ]
     
      HCR@real.time["dist",1]<-Freduction
      
      if (TACconstraint>0 ) {
         HCR@TAC.constraints["min",1]<-1-TACconstraint/100 
         HCR@TAC.constraints["max",1]<-1+TACconstraint/100 
       } else {
         HCR@TAC.constraints["min",1]<-0
         HCR@TAC.constraints["max",1]<-0 
       }
       
      HCR@Trigger["T2",1]<-Trigger2              
      HCR@Trigger.a.b["FT12b",1]<-(targetF-HCR@Trigger.a.b["FT1b",1])/ (HCR@Trigger["T2",1]-HCR@Trigger["T1",1])
      HCR@Trigger.a.b["FT2a",1]<-targetF
      HCR@Trigger.a.b["FT2b",1]<-0.0
    
      write.FLSMS.predict.control(HCR,file='HCR_options.dat')

      #run SMS
      shell(paste( file.path(root,"program","sms.exe"),"-mceval",SMS.option,sep=" "), invisible = TRUE)

      condense_file<-function(filename){
        file<-file.path(data.path,filename)
        a<-read.table(file,header=TRUE)
        a<-subset(a,Year>cutof.year.detailed)
        a<-data.frame(a,targetF=targetF, Freduction=Freduction, TACconstraint=TACconstraint, Trigger2=Trigger2)
      
        file<-paste(file,".all",sep='')
        if (iter==1) write.table(a, file =file, row.names = F,col.names = T,quote = F) else write.table(a, file =file, row.names =F,col.names =F, quote =F,append=T)
      }
      
      condense_file("mcout_SSB.out")
      condense_file("mcout_recruit.out")
      condense_file("mcout_mean_F.out")
      condense_file("mcout_yield.out")

      a<-Read.MCMC.SSB.rec.data()
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)

      b<-tapply(a$SSB,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(targetF,Freduction, TACconstraint, Trigger2 ,i,' '),file=ssb.out,append=TRUE)
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
        cat(paste(targetF,Freduction, TACconstraint, Trigger2,sp,p.T1,p.T2,'\n'),file=prob.out,append=TRUE)
      })

      a<-Read.MCMC.F.yield.data(dir=data.path)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      b<-tapply(a$Yield,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(targetF,Freduction, TACconstraint, Trigger2,i,' '),file=yield.out,append=TRUE)
         cat(b[[i]],file=yield.out,append=TRUE)
         cat('\n',file=yield.out,append=TRUE)
      }

      b<-tapply(a$mean.F,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(targetF,Freduction, TACconstraint, Trigger2,i,' '),file=F.out,append=TRUE)
         cat(b[[i]],file=F.out,append=TRUE)
         cat('\n',file=F.out,append=TRUE)
      } 
  }
}   # end do.simulations



if (read.condense) {

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names

  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  proba<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)
    
  a<-merge(ssb,yield)
  a<-merge(a,proba)
  a<-merge(a,fi)
  condensed<-data.frame(a,targetF.fac=as.factor(a$targetF), Freduction.fac=as.factor(a$Freduction),
    TACconstraint.fac=as.factor(a$TACconstraint), Trigger2.fac=as.factor(a$Trigger2/1000))
 }
 
 
if (read.detailed) {

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names
  Years<- c(2009,2010,2011,2012,2013,2014,2015, 2018, 2020, 2027,2029)
  ssb<-read.table(ssb.out.all,header=TRUE)
  ssb<-subset(ssb,Year %in% Years)
  
  cat("Year targetF Freduction TACconstraint Trigger2 p.T2 p.T1 Species.n\n",file=prob.out.all)
  dummy<-by(ssb,list(ssb$Species.n),function(x) {
        q<-tapply(x$SSB,list(x$Repetion,x$Iteration,x$Year,x$targetF,x$Freduction,x$TACconstraint,x$Trigger2),sum)
        sp<-x[1,"Species.n"]
        q[q>T2[sp]]<-0
        q[q>0]<-1
        p.T2<-apply(q,c(3,4,5,6,7),sum)/ (dim(q)[1]*dim(q)[2])
 
        q<-tapply(x$SSB,list(x$Repetion,x$Iteration,x$Year,x$targetF,x$Freduction,x$TACconstraint,x$Trigger2),sum)
        q[q>T1[sp]]<-0
        q[q>0]<-1
        p.T1<-apply(q,c(3,4,5,6,7),sum)/ (dim(q)[1]*dim(q)[2])

        a<-arr2dfny(p.T2,name="p.T2")
        b<-arr2dfny(p.T1,name="p.T1")
        a<-data.frame(merge(a,b),Species.n=sp)
        names(a)[1:5]<-list("Year", "targetF","Freduction", "TACconstraint", "Trigger2")
        
        write.table(a,row.names=F,col.names=F,quote=F,file=prob.out.all,append=T)
   })
      
  #proba<-read.table(prob.out.all,header=TRUE)
  
  yield<-read.table(yield.out.all,header=TRUE)
  yield<-subset(yield,Year %in% Years)
 
  fi<-read.table(F.out.all,header=TRUE)
  fi<-subset(fi,Year %in% Years)
   
  a<-merge(ssb,yield)
  a<-merge(a,fi)
  detailed<-data.frame(a,Year.fac=as.factor(a$Year),targetF.fac=as.factor(a$targetF), Freduction.fac=as.factor(a$Freduction),TACconstraint.fac=as.factor(a$TACconstraint), Trigger2.fac=as.factor(a$Trigger2/1000))
  print(summary(detailed))
 }
 

 #### plot function
 my.dev<-function(filen) { 
 	  win.metafile(filename = filen, width=14,height=14,restoreConsole = TRUE, pointsize=12)
  }

if (do.plot.detailed) {

  PlotYears<- c(2009,2010,2011,2012, 2020, 2027)
 #PlotYears<- c(2010,2012,2015, 2020, 2025,2029)
 ### Yield
 res<-subset(detailed,(Year %in% PlotYears))

 a<-subset(res,Year>2008 ,select=c(yield,targetF,Freduction,Year,Trigger2,TACconstraint))
 # tapply(a$yield,list(a$targetF,a$Freduction,a$Year,a$Trigger2,a$TACconstraint),mean) 
 a<-aggregate(a,list(a$targetF,a$Freduction,a$Year,a$Trigger2,a$TACconstraint),mean)
 max(a$yield)/1000
 
 resTAC<-data.frame(a,Year.fac=as.factor(a$Year),targetF.fac=as.factor(a$targetF), 
    Freduction.fac=as.factor(a$Freduction),TACconstraint.fac=as.factor(a$TACconstraint), Trigger2.fac=as.factor(a$Trigger2/1000))

my.fontsize<-22
my.dev("TAC_SMS2.wmf") 
    
    print(contourplot(yield/1000 ~ targetF * Freduction | Year.fac*Trigger2.fac,  data = resTAC, 
             cuts = 10, region = TRUE,
             at=seq(0,800,100),
            # at=seq(200,500,50),
 
              xlab = "F target",  par.settings=list(fontsize=list(text=my.fontsize)),
              ylab = "% F reduction", col.regions=rev(heat.colors(1000)), pretty=T,
              main = paste(my.stock.dir,": TAC"))             
  ) 
  if (paper) cleanup()
  
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

#}
#do.HCR.batch()


