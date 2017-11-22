
######### end user options #########
do.HCR.batch<-function(do.simulation=T,read.condense=T,do.plots=T,read.detailed=T,cutof.year.detailed=2006,NewPlot=T,stochastich=T,YieldPerRecruit=T) {


HCR<-new("FLSMS.predict.control")
HCR@years.wsea[1,1]<-min(refYear)
HCR@years.wsea[2,1]<-max(refYear)
HCR@years.weca<-HCR@years.wsea
HCR@years.propmat[1,1]<-min(refYearPropmat)
HCR@years.propmat[2,1]<-max(refYearPropmat)

HCR@HCR.F.TAC[1]<-0
HCR@rec.noise.input[1]<-0
HCR@inter.year[1]<-1
HCR@rec.noise[1,1]<- recruit.noise.low
HCR@rec.noise[2,1]<- recruit.noise.high

HCR@last.prediction.year<-2026

control<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))

#recruitment geometric mean
s<-Read.summary.data()
tmp<-subset(s,Year < (control@last.year.model-1) & Quarter==control@rec.season & Age==control@first.age,select=c(Year,N))
GM.mean.log<-mean(log(tmp$N))
GM.mean<-exp(GM.mean.log)
GM.sd.log<-sd(log(tmp$N))


if (stochastich) {
   HCR@no.MCMC.iterations<-no.MCMC.iterations
   HCR@read.rec.SSB.parm<-0
   include.probability<-T
   if (YieldPerRecruit) {
     HCR@read.rec.SSB.parm<-1
     pp<-file.path(data.path,'SSB_R.in')
     cat(paste("#model alfa  beta std \n",
                 "3 ",GM.mean.log, 0, GM.sd.log,"\n",sep=' '),file=pp)
     include.probability<-F
    }

} else {
    HCR@no.MCMC.iterations<-1
    HCR@read.rec.SSB.parm<-1
    if (YieldPerRecruit) {
     pp<-file.path(data.path,'SSB_R.in')
     cat(paste("#model alfa  beta std \n",
                 "3 ",GM.mean.log, 0, GM.sd.log,"\n",sep=' '),file=pp)
     include.probability<-F
    } else {
      p<-Read.SSB.Rec.data()
      pp<-file.path(data.path,'SSB_R.in')
      cat("#model alfa  beta std \n",file=pp)
      p<-subset(p,select=c(model,alfa,beta,std))
      p$std<-1E-4
      write.table(p,file=pp,append=T,row.names=F,col.names=F)
      include.probability<-F
    }
}

# make sms.psv file
source(file.path(prog.path,"make_psv.R"))

SMS.option<-" "

first.year.in.mean<-HCR@last.prediction.year-3    # years where output is independent of initial conditions and used as "equilibrium"
last.year.in.mean<-HCR@last.prediction.year-1


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
  sp.name<-control@species.names

  percentiles<-c(0.025,0.05,0.10,0.25,0.50,0.75,0.95,0.975)

  # headers in output files
  heading<-"targetF"
  cat(paste(heading,"Species.n "),file=ssb.out)
  cat(paste("SSB",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=ssb.out,append=T); cat('\n',file=ssb.out,append=T)
  cat(paste(heading,"Species.n "),file=yield.out)
  cat(paste("y",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=yield.out,append=T); cat('\n',file=yield.out,append=T)
  cat(paste(heading,"Species.n "),file=F.out)
  cat(paste("F",formatC(percentiles*1000,width=3,flag='0'),sep=''),file=F.out,append=T); cat('\n',file=F.out,append=T)
  cat(paste(heading,"Species.n "," p.T1 p.T2 \n"),file=prob.out)

  iter<-0
  i<-0
  for (targetF in (targetFs)) i<-1+i
  #print(paste("You have asked for",i,"runs. Do you want to continue? (Y/N):"))
  #a<-readLines(n=1)
  #if(a=='N') stop("Script stopped'")
  #print('OK')
  tot.rep<-i

  for (targetF in (targetFs)) {
      iter<-iter+1
      print(paste("targetF:",targetF))
      print(paste("run no.:",iter, "out of a total of",tot.rep,"runs"))

      HCR@constant.F[1]<-targetF

      write.FLSMS.predict.control(HCR,SMS=control,file='HCR_options.dat')

      #run SMS
      shell(paste( file.path(data.path,"sms.exe"),"-mceval",SMS.option,sep=" "), invisible = TRUE)

      condense_file<-function(filename){
        file<-file.path(data.path,filename)
        a<-read.table(file,header=TRUE)
        a<-subset(a,Year>cutof.year.detailed)
        a<-data.frame(a,targetF=targetF)

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
         cat(paste(targetF,i,' '),file=ssb.out,append=TRUE)
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
        cat(paste(targetF,sp,p.T1,p.T2,'\n'),file=prob.out,append=TRUE)
      })

      a<-Read.MCMC.F.yield.data(dir=data.path)
      a<-subset(a,Year>=first.year.in.mean & Year<=last.year.in.mean ,drop=T)
      b<-tapply(a$Yield,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(targetF,i,' '),file=yield.out,append=TRUE)
         cat(b[[i]],file=yield.out,append=TRUE)
         cat('\n',file=yield.out,append=TRUE)
      }

      b<-tapply(a$mean.F,list(a$Species.n), function(x) quantile(x,probs = percentiles))
      for (i in (1:length(b))) {
         cat(paste(targetF,i,' '),file=F.out,append=TRUE)
         cat(b[[i]],file=F.out,append=TRUE)
         cat('\n',file=F.out,append=TRUE)
      }
  }
}   # end do.simulations

if (read.condense) {

  # read data and options into FLR objects
  HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names

  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  proba<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)

  a<-merge(ssb,yield)
  a<-merge(a,proba)
  a<-merge(a,fi)
  condensed<-data.frame(a,targetF.fac=as.factor(a$targetF))
  write.csv(condensed,file=file.path(data.path,"condensed_MSY.csv"))

 }


if (read.detailed) {

  # read data and options into FLR objects
  control<-read.FLSMS.control()
  #HCR<-read.FLSMS.predict.control(control=control,file='HCR_options.dat')

  sp.name<-control@species.names
  Years<- c(2010,2012,2014,2030)
  ssb<-read.table(ssb.out.all,header=TRUE)
  ssb<-subset(ssb,Year %in% Years)

  cat("Year targetF   p.T2 p.T1 Species.n\n",file=prob.out.all)
  dummy<-by(ssb,list(ssb$Species.n),function(x) {
        q<-tapply(x$SSB,list(x$Repetion,x$Iteration,x$Year,x$targetF),sum)
        sp<-x[1,"Species.n"]
        q[q>T2[sp]]<-0
        q[q>0]<-1
        p.T2<-apply(q,c(3,4),sum)/ (dim(q)[1]*dim(q)[2])

        q<-tapply(x$SSB,list(x$Repetion,x$Iteration,x$Year,x$targetF),sum)
        q[q>T1[sp]]<-0
        q[q>0]<-1
        p.T1<-apply(q,c(3,4),sum)/ (dim(q)[1]*dim(q)[2])

        a<-arr2dfny(p.T2,name="p.T2")
        b<-arr2dfny(p.T1,name="p.T1")
        a<-data.frame(merge(a,b),Species.n=sp)
        write.table(a,row.names=F,col.names=F,quote=F,file=prob.out.all,append=T)
   })


  #proba<-read.table(prob.out.all,header=TRUE)

  yield<-read.table(yield.out.all,header=TRUE)
  yield<-subset(yield,Year %in% Years)

  fi<-read.table(F.out.all,header=TRUE)
  fi<-subset(fi,Year %in% Years)

  a<-merge(ssb,yield)
  a<-merge(a,fi)
  detailed<-data.frame(a,Year.fac=as.factor(a$Year),targetF.fac=as.factor(a$targetF))

 }


if (do.plots) {
   # read data and options into FLR objects
  control<-read.FLSMS.control()

  sp.name<-control@species.names
  ssb<-read.table(ssb.out,header=TRUE)
  yield<-read.table(yield.out,header=TRUE)
  prob<-read.table(prob.out,header=TRUE)
  fi<-read.table(F.out,header=TRUE)

  a<-merge(ssb,yield)
  a<-merge(a,prob)
  a<-merge(a,fi)

  if (paper) dev<-"wmf" else dev<-"screen"
  if (NewPlot) newplot(dev,nox=nox,noy=noy,filename=paste("HCR_",sp.name,sep=''),Portrait=T);

  par(mar=c(4,4,3,5)+.1)  # c(bottom, left, top, right)
  s<-a$SSB500/1000
  y<-a$y500/1000
  x<-a$targetF
  x.lab<-'F(1-2)'

  plot(x,s,ylab='SSB & Yield (1000 t)',xlab=x.lab ,ylim=c(min(s,y,0),max(s,y)),lty=1,type='l',lwd=2,col=1,main=NULL)
  if (include.probability) {
    legend(legendPlace,  c('SSB','Yield', paste('p(SSB<',round(T1),'t)')), pch="  1",lty=c(1,2,3),col=c(1,2,4), lwd=rep(2,3))
  } else  legend(legendPlace,c('SSB','Yield'),pch="   ",lty=c(1,2),lwd=rep(2,2),col=c(1,2))

  lines(x,y,lty=2,lwd=2,col=2)

  if (include.probability) {
    par(new=T)
    plot(x,a$p.T1,axes=F,xlab=x.lab, ylab=' ',lty=3,lwd=2,ylim=c(0,1),type='b',pch="1",col=4)
    abline(h=0.05)
    axis(side=4)
    mtext(side=4,line=3.0,"Probability")
    par(xaxs="r")
  }

  #if (paper) cleanup()

} #end do.plots
}
