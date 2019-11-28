library(FLCore)
library(FLXSA)
library(FLash) # from the MSE directory

data(ple4)

## Check Retro
# MV retro.ple4<-retro(ple4,ple4.indices,FLXSA.control(),retro=10)
# MV xyplot(data~year,groups=qname,data=lapply(retro.ple4,fbar), type="l")
# MV xyplot(data~year,groups=qname,data=lapply(retro.ple4,"ssb"),type="l")

## Stock recruitment relationship
ple4.sr       <-as.FLSR(ple4)
ple4.sr       <-transform(ple4.sr,ssb=ssb/1000,rec=rec/1000)
model(ple4.sr)<-ricker()
ple4.sr       <-mle(ple4.sr)
plot(ple4.sr)
sr.params      <- c(params(ple4.sr))[1:2]*c(1,1/1000)
sr.model      <-"ricker"

## set up objects
nits                   <-100
ple4                   <-propagate(stf(ple4,nyrs=21),iter=nits)
biol                   <-as(ple4,"FLBiol")
fleet                  <-as(ple4,"FLFleet")
fleet@capacity[]       <-0.75

## years
now        <-2001
future.yr  <-now+19
history.yr <-dims(ple4)$minyear
plusgroup  <-ple4@range["plusgroup"]

## HCR options
Fpa          <-.60
Bpa          <-200000
Fmin         <-.10
TAC.bound    <-0.15
GodOnlyKnows <-FALSE

plusgrp      <-ple4@range["plusgroup"]

# Monte Carlo
#sr.deviates   <-FLQuant(sample(c(exp(residuals(ple4.sr))),replace=TRUE,size=nits*20*nits),dimnames=dmns)
sr.deviates       <-FLQuant(exp(rnorm(nits*dims(ple4)$year, mean=0.0, sd=.3)),dimnames=list(age=1,year=now:future.yr,iter=1:nits))/exp(.3^2/2.0)
index.deviates    <-FLQuant(exp(rnorm(dims(biol@m)$age*dims(biol@m)$year*nits, mean=0, sd=.30)), dimnames=list(age=ple4@range["min"]:plusgrp,year=history.yr:future.yr,iter=1:nits))
catch.q(fleet,1,1)<-FLQuant(1*exp(rnorm(length(c(catch.q(fleet,1)[[1]])),0,.2)),dimnames=dimnames(catch.q(fleet,1)[[1]]))


################
# MV Here you have to remove the FLash package and substitute it by another FLAsh

# remove.packages("FLash")




## MP ########################################################################
## set up CPUE index


OEMCPUE<-function(biol,deviates,start,end="missing",plusgroup="missing")
     {
     if (missing(end)) end<-start
     yrs<-start:end

     if (!missing(plusgroup))
        index<-setPlusGroup(trim(biol,year=yrs),plusgroup)@n
     else
        index<-trim(biol,year=yrs)@n

     ## unbiased population estimates
     return(index*deviates[,ac(yrs)])
     }



index                               <-as(trim(biol, age=biol@range["min"]:plusgroup),"FLIndex")
index@range[c("startf","endf")]     <-c(0,0.1)
index@index[,ac(history.yr:(now-1))]<-OEMCPUE(biol,index.deviates,start=history.yr,end=now-1,plusgroup=plusgroup)

## set up stock
stk               <-as(biol,"FLStock")
stk               <-setPlusGroup(stk,plusgrp)
stk@stock.n[]     <-NA
stk@harvest[]     <-NA
units(stk@harvest)<-"f"
 ## Observation Error Model

OEMStock<-function(biol,fleet,start,end="missing",plusgroup="missing")
    {
    if (missing(end)) end<-start
    yrs<-as.character(end:start)

    stk <-as(window(fleet@metiers[[1]]@catches[[1]], start=start, end=end),"FLStock")

    ## biological parameters
    stock.wt(    stk)[,yrs] <-wt(  biol)[dimnames(stk@m)$age,yrs]
    m(           stk)[,yrs] <-m(   biol)[dimnames(stk@m)$age,yrs]
    mat(         stk)[,yrs] <-fec( biol)[dimnames(stk@m)$age,yrs]
    harvest.spwn(stk)[,yrs] <-spwn(biol)[dimnames(stk@m)$age,yrs]
    m.spwn(      stk)[,yrs] <-spwn(biol)[dimnames(stk@m)$age,yrs]

    if (!missing(plusgroup))
       stk<-setPlusGroup(stk,plusgroup)

    return(stk)
    }

## set up stock
yrs.assess<-ac(-30:(now-1))
stk       <-OEMStock(biol,fleet,start=history.yr,end=now-1)
TAC       <-catch(fleet,1,1)[,ac(now)]


  cat(sprintf("\n===========================================\n"))
  for (iyr in now:future.yr)
     {
     stk@range["maxyear"] <-iyr-1
     yrs.assess           <-as.character((history.yr):stk@range["maxyear"])
     cat(sprintf("Year =%.0f\n",iyr))

     ## Observation Error Model
     index@index[,ac(iyr-1)]<-OEMCPUE( biol,index.deviates,iyr-1,plusgroup=plusgroup)
     stk[        ,ac(iyr-1)]<-OEMStock(biol,fleet,         iyr-1,plusgroup=plusgroup)

     cat("=============","Assess!"," =============\n")
     # MV if (TRUE) {
        t.<-system.time(stk<-stk[,yrs.assess]+FLXSA(stk[,yrs.assess],trim(index,year=((history.yr):(iyr-1))),diag.flag=FALSE))
     # MV }
     # MV else {
     # MV    t.<-system.time(stk<-perfectAssess(stk,biol,fleet,yrs.assess))
     # MV }
     
     cat("system.time",t.,"\n")


     ## HCR
     cat("=============","HCR"," =================\n")
     ## years for projection etc.
     yr.now     <-as.character(stk@range["maxyear"]+1)
     yr.TAC     <-as.character(stk@range["maxyear"]+2)
     yr.recruits<-as.character((stk@range["minyear"]+1):(stk@range["maxyear"]+2))

     ## set up future years
     stk   <-stf(window(stk,end=stk@range["maxyear"]),nyrs=2)

     ## F status quo
     Fsq   <-c(fbar(stk)[,yr.now])

     ## target options
     target<-fwdTarget(year    =c(stk@range["maxyear"]-1,rep(stk@range["maxyear"],4)),
                       quantity=c(rep(      "f",2),"catch","ssb","f"),
                       value   =c(mean(Fsq),mean(pmin(Fsq,Fpa)),   NA,    NA, NA),
                       min     =c(NA,NA,mean(TAC*(1.0-TAC.bound)),mean(Bpa),     Fmin),
                       max     =c(NA,NA,mean(TAC*(1.0+TAC.bound)),       NA,mean(Fpa)))

      cat("\n===================","Target"," ====================\n")
      print(target[,1:5])
      cat("================================================\n")

      ## targets by iter
      value    <-array(NA,c(dim(target)[1], dims(stk)$iter))

      min      <-value
      max      <-value

      value[1,]<-c(Fsq)
      value[2,]<-c(pmin(Fsq,Fpa))
      value[2,]<-c(pmin(Fsq,Fpa))

      min[3,]<-c(TAC*(1.0-TAC.bound))
      min[4,]<-c(Bpa)
      min[5,]<-Fmin

      max[3,]<-c(TAC*(1.0-TAC.bound))
      max[5,]<-c(Fpa)

      ## run
      t.<-system.time(stk<-fwd(stk,target[1:2,],value=value[1:2,], min=min[1:2,], max=max[1:2,], sr.model="mean",sr.params=exp(mean(log(stk@stock.n[1,yr.recruits]),na.rm=TRUE))))

      ## get Quota
      TAC<-computeCatch(stk)[,yr.TAC]
      cat("system.time",t.,"\n")

      ## go fish!!
      cat("=============","Go Fish!"," ============\n")
      OMcontrol <-fwdControl(year=iyr+1, fleet=1, metier=1, value=NA, min=.01, max=.75)
      OMtarget  <-fwdTarget( year=iyr+1, value=mean(c(TAC)), quantity="catch", fleet=1, metier=1, spp=1)

      t.<-system.time(res<-fwd(biol, fleet, OMtarget, OMcontrol, target.value=t(matrix(TAC)),sr.model=sr.model, sr.params=sr.params, sr.residuals=sr.deviates[,as.character(iyr+1)]))

      landings.n(fleet,1,1)[,as.character(iyr+1)]<-res$landings.n[,as.character(iyr+1)]
      discards.n(fleet,1,1)[,as.character(iyr+1)]<-res$discards.n[,as.character(iyr+1)]
      effort(fleet)[        ,as.character(iyr+1)]<-res$effort[    ,as.character(iyr+1)]
      n(biol)[              ,as.character(iyr+1)]<-res$n[         ,as.character(iyr+1)]
      cat("system.time",t.,"\n")

      SmryPlots(fleet,biol,stk,stk@range[c("minfbar","maxfbar")],(history.yr):(iyr-1))
      }