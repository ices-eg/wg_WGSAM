# user options

last.assessment.year<-2009
last.year.in.forecast<-2014
years.in.refernce.F<-1  #take the last X years mean F and used them for reference mean F

TACfirst<-548  # TAC for the intermidiate year, used for output only
 
# percentiles, put =c() for none
percentiles<-c(0.025,0.25,0.50,0.75,0.975)
percentiles<-c()

incl.prob.below.SSB.refernce.point<-F

if (T) {  #constant F
  HCR.rule<-1
  #Fsq<-0.453
  Fsq<-1
  Fpa<-0.32
  F2009<-0.399
  HCR.options.file<-'HCR_options.dat'  #option file for prediction
#  ICES.F.levels<-c(0.0001,0.10,0.25,0.50,0.75,0.90,1.00,1.10,1.25,Fsq/Fpa)
#  ICES.basis<-paste('Fpa*',round(ICES.F.levels,2),sep='')
#  iterations<-ICES.F.levels*Fpa    # levels times Fpa

  ICES.F.levels<-c(0.000, F2009*0.1 , F2009*0.25 , F2009*0.50 ,0.05             ,0.0636528       ,0.156237354,         0.18         ,F2009*0.75,F2009        ,0.32 )   # please note, you cannot have the same value twice
  ICES.basis<-   c("F=0","F2009*0.1","F2009*0.25","F2009*0.50","Management plan","MSY Framework","ICES MSY transition","FMSY","F2009*0.75","Fsq=F2009","F=Fpa")
  cbind(ICES.basis, ICES.F.levels)
  iterations<-ICES.F.levels    # levels times Fpa
  
  #iterations<-c(0.0,0.32,0.51,1.0)  # F values
  
  ICES.basis<-ICES.basis[order(ICES.F.levels)]
}
if (F) {  #constant TAC
  HCR.rule<-2
  Fsq<-0.453
  Fpa<-0.32
  HCR.options.file<-'HCR_options_constantTAC.dat'  #option file for prediction
  ICES.F.levels<-c(1.6E6,1.747E6)
  ICES.basis<-paste(round(ICES.F.levels,3),sep='')
  iterations<-ICES.F.levels
}

Landings.fac<-1E3  # factor applied in output tables for nice output
SSB.fac<-1E3
TSB.fac<-1E3

short.out<-'short-term.csv'               #output file
short.out.ICES<-'short-term-ICES.csv'     #output file, speciial ICES format


######### end user options #########
do.HCR.short.term<-function() {

scenario<-"Short-term"
outdir<-file.path(data.path,scenario)

setwd(data.path)
bio.interact<-FALSE

#  runs are made in a separate dirictory
scenario.dir<-scenario

dir.create(scenario.dir,showWarnings = FALSE)

SMS.files.single<-c("sms.exe","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in",
                    "fleet_names.in","fleet_info.dat","just_one.in","sms.psv","species_names.in",
                    "SSB_R.in","Prediction_F.in","reference_points.in","predict_stock_N.in",
                    "Exploitation_pattern.in","zero_catch_season_ages.in","zero_catch_year_season.in","proportion_M_and_F_before_spawning.in")

for (from.file in SMS.files.single) {
  to.file<-file.path(data.path,scenario.dir,from.file)
  file.copy(from.file, to.file, overwrite = TRUE)
}

# read data and options into FLR objects
control<-read.FLSMS.control()
HCR<-read.FLSMS.predict.control(control=control,file=HCR.options.file)

sp.name<-control@species.names


setwd(outdir)
write.FLSMS.control(control,file='SMS.dat')

iter<-0

for (option in (iterations)) {
    iter<-iter+1

    print(paste("Iteration:",iter))
    
    HCR@no.MCMC.iterations <-1
    
    if (HCR.rule==1) {
      HCR@HCR[1]<-1    # constant F
      HCR@constant.F[1,1]<-option
    }
    if (HCR.rule==2) {
      HCR@HCR[1]<-2    # constant TAC
      HCR@constant.TAC[1,1]<-option
    }
   
    write.FLSMS.predict.control(HCR,control,file='HCR_options.dat')

    #run SMS
    shell(paste( file.path(data.path,"sms.exe")," -mceval",sep=" "), invisible = TRUE)

    a<-Read.MCMC.SSB.rec.data(dir=outdir)
    a<-subset(a,Year>=last.assessment.year & Year<=last.year.in.forecast ,drop=T)
    if (iter==1) b1<-data.frame(option=option,a)
    else b1<-rbind(b1,data.frame(option=option,a))

    a<-Read.MCMC.F.yield.data(dir=outdir)
    a<-subset(a,Year>=last.assessment.year & Year<=last.year.in.forecast ,drop=T)
    if (iter==1) b2<-data.frame(option=option,a)
    else b2<-rbind(b2,data.frame(option=option,a))
  }

  setwd(data.path)

  # prepare to get SSB next year in the same row
  b.plus<-data.frame(option=b1$option,Species=b1$Species,Species.n=b1$Species.n, Year=b1$Year-1, Repetion=b1$Repetion,
                       Iteration=b1$Iteration,SSB.plus=b1$SSB,TSB.plus=b1$TSB,recruit.plus=b1$recruit)

  b<-merge(b1,b2)
  b<-merge(b,b.plus)
  b<-subset(b,select=-mean_F_percieved)


  tmp<-Read.MCMC.mean.data(dir=outdir)
  tmp<-subset(tmp,Year>=last.assessment.year-years.in.refernce.F+1 & Year<=last.assessment.year)
  ref.F<-tapply(tmp$mean.F,list(tmp$Species), mean)
  list(ref.F=ref.F,result=b)
}
short.results<-do.HCR.short.term()


#Read refence points from file reference_points.in
ref<-Read.reference.points()
blim<-ref[1,"Blim"]
bpa<-ref[1,"Bpa"]
T1<-blim
T2<-bpa


print.result<-function(act.year=last.assessmet.year+1) {
  comma<-','
  if (act.year==last.assessment.year+1) {
    cat(act.year,'\n',file=short.out)
    cat("Biomass,SSB,FMult,FBar,Landings\n",file=short.out,append=T)
    cat(act.year,'\n',file=short.out.ICES)
    cat("Biomass,SSB,FMult,FBar,Landings\n",file=short.out.ICES,append=T)

  }
  
  tmp<-subset(short.results[['result']],Year==act.year)
  ref.F<-short.results[['ref.F']]
  
  if (!is.null(percentiles)) {
    p.TSB<-tapply(tmp$TSB/SSB.fac,list(tmp$option), function(x) quantile(x,probs = percentiles))
    p.SSB<-tapply(tmp$SSB/SSB.fac,list(tmp$option), function(x) quantile(x,probs = percentiles))
    p.Yield<-tapply(tmp$Yield/Landings.fac,list(tmp$option), function(x) quantile(x,probs = percentiles))
    p.mean.F<-tapply(tmp$mean.F,list(tmp$option), function(x) quantile(x,probs = percentiles))
    p.TSB.plus<-tapply(tmp$TSB.plus/SSB.fac,list(tmp$option), function(x) quantile(x,probs = percentiles))
    p.SSB.plus<-tapply(tmp$SSB.plus/SSB.fac,list(tmp$option), function(x) quantile(x,probs = percentiles))
  }

  TSB<-tapply(tmp$TSB,list(tmp$option), mean)/TSB.fac
  SSB<-tapply(tmp$SSB,list(tmp$option), mean)/SSB.fac
  Yield<-tapply(tmp$Yield,list(tmp$option), mean)/Landings.fac
  mean.F<-tapply(tmp$mean.F,list(tmp$option), mean)
  SSB.plus<-tapply(tmp$SSB.plus,list(tmp$option), mean)/SSB.fac
  TSB.plus<-tapply(tmp$TSB.plus,list(tmp$option), mean)/TSB.fac

  #probability to go below Bpa and Blim
  q<-tapply(tmp$SSB.plus,list(tmp$option,tmp$Repetion,tmp$Iteration),sum)
  q[q>T1]<-0
  q[q>0]<-1
  p.Blim<-apply(q,1,sum)/(dim(q)[2]*dim(q)[3])

  q<-tapply(tmp$SSB.plus,list(tmp$option,tmp$Repetion,tmp$Iteration),sum)
  q[q>T2]<-0
  q[q>0]<-1
  p.Bpa<-apply(q,1,sum)/(dim(q)[2]*dim(q)[3])

  if (act.year==last.assessment.year+1) {
   cat(TSB[1],comma,SSB[1],comma,mean.F[1]/ref.F,comma,mean.F[1],comma,Yield[1],'\n',file=short.out,append=T)
   cat(TSB[1],comma,SSB[1],comma,mean.F[1]/ref.F,comma,mean.F[1],comma,Yield[1],'\n',file=short.out.ICES,append=T)


  }
  else {
    cat('\n',act.year,",,,,,,",act.year+1,"\n",file=short.out,append=T)
    cat("\n",file=short.out.ICES,append=T)
    cat("Biomass,SSB,FMult,FBar,Landings,,Biomass,SSB",file=short.out,append=T)
    cat("Rationale,Catch(",act.year,"),Basis,F(",act.year,"),SSB(",act.year,"),SSB(",act.year+1,"), %SSB change, %TAC change",sep='',file=short.out.ICES,append=T)

    if (incl.prob.below.SSB.refernce.point) cat(",P(SSB<Bpa),P(SSB<Blim)\n",file=short.out,append=T)
    else cat("\n",file=short.out,append=T)

    if (incl.prob.below.SSB.refernce.point) cat(",P(SSB<Bpa),P(SSB<Blim)\n",file=short.out.ICES,append=T)
    else cat("\n",file=short.out.ICES,append=T)

    for (i in (1:length(SSB))) {
     if ((i==1) | (act.year>last.assessment.year+2)) cat(TSB[i],comma,SSB[i],comma,file=short.out,append=T)
     else cat(".,.,",file=short.out,append=T)
     cat(mean.F[i]/ref.F,comma,mean.F[i],comma,Yield[i],comma,comma,TSB.plus[i],comma,SSB.plus[i],file=short.out,append=T)
     if (incl.prob.below.SSB.refernce.point) cat(comma,p.Bpa[i],comma,p.Blim[i],"\n",file=short.out,append=T)
     else cat("\n",file=short.out,append=T)
    }

    for (i in (1:length(SSB))) {
     cat(comma,Yield[i],comma,ICES.basis[i],comma,mean.F[i],comma,SSB[i],comma,SSB.plus[i],comma,round((SSB.plus[i]-SSB[i])/SSB[i]*100),comma,file=short.out.ICES,append=T)
     if (act.year==last.assessment.year+2) cat(round((Yield[i]-TACfirst)/TACfirst*100),file=short.out.ICES,append=T)
    if (incl.prob.below.SSB.refernce.point) cat(comma,p.Bpa[i],comma,p.Blim[i],"\n",file=short.out.ICES,append=T)
     else cat("\n",file=short.out.ICES,append=T)

    }
   }
}

for (y in seq(1,4)) print.result(last.assessment.year+y)


