
### class ######################################################################



validFLOPtrigger.control <- function(object){
    if (object@first.year<0 | object@last.year<0 | object@first.year > object@last.year) return(paste("value of first.year:",object@first.year, "or last.year",object@last.year, "is wrong"))
    if (object@last.year<object@first.year) return("last.year must be >= first.year")
    # Everything is fine
    return(TRUE)
}

setClass("FLOPtrigger.control",
    representation(
        first.year           ="numeric",
        last.year            ="numeric",
        first.run.no         ="numeric",
        first.iter.no        ="numeric",
        no.iter              ="numeric",
        HCR                  ="matrix",
        trigger               ="matrix",
        Ftarget              ="matrix",
        Fslope               ="matrix",
        SSB50                ="matrix",
        S1                   ="matrix",
        SSBpenalty           ="matrix",
        weighting            ="matrix",
        at.age.weighting     ="numeric"
 )
  ,
  prototype=prototype(
        first.year          =2020,
        last.year           =2030,
        first.run.no        =1,
        first.iter.no        =1,
        no.iter             =1,
        HCR                 =matrix(1,ncol=1,nrow=1,dimnames=list(c("HCR"),c("sp1"))),
        trigger             =matrix(1,ncol=1,nrow=2,dimnames=list(c("T1","T2"),c("sp1"))),
        Ftarget             =matrix(1,ncol=1,nrow=4,dimnames=list(c("lower","higher","init","phase"),c("sp1"))),
        Fslope              =matrix(1,ncol=1,nrow=4,dimnames=list(c("lower","higher","init","phase"),c("sp1"))),
        SSB50               =matrix(1,ncol=1,nrow=4,dimnames=list(c("lower","higher","init","phase"),c("sp1"))),
        S1                  =matrix(1,ncol=1,nrow=4,dimnames=list(c("lower","higher","init","phase"),c("sp1"))),
        SSBpenalty          =matrix(1,ncol=1,nrow=4,dimnames=list(c("use","SSB50","SSB75","factor"),c("sp1"))),
        weighting           =matrix(1,ncol=1,nrow=1,dimnames=list(c("weight"),c("sp1"))),
        at.age.weighting    =0)
        ,
    validity=validFLOPtrigger.control
)                                                         
setValidity("FLOPtrigger.control", validFLOPtrigger.control)

validFLOP.control <- function(object){
    valid.test.output<-c(0,1,2,3,4,11,12,13,19,51,52,53)
    if (!(object@test.output %in% valid.test.output)) return(paste("value of test.ouput must have a value in:", valid.test.output))
    if (object@first.year<0 | object@last.year<0 | object@first.year > object@last.year) return(paste("value of first.year:",object@first.year, "or last.year",object@last.year, "is wrong"))
    if (object@last.year<object@first.year) return("last.year.model must be >= first.year")


    # Everything is fine
    return(TRUE)
}

setClass("FLOP.control",
    representation(
        test.output          ="numeric",
        output               ="numeric",
        indicator            ="numeric",
        first.year           ="numeric",
        first.year.out       ="numeric",
        last.year            ="numeric",
        years.wsea           ="matrix",
        years.M              ="matrix",
        years.propmat        ="matrix",
        years.F              ="matrix",
        years.weca           ="matrix",
        years.prop.landed    ="matrix",
        years.stock.distrib  ="matrix",
        years.ration         ="matrix",
        species.names        ="vector",
        growth.model         ="matrix",
     stochastic.recruitment  ="matrix",
        rec.noise            ="matrix",
        recruit.adjust       ="matrix",
        recruit.adjust.CV    ="matrix",
        recruit.min          ="matrix",
        F.or.C               ="matrix",
        M2.iterations        ="numeric",
        max.M2.sum2          ="numeric",
        years.other          ="matrix",
        other.predator       ="matrix"
  )
  ,
  prototype=prototype(
        test.output         =0,
        output              =0,
        indicator           =0,
        first.year          =1900,
        first.year.out      =1900,
        last.year           =1901,
        years.wsea           =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        years.M              =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        years.propmat        =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        years.F              =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        years.weca           =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        years.prop.landed    =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        years.stock.distrib  =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        years.ration         =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        species.names        =as.vector("sp1",mode="character"),
        growth.model         =matrix(0,ncol=1,nrow=5,dimnames=list(c("Type","first.age","last.age","add.info1","add.info2"),c("sp1"))),
     stochastic.recruitment  =matrix(1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
        rec.noise           =matrix(rep(c(-10,10),1),ncol=1,nrow=2,dimnames=list(c("lower","upper"),c("sp1"))),
        recruit.adjust      =matrix(1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
        recruit.adjust.CV   =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
        recruit.min         =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
        F.or.C              =matrix(11,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
        M2.iterations       =5,
        max.M2.sum2         =0.0,
        years.other         =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
        other.predator      =matrix(-1,ncol=1,nrow=3,dimnames=list(c("factor","first","second"),c("sp1")))
        )
        ,
    validity=validFLOP.control
)

setValidity("FLOP.control", validFLOP.control)

# in final version remove(validOP.control)  # We do not need this function any more
### End class ###########################################################


### Methods #############################################################
 
FLOPtrigger.control <- function(
        first.year=1900,
        last.year=first.year+1, 
        first.VPA=1,
        no.species=1,
        ref=Read.reference.points())
  {
        ref<-ref[first.VPA:(first.VPA+no.species-1),]
        ref[,'Bpa']<-ref[,'Bpa']/1000
        ref[,'Blim']<-ref[,'Blim']/1000
        species.names<-dimnames(ref)[[1]]
        
  
        res <- new("FLOPtrigger.control",
            first.year          =as.integer(first.year),
            first.year.out      =as.integer(first.year),
            last.year           =as.integer(last.year),
            first.run.no        =as.integer(1),
            first.iter.no        =as.integer(1),
            no.iter             =as.integer(1),
            HCR                 =matrix(1,ncol=no.species,nrow=1,dimnames=list(c("HCR"),species.names)),
            trigger             =matrix(c(ref[,'Blim'],ref[,'Bpa']),ncol=no.species,nrow=2,byrow=T,dimnames=list(c("T1","T2"),species.names)),
            Ftarget             =matrix(c(rep(0,no.species),ref[,'Fpa'],ref[,'Fpa']*0.5,rep(1,no.species)),               ncol=no.species,nrow=4,byrow=T,dimnames=list(c("lower","higher","init","phase"),species.names)),
            Fslope              =matrix(c(rep(0,no.species),rep(1e-3,no.species),rep(0,no.species),rep(1,no.species)),    ncol=no.species,nrow=4,byrow=T,dimnames=list(c("lower","higher","init","phase"),species.names)),
            SSB50               =matrix(c(ref[,'Blim']*0.5,ref[,'Bpa'],ref[,'Blim'],rep(1,no.species)),                   ncol=no.species,nrow=4,byrow=T,dimnames=list(c("lower","higher","init","phase"),species.names)),
            S1                  =matrix(c(ref[,'Blim']/100,ref[,'Blim']/10,ref[,'Blim']/50,rep(1,no.species)),            ncol=no.species,nrow=4,byrow=T,dimnames=list(c("lower","higher","init","phase"),species.names)),        
            SSBpenalty          =matrix(c(rep(1,no.species),ref[,'Blim']*1000,rep(10,no.species),rep(10000,no.species)),       ncol=no.species,nrow=4,byrow=T,dimnames=list(c("use","SSB50","SSB75","factor"),species.names)),
            weighting           =matrix(1,ncol=no.species,nrow=1,dimnames=list(c("weight"),species.names)),
            at.age.weighting    =as.integer(0)
         )
 
    return(res)
 }

###

FLOP.control <- function(
        FLOP=NULL,
        first.year=1900,
        first.year.out=1900,
        last.year=first.year+1, 

        no.species=1,
        no.VPA.predators=0,
        no.other.predators=0,
        species.names=c("sp1"),
        first.age=0, 
        max.age.all=10)
  {
    if (is.null(FLOP)){
        if (no.species == 1) {
           no.VPA.sp<-no.species
           no.predators<-0
           no.other.predators<-0
        }
        if (no.species>1) {
          if (no.other.predators>=no.species) stop("no.other.predators cannot be larger than no.species")
          no.VPA.sp<-no.species-no.other.predators
          no.predators<-no.other.predators+no.VPA.predators
        }
        #print(no.predators);  print(no.other.predators)  ; print(no.VPA.predators)
        
        if (species.names[1] != c("sp1") & length(species.names)!=no.species) 
              stop("no.species is different from number of species names")
        VPA.species.names<-species.names[(no.other.predators+1):no.species]
        
        species.info<-matrix(0,ncol=5,nrow=no.species,dimnames=list(species.names,c("last-age","first-age F>0","+group","predator","prey")))
        species.info[,1]<-max.age.all
        species.info[,2]<-first.age
        species.info[,3]<-1
        species.info[,4]<-c(rep(1,no.predators),rep(0,no.VPA.sp-no.VPA.predators))
        species.info[,5]<-c(rep(0,no.other.predators),rep(1,no.VPA.sp))
        if (no.other.predators==0) oth.sp.names<-NULL
        else oth.sp.names<-species.names[1:no.other.predators]

        if (no.other.predators==0) lab.oth.sp<-'dummy' else lab.oth.sp <-oth.sp.names
        res <- new("FLOP.control",
            test.output         =as.integer(0),
            output              =as.integer(15),
            indicator           =as.integer(0),
            first.year          =as.integer(last.year+1),
            first.year.out      =as.integer(last.year+1),
            last.year           =as.integer(last.year+30),
            years.wsea          =matrix(rep(last.year-1,2*no.species),ncol=no.species,nrow=2,dimnames=list(c("first-year","last-year"),species.names)),
            years.M             =matrix(rep(last.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),VPA.species.names)),
            years.propmat       =matrix(rep(last.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),VPA.species.names)),
            years.F             =matrix(rep(last.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),VPA.species.names)),
            years.weca          =matrix(rep(last.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),VPA.species.names)),
            years.prop.landed   =matrix(rep(last.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),VPA.species.names)),
            years.stock.distrib =matrix(rep(last.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),VPA.species.names)),
            years.ration        =matrix(rep(last.year-1,2*no.predators),ncol=no.predators,nrow=2,dimnames=list(c("first-year","last-year"),species.names[1:no.predators])),
            species.names=species.names,
            growth.model         =matrix(0,ncol=no.VPA.sp,nrow=5,dimnames=list(c("Type","first.age","last.age","add.info1","add.info2"),VPA.species.names)),
        stochastic.recruitment  =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",VPA.species.names)),
            rec.noise           =matrix(rep(c(-2 ,2),no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("lower","upper"),VPA.species.names)),
            recruit.adjust      =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",c(VPA.species.names))),
            recruit.adjust.CV   =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",c(VPA.species.names))),
            recruit.min      =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",c(VPA.species.names))),
            F.or.C              =matrix(11,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",c(VPA.species.names))),
            M2.iterations       =as.integer(3),
            max.M2.sum2         =0.0,
            years.other         =matrix(rep(last.year-1,2*max(1,no.other.predators)),ncol=max(1,no.other.predators),nrow=2 ,dimnames=list(c("first-year","last-year"),lab.oth.sp)),
           other.predator       =matrix(rep(c(1,-1,-1),1),ncol=max(1,no.other.predators),nrow=3,dimnames=list(c("factor","first","second"),lab.oth.sp))
          )
       
    }
    else {  # We re-use an FLOP.control object
            if (!inherits(FLOP, "FLOP"))
                stop("FLOP must be an 'FLOP' object!")

            res <- FLOP@control

           # ... and possibly redefine some of its parameters
       if (!missing(test.output))  res@test.output <- test.output
       if (!missing(first.year))  res@first.year <- first.year
       if (!missing(last.year))  res@last.year <- last.year
         # Verify that this object is valid
        test <- validObject(res)

        if (!test) stop("Invalid object:", test)
        }

    return(res)
 }

###
write.FLOPtrigger.control<-function(control,file="op_trigger.dat",path=NULL,nice=TRUE,writeSpNames=TRUE) {

  wr.matrix<-function(m,text){
    cat("# ",text,"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(m[j,],"\n",file=file,append=TRUE)
  }

  wr.matrix.nice<-function(m,sp){
   cat("#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(formatC(m[j,],width=11),"\n",file=file,append=TRUE)
  }

                                        
  if (!inherits(control, "FLOPtrigger.control"))
        stop(paste("control" ,"must be an 'FLOPtrigger.contol' object!"))
  
  old.path<-getwd()
  if (!is.null(path)) setwd(path)
  sepLine<-"########################################\n"

   sp.names<-dimnames(slot(control,"HCR"))[[2]]
  cat("#options and data for optimationzation\n",file=file)
  cat('# the character "#" is used as comment character, such that all text and numbers after # are skipped\n#\n',file=file, append=TRUE)
  
  n.<-slotNames(control)
  for (x in n.) {
      switch(x,
        "first.year"       ={if (nice) {
                                 cat(sepLine,file=file,append=T)
                                 cat("## first year of input data (option first.year)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },

        "last.year"       ={if (nice) {
                                cat("## last year (option last.year)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "first.run.no"       ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## First run number (option first.run.no). Default=1\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
         "first.iter.no"       ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## First iteration number (option first.iter.no). Default=1\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },

        "no.iter"       ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## number of iterations (option no.iter)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },


       "HCR"               ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# HCR by species\n",
                                    "# 1: Target F\n",
                                    "#      F=Ftarget\n",
                                    "# 2: Target F, and known trigger T1 and T2\n",
                                    "#      F=0                        for     SSB<T1\n",
                                    "#      F=Ftarget*(SSB-T1)/(T2-T1) for  T1<SSB<T2\n",
                                    "#      F=Ftarget                  for  T2<SSB\n",
                                    "# 3: Target F and Fslope, and known trigger T1 and T2\n",
                                    "#      F=0                          for     SSB<T1\n",
                                    "#      F=Ftarget*(SSB-T1)/(T2-T1)   for  T1<SSB<T2\n",
                                    "#      F=Ftarget+(SSB-T2)/T2*Fslope for  T2<SSB\n",
                                    "#  4: Logistic curve\n", 
                                    "#      F=Ftarget / (1+exp(S1 - S1/SSB50%*SSB)\n",
                                    "#  22, 33 and 44 is the same rule as 2,3 and 4 but TSB is used\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },

       "trigger"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# T1 and T2 trigger by species\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                              },

       "Ftarget"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Values for Ftarget\n",
                                    "# 1) lower limit, 2) higher limit, 3) initialization, 4) estimation phase\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                              },
       "Fslope"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Values for Fslope\n",
                                    "# 1) lower limit, 2) higher limit, 3) initialization, 4) estimation phase\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                              },
       "SSB50"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Values for logistic curve, SSB50% of Ftarget (HCR=4)\n",
                                "# 1) lower limit of SSB50% in 1000 tonnes , 2) higher limit, 3) initialization, 4) estimation phase\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                              },
       "S1"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Values for logistic curve, S1 (HCR=4)\n",
                                "# 1) lower limit of S1 in 1000 tonnes , 2) higher limit, 3) initialization, 4) estimation phase\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                              },
       "SSBpenalty"     ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Values for penalty function for SSB below limit (HCR=4)\n",
                                "# 1) use penalty 0=no, 1=logistic, 2=simple power function \n# 2) limit (SSB50%), 3) steepnes (SSB75%), 4) factor\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                              },                          
   "weighting"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# relative weighting (e.g. price/kg, all sizes combined) of yield used in objective function\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                              },
                             
   "at.age.weighting"          ={if (nice) {
                                cat(sepLine,file=file,append=TRUE) 
                                cat("# use weighting factor by age (e.g. price) from file op_price.in\n",
                                    "# 0=no use; 1=use\n", 
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             }
   )  #end switch
  }
 
 setwd(old.path)
}

#####

write.FLOP.control<-function(control,file="op.dat",path=NULL,nice=TRUE,writeSpNames=F) {

  if (!inherits(control, "FLOP.control")) stop(paste("control" ,"must be an 'FLOP.contol' object!\n"))

    wr.matrix<-function(m,text){
    cat("# ",text,"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(m[j,],"\n",file=file,append=TRUE)
  }

  wr.matrix.nice<-function(m,sp){
   cat("#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(formatC(m[j,],width=11),"\n",file=file,append=TRUE)
  }

  wr.vector.nice<-function(m,sp){
    cat("# ",formatC(sp,width=11),"\n  ",formatC(m,width=11),"\n",file=file,append=TRUE)
  }
    
  wr.list<-function(l,text1,text2){
    for (j in 1:length(l)) cat(length(l[[j]])," ",file=file,append=TRUE) 
    cat("\t#",text1,"\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\n",file=file,append=TRUE)    
   }
   
  wr.list.nice<-function(l,text1,text2,sp){
    cat("#",text1,"\n#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    for (j in 1:length(l)) cat(formatC(length(l[[j]]),width=12),file=file,append=TRUE) 
    cat("\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\t# ",sp[j],"\n",file=file,append=TRUE)    
   }

  wr.list2<-function(l,text1,text2){
    for (j in 1:length(l)) {
       ifelse(l[[j]]==0, out<-0,out<-length(l[[j]]))
       cat(out," ",file=file,append=TRUE) 
    }
    cat("\t#",text1,"\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\n",file=file,append=TRUE)    
   }
  wr.list2.nice<-function(l,text1,text2,sp){
    cat("#",text1,"\n#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    for (j in 1:length(l)) {
       ifelse(l[[j]]==0, out<-0,out<-length(l[[j]]))
       cat(formatC(out,width=12),file=file,append=TRUE) 
    }
    cat("\n# ",text2,"\n",file=file,append=TRUE)  
    for (j in 1:length(l)) cat(l[[j]],"\t# ",sp[j],"\n",file=file,append=TRUE)    
   }
                                        
  
  old.path<-getwd()
  if (!is.null(path)) setwd(path)
  sepLine<-"########################################\n"
  last.pred<-1
  sp.names<-slot(control,"species.names")
  nsp<<-length(sp.names)
  n.VPA<-dim(control@rec.noise)[[2]]
  n.other.pred<-nsp-n.VPA
  first.VPA<-nsp-n.VPA+1
  #cat('first.VPA',first.VPA,'\n')
  VPA.species<-sp.names[(n.other.pred+1):nsp]
  
  cat("# op.dat option file\n",file=file)
  cat('# the character "#" is used as comment character, such that all text and numbers after # are skipped\n#\n',file=file, append=TRUE)
  n.<-slotNames(control)
  for (x in n.) {
      switch(x,
        "test.output"        ={ if (nice)  {
                                cat(sepLine,file=file,append=TRUE) 
                                cat("# Produce TEST output (option test.output)\n",
                                    "#  0 no test output\n",
                                    "#  1 output file op.dat  as read in\n",
                                    "#  2 output op_config.in as read in\n",
                                    "#  3 output all data files as read in\n",
                                    "#  4 output file op_trigger.in as read in\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                            },
       "output"             ={ if (nice)  {
                                cat(sepLine,file=file,append=TRUE)
                                cat("# Produce output (option output)\n",
                                    "# 10 stock N by year and age (file op_n.out) \n",
                                    "# 11 as 10 plus output by year and age (file op_summary_anno.out) \n",
                                    "# 12 as 10 plus output by Year, quarter, areas and age (file op_summary.out)\n",
                                    "# 13 condensed output (file op_condensed.out)\n",
                                    "# 14 condensed from F-combinations (file op_Fcombinations.out)\n",
                                    "# 15 10 + 11 + 12 + 13\n",
                                    "# 20 condensed output for last year only, AMOEBA, (file op_condensed.out)\n",
                                    "# 21 condensed output for all years, AMOEBA, (file op_condensed_long.out)\n",
                                    "# 22 M, F and N by output by year and age,  AMOEBA, (file op_anno_M.out)\n", 
                                    "# 25 20 + 21 + 22\n",
                                    "# 26 25 + output by Year, quarter, areas and age (file op_summary.out) and partial M2 (file op_part_m2.out)\n",
                                    
                                    
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              }
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
       "indicator"             ={ if (nice)  {
                                cat(sepLine,file=file,append=TRUE)
                                cat("# Produce output MSFD indicator output, defined from file op_msfd.dat (option indicator)\n",
                                    "# 0=no, 1=yes \n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              }
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "first.year"       ={if (nice) {
                                 cat(sepLine,file=file,append=T)
                                 cat("## first year of input data (option first.year)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "first.year.out"       ={if (nice) {
                                 cat(sepLine,file=file,append=T)
                                 cat("## first year for output data (option first.year.out)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "last.year"       ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## last year (option last.year)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
       "years.wsea"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of mean weight in the sea (file op_wsea.in) \n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },
       "years.M"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of natural mortalities (file op_m.in & op_m1.in)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                              },
     "years.propmat"        ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of proportion mature (file op_propmat.in)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                              },
       "years.F"            ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of F at age and exploitation pattern in the catch (file op_f.in & op_exploitation.in)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                              },
      "years.weca"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of mean weight in the catch file op_wcatch.in)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
      "years.prop.landed"   ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of proportion landed of the catch (file op_prop_landed.in)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
     "years.stock.distrib"    ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of stock distribution (file op_stock_distribution.in)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
     "years.ration"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of food ration (file op_consum.in)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),dimnames(slot(control,x))[[2]])
                                } else wr.matrix(slot(control,x),x)
                             },
     "species.names"      ={ cat(sepLine,file=file,append=T)
                               cat("# Species names, for information only. See file species_names.in \n# ",sp.names,"\n",file=file,append=T)
                              },
     "growth.model"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Growth model:\n",
                                    "#   1  : Type  0=no growth;  1=density dependent, 10=Saithe special (Xochitl Cormon)\n",
                                    "#   2-3: First and last age for growth\n",
                                    "#   4-5: Additional info1 and info2 \n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
  "stochastic.recruitment" ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Stochastic recruitment.\n",
                                    "# 0=No,   S-R parameters from op_config.dat\n", 
                                    "# 1=Yes   S-R parameters and variance from op_config.dat\n",
                                    "# 2=Yes,  S-R parameters from op_config.dat and variance-covariance matrix from file covariance_Rec.in\n",
                                    "# 3=Yes,  S-R parameters from op_config.dat and residuals from op_ssb_rec_residuals.in\n",
                                    "# 20=No,  S-R parameters from EquiSim results, file op_eqsim.in\n",
                                    "# 21=Yes, S-R parameters from EquiSim results, file op_eqsim_stoch.in\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
       "rec.noise"        ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# truncation of standardised normal distribution used to produce noise on recruitment\n",
                                    "# lower and upper   (values -15.0 and 15.0 give no truncation, -2 and 2 give approximately 95% of the distribution)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
       "recruit.adjust"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# factor to adjust expected recruits (option adjust.recruits)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
  
      "recruit.adjust.CV"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# adjust recruitment with half of the variance (factor exp(-(CV^2)/2) option adjust.recruit.CV\n",
                                    "# 0=no adjustment\n",
                                    "# 1=stochastic recruitment, do adjustment such that average recruitment becomes equal to the median (downward adjustment)\n",
                                    "# 2=determenistic recruitment, do ajustment such that recruitment becomes equal to the mean of the log-normal distribution (upward adjustment)\n", file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
       "recruit.min"        ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# minimum recruitment in percentage of recruitment from S/R relation. (0 means that the S/R values are always used))\n",file=file,append=T,sep="")
                                wr.matrix.nice(slot(control,x),VPA.species)
                              } else wr.matrix(slot(control,x),x)
                              },
  
      "F.or.C"              ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Update N by:\n",
                                    "#     1=F at age or 2=Catch at age (option F.or.C)\n",
                                    "#    11=as 1 but re-use input F, 22=as 2 but re-use catch\n",
                                    "#    31= as 1 but get target F values from file op_mulTargetf.in\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.species)
                                } else wr.matrix(slot(control,x),x)
                             },
        "M2.iterations"   = {if (nice)  {
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## Maximum M2 iterations (option M2.iterations)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },      
        "max.M2.sum2"      = {if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## convergence criteria (option max.M2.sum2)\n",
                                    "#  use max.M2.sum2=0.0 and M2.iterations=7 (or another high number) to make Hessian\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },
        "years.other"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calculation of mean stock numbers of other predators (file op_other_n.in) \n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(control,x),sp.names[1:(first.VPA-1)])
                                } else wr.matrix(slot(control,x),x)
                             },

        "other.predator"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%% various for other predators %%%%%%%%%%%%%%%%%%% ###\n",
                                    "# annual change factor for population number\n",
                                    "# first year year of change  (-1 is no change)\n",
                                    "# last year  of change (-1 is no change)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names[1:(first.VPA-1)])
                                } else wr.matrix(slot(control,x),x)
                             }
        #other wise                       
                              # cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
   )  #end switch
  }


  if (writeSpNames) {
      sp.names<-slot(control,"species.names")
      sp.names<-substr(paste(sp.names,"___________",sep=''),1,11)
      sp.names<-gsub(' ','_',sp.names)
      write(sp.names,file="species_names.in")
      cat("12345678901\nPlease note. exactly 11 charaters for species names !!!!\n",file="species_names.in",append=TRUE) 
  }
  setwd(old.path)
}
###

read.FLOPtrigger.control<-function(file="op_trigger.dat",path=data.path,n.VPA,n.other.pred) {
  
  nsp<-n.VPA+n.other.pred
  species.names<-readLines(file.path(path,"species_names.in"), n=nsp)
  species.names<-species.names[(n.other.pred+1):nsp]
  species.names<-gsub('_',' ',species.names)
  species.names<-sub('[[:space:]]+$', '', species.names)
  
  opt<-scan(file.path(path,file), comment.char = "#",quiet=TRUE) 
   
  n<-1
  control<-new("FLOPtrigger.control")
  n.<-slotNames(control)
  for (x in n.) {
    switch(x,
      "HCR"                   = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("HCR"),species.names),byrow=TRUE); n<-n+1*n.VPA},
      "trigger"               = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("T1","T2"),species.names),byrow=TRUE); n<-n+2*n.VPA},
      "Ftarget"               = {slot(control,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("lower","higher","init","phase"),species.names),byrow=TRUE); n<-n+4*n.VPA},
      "Fslope"                = {slot(control,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("lower","higher","init","phase"),species.names),byrow=TRUE); n<-n+4*n.VPA},
      "SSB50"                 = {slot(control,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                    dimnames=list(c("lower","higher","init","phase"),species.names),byrow=TRUE); n<-n+4*n.VPA},
      "S1"                    = {slot(control,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("lower","higher","init","phase"),species.names),byrow=TRUE); n<-n+4*n.VPA},
      "SSBpenalty"            = {slot(control,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("use","SSB50","SSB75","factor"),species.names),byrow=TRUE); n<-n+4*n.VPA},
      "weighting"             = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("weight"),species.names),byrow=TRUE); n<-n+1*n.VPA}
                               ,
         # otherwise                        
                              {slot(control,x)<-opt[n];n<-n+1}                                 
      )
  }
  control
}

###

read.FLOP.control<-function(file="op.dat",path=data.path,n.VPA,n.other.pred,n.pred) {

  nsp<-n.VPA+n.other.pred
  species.names<-readLines(file.path(path,"species_names.in"), n=nsp)
  species.names<-gsub('_',' ',species.names)
  species.names<-sub('[[:space:]]+$', '', species.names)
  VPA.sp.names<-species.names[(n.other.pred+1):nsp]
  
  opt<-scan(file.path(path,file), comment.char = "#",quiet=TRUE) 
   
  n<-1
  control<-new("FLOP.control")
  n.<-slotNames(control)
  if (n.other.pred==0) n.<-n.[1:(length(n.)-2)]
  for (x in n.) {
    switch(x,
    "species.names"            = { species.names<-readLines(file.path(path,"species_names.in"), n=nsp)
                                   species.names<-gsub('_',' ',species.names)
                                   species.names<-sub('[[:space:]]+$', '', species.names)
                                   slot(control,x)<-species.names; },
     "years.wsea"              = {slot(control,x)<-matrix(opt[n:(n-1+2*nsp)],ncol=nsp,nrow=2,
                                   dimnames=list(c("first","last"),species.names),byrow=TRUE); n<-n+2*nsp},
     "years.M"                 = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","last"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.propmat"           = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","last"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.F"                 = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","last"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.weca"              = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","last"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.prop.landed"        = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","last"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.stock.distrib"      = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","last"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.ration"             = {slot(control,x)<-matrix(opt[n:(n-1+2*n.pred)],ncol=n.pred,nrow=2,
                                   dimnames=list(c("first","last"),species.names[1:n.pred]),byrow=TRUE); n<-n+2*n.pred},
     "growth.model"             = {slot(control,x)<-matrix(opt[n:(n-1+5*n.VPA)],ncol=n.VPA,nrow=5,dimnames=list(c("Type","first.age","last.age","add.info1","add.info2"),VPA.sp.names),byrow=TRUE); n<-n+5*n.VPA},
     "stochastic.recruitment"   = {slot(control,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
      "rec.noise"               = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("lower","upper"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "recruit.adjust"          = {slot(control,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "recruit.adjust.CV"       = {slot(control,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "recruit.min"             = {slot(control,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "F.or.C"                  ={slot(control,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "years.other"             = {slot(control,x)<-matrix(opt[n:(n-1+2*n.other.pred)],ncol=n.other.pred,nrow=2,
                                   dimnames=list(c("first","last"),species.names[1:n.other.pred]),byrow=TRUE); n<-n+2*n.other.pred},
     "other.predator"          = {if (n.other.pred>=1) {
                                   slot(control,x)<-matrix(opt[n:(n-1+3*n.other.pred)],ncol=n.other.pred,nrow=3,
                                   dimnames=list(c("factor","first","second"),species.names[1:n.other.pred]),byrow=TRUE); n<-n+3*n.other.pred
                                 }
                                 },
         # otherwise                        
                                 {slot(control,x)<-opt[n];n<-n+1}                                 
      )
  }
  control
}



## show (a replacement for print of S3 classes)
setMethod("show", signature(object="FLOP.control"),
    function(object){
      n.<-slotNames(object)
       for (i in 1:length(n.)) {
         cat(n.[i]) 
         for (j in nchar(n.[i]):25) cat(" ")
        
         if (is.integer(slot(object,n.[i]))) cat(slot(object,n.[i]),"\n")
        
         else if (is.vector(slot(object,n.[i]))) {          
           if (is.list(slot(object,n.[i]))) { 
              for (k in 1:length(slot(object,n.[i]))) cat("\n\t",slot(object,n.[i])[[k]])
              cat("\n")
           }
           else cat(slot(object,n.[i]),"\n")
         }
         else if (is.matrix(slot(object,n.[i]))) {
           m<-slot(object,n.[i])
           cat("\n")
           print(m)
         }
       }
    }
)


## show (a replacement for print of S3 classes)
setMethod("show", signature(object="FLOPtrigger.control"),
    function(object){
      n.<-slotNames(object)
       for (i in 1:length(n.)) {
         cat(n.[i]) 
         for (j in nchar(n.[i]):25) cat(" ")
        
         if (is.integer(slot(object,n.[i]))) cat(slot(object,n.[i]),"\n")
        
         else if (is.vector(slot(object,n.[i]))) {          
           if (is.list(slot(object,n.[i]))) { 
              for (k in 1:length(slot(object,n.[i]))) cat("\n\t",slot(object,n.[i])[[k]])
              cat("\n")
           }
           else cat(slot(object,n.[i]),"\n")
         }
         else if (is.matrix(slot(object,n.[i]))) {
           m<-slot(object,n.[i])
           cat("\n")
           print(m)
         }
       }
    }
)
