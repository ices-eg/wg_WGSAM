### class ######################################################################

validFLSMS.control <- function(object){
    valid.test.output<-c(0,1,2,3,4,11,12,13,19,51,52,53)
    if (!(object@test.output %in% valid.test.output))
        return(paste("value of test.ouput must have a value in:", valid.test.output))
    if (object@VPA.mode<0 & object@VPA.mode>2)
        return("value of VPA.mode must be in the range 0-2")
    if (object@first.year<0 | object@last.year<0 | object@first.year > object@last.year)
        return(paste("value of first.year:",object@first.year, "or last.year",object@last.year, "is wrong"))
    if (object@last.year.model>object@last.year) 
        return(paste("last.year model(",object@last.year.model,") must be <= last.year (",object@last.year.model,")",sep=""))
    if (object@last.year.model<=object@first.year) 
        return("last.year.model must be > first.year")
    if (object@last.season < 1) 
        return("last.season must be >= 1")
    if (object@last.season.last.year < 1 | object@last.season.last.year > object@last.season) 
        return("last.season.last.year must be in the range 1:last.season")


    # Everything is fine
    return(TRUE)
}

setClass("FLSMS.control",
    representation(
        test.output         ="numeric",
        OP.output           ="numeric",
        VPA.mode            ="numeric",
        no.areas            ="numeric",
        first.year          ="numeric",
        first.year.model    ="numeric",
        last.year           ="numeric",
        last.year.model     ="numeric",
        last.season         ="numeric",
        last.season.last.year="numeric",
        no.species          ="numeric",
        species.names       ="vector",
        first.age           ="numeric",
        rec.season          ="numeric",
        max.age.all         ="numeric",
        species.info        ="matrix",
        use.known.rec       ="numeric",
        beta.cor            ="vector",
        SSB.R.year.first    ="vector",
        SSB.R.year.last     ="vector",
        obj.func.weight     ="matrix",
        phase.rec           ="numeric",
        phase.rec.older     ="numeric",
        phase.F.y           ="numeric",
        phase.F.y.spline    ="numeric",
        phase.F.q           ="numeric",
        phase.F.a           ="numeric",
        phase.catchability  ="numeric",
        phase.SSB.R.alfa    ="numeric",
        phase.SSB.R.beta    ="numeric",
        min.catch.CV        ="numeric",
        min.SR.CV           ="numeric",
        discard             ="vector",
        combined.catches    ="vector",
        seasonal.catch.s2   ="vector",
        catch.s2.group      ="vector",
        catch.season.age    ="vector",
        avg.F.ages          ="matrix",
        min.catch           ="vector",
        catch.sep.year      ="vector",
        catch.spline.year   ="vector",
        zero.catch.year.season="numeric",
        zero.catch.season.age="numeric",
        fix.F.factor        ="vector",
        est.calc.sigma      ="vector",
        read.HCR            ="numeric",
        incl.stom.all       ="numeric",
        use.Nbar            ="numeric",
        M2.iterations       ="numeric",
        max.M2.sum2         ="numeric",
        stom.likelihood     ="numeric",
        stomach.variance    ="numeric",
        simple.ALK          ="numeric",
        consum              ="numeric",
        size.select.model   ="numeric",
        L50.mesh            ="vector",
        size.selection      ="vector",
        sum.stom.like       ="vector",
        stom.obs.var        ="vector",
        stom.max.sumP        ="vector",
        var.scale.stom      ="vector",
        size.other.food.suit="vector",
        min.stom.cont       ="vector",
        max.stom.sampl      ="vector",
        prey.pred.size.fac  ="vector",
        stom.type.include   ="vector",
        use.overlap         ="numeric",
        phase.vulnera       ="numeric",
        phase.other.suit.slope ="numeric",
        phase.pref.size.ratio  ="numeric",
        phase.pref.size.ratio.correction ="numeric",
        phase.prey.size.adjustment ="numeric",
        phase.var.size.ratio ="numeric",
        phase.season.overlap ="numeric",
        phase.stom.var       ="numeric",
        phase.mesh.adjust    ="numeric"
  )
  ,
  prototype=prototype(
        test.output     =0,
        OP.output       =0,
        VPA.mode        =0,
        no.areas        =1,                                            
        first.year      =1900, 
        first.year.model=1901,                                            
        last.year       =1901,                                            
        last.year.model =1901,                                            
        last.season     =1,                                            
        last.season.last.year=1,                                       
        no.species      =1,  
        species.names   =as.vector("sp1",mode="character"),                                         
        first.age       =0,                                            
        rec.season      =1,                                            
        max.age.all     =0,                                            
        species.info    =matrix(0,ncol=11,nrow=1,dimnames=list(c("sp1"),c("last-age","first-age F>0","last-age-selec","effort",
                                                "last-age-likelihood","+group","predator","prey","SSB/R","SpawningQ","RecAdd2"))),
        use.known.rec   =0,
        beta.cor        =as.vector(1.0E6,mode="numeric"),
        SSB.R.year.first=as.vector(0,mode="numeric"),
        SSB.R.year.last=as.vector(0,mode="numeric"),
        obj.func.weight =matrix(1,ncol=5,nrow=1,dimnames=list(c("sp1"),c("catch","survey","SSB/R",
                                                "stomach1","stomach2"))),                                        
        phase.rec       =1,                                                  
        phase.rec.older =1,                                                   
        phase.F.y       =1,
        phase.F.y.spline=-1,
        phase.F.q       =1,                                                  
        phase.F.a       =1,                                                  
        phase.catchability=1,                                                
        phase.SSB.R.alfa=1,                                                  
        phase.SSB.R.beta=1,                                                  
        min.catch.CV    =0.2,                                                
        min.SR.CV       =0.2,
        discard         =as.vector(0,mode="list"),
        combined.catches=as.vector(0,mode="list"),                                                 
        seasonal.catch.s2=as.vector(0,mode="list"),                                                 
        catch.s2.group  =as.vector(0,mode="list"),                                       
        catch.season.age=as.vector(0,mode="list"),                                       
        avg.F.ages      =matrix(0,ncol=2,nrow=1,dimnames=list(c("sp1"),c("first-age","last-age"))),                                        
        min.catch       =as.vector(-5,mode="numeric"),                                    
        catch.sep.year  =as.vector(0,mode="list"),
        catch.spline.year =as.vector(0,mode="list"),
        zero.catch.year.season =0,
        zero.catch.season.age =0,
        fix.F.factor    = as.vector(1,mode="numeric"),
        est.calc.sigma  =as.vector(0,mode="numeric"),                                   
        read.HCR        =0, 
        incl.stom.all   =0, 
        use.Nbar        =0,
        M2.iterations   =3,
        max.M2.sum2     =0.0, 
        stom.likelihood     =1,                                              
        stomach.variance    =1,                                              
        simple.ALK          =0, 
        consum              =0,                                            
        size.select.model   =2, 
        L50.mesh            =as.vector(0,mode="numeric"),                                                                           
        size.selection      =as.vector(0,mode="numeric"),  
        sum.stom.like       =as.vector(0,mode="numeric"),
        stom.obs.var        =as.vector(0,mode="numeric"),
        stom.max.sumP        =as.vector(0,mode="numeric"),
        var.scale.stom      =as.vector(0,mode="numeric"),
        size.other.food.suit=as.vector(0,mode="numeric"),
        min.stom.cont       =as.vector(0,mode="numeric"),
        max.stom.sampl      =as.vector(0,mode="numeric"),
        prey.pred.size.fac  =as.vector(0,mode="numeric"), 
        stom.type.include   =as.vector(1,mode="numeric"),                               
        use.overlap         =0,                                              
        phase.vulnera       =2,                                             
        phase.other.suit.slope =2,                                          
        phase.pref.size.ratio  =2,                                          
        phase.pref.size.ratio.correction =-1,                                
        phase.prey.size.adjustment =-1,                                      
        phase.var.size.ratio =-1,                                            
        phase.season.overlap =-1,                                            
        phase.stom.var       =2,
        phase.mesh.adjust    =-1                                                                                                              
        )                                                           
        ,
    validity=validFLSMS.control
)

setValidity("FLSMS.control", validFLSMS.control)

# in final version remove(validSMS.control)  # We do not need this function any more


### End class ###########################################################


### Methods #############################################################

FLSMS.control <- function(  
        FLSMS=NULL,
        first.year=1900,
        first.year.model=1900,
        last.year=first.year+1, 
        last.season=1,  
        no.species=1,
        no.VPA.predators=0,
        no.other.predators=0,
        species.names=c("sp1"),
        first.age=0, 
        max.age.all=10)
  {
    if (is.null(FLSMS)){
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
        if (species.names[1] != c("sp1") & length(species.names)!=no.species) 
              stop("no.species is diffrent from number of species names")
     
        species.info<-matrix(0,ncol=11,nrow=no.species,dimnames=list(species.names,c("last-age","first-age F>0","effort",
                               "last-age-selec","last-age-likelihood","+group","predator","prey","SSB/R","SpawningQ","RecAdd2")))
        species.info[,1]<-max.age.all
        species.info[,2]<-first.age
        species.info[,3]<-max.age.all
        species.info[,4]<-0
        species.info[,5]<-max.age.all       
        species.info[,6]<-1
        species.info[,7]<-c(rep(0,no.predators),rep(0,no.VPA.sp-no.VPA.predators))
        species.info[,8]<-c(rep(0,no.other.predators),rep(1,no.VPA.sp))
        species.info[,9]<-c(rep(0,no.other.predators),rep(3,no.VPA.sp))
        species.info[,10]<-c(rep(0,no.other.predators),rep(1,no.VPA.sp))
        species.info[,11]<-c(rep(0,no.other.predators),rep(0,no.VPA.sp))
        catch.s2.group<-vector("list", length=no.VPA.sp);
        for (j in 1:no.VPA.sp) catch.s2.group[[j]]<-as.integer(first.age:(first.age+2)) 
        catch.season.age<-vector("list", length=no.VPA.sp)
        for (j in 1:no.VPA.sp) {
          if (last.season>1) catch.season.age[[j]]<-as.integer(first.age:(first.age+1)) else 
               catch.season.age[[j]]<-as.integer(first.age)
        }
        catch.sep.year<-vector("list", length=no.VPA.sp)
        for (j in 1:no.VPA.sp) catch.sep.year[[j]]<-as.integer(first.year.model); 
        catch.spline.year<-vector("list", length=no.VPA.sp)
        for (j in 1:no.VPA.sp) catch.spline.year[[j]]<-as.integer(first.year.model);
        zero.catch.year.season<-as.integer(0)
        zero.catch.season.age<-as.integer(0)

        res <- new("FLSMS.control",
           test.output=as.integer(0) ,
           OP.output=as.integer(0),
           VPA.mode=as.integer(0),
           no.areas=as.integer(1),
           first.year=as.integer(first.year), 
           first.year.model=as.integer(first.year.model), 
           last.year=as.integer(last.year),
           last.year.model=as.integer(last.year),
           last.season=as.integer(last.season),
           last.season.last.year=as.integer(last.season),          
           no.species=as.integer(no.species),
           first.age=as.integer(first.age),
           max.age.all=as.integer(max.age.all),
           species.names=species.names,
           species.info=species.info,
           use.known.rec=as.integer(0),
           beta.cor        =rep(1E6,no.VPA.sp),
           SSB.R.year.first=rep(first.year.model,no.VPA.sp),
           SSB.R.year.last=rep(last.year,no.VPA.sp)  ,
            obj.func.weight =matrix(1,ncol=4,nrow=no.species,dimnames=list(species.names,
                                 c("catch","survey","SSB/R","stomach1","stomach2"))),
           seasonal.catch.s2=rep(0,no.VPA.sp),                                                 
           catch.s2.group=catch.s2.group,
           catch.season.age=catch.season.age,
           avg.F.ages      =matrix(0,ncol=2,nrow=no.VPA.sp,
                               dimnames=list(species.names[1:no.VPA.sp],c("first-age","last-age"))),
           min.catch=rep(-5,no.VPA.sp),
           catch.sep.year=catch.sep.year,
           catch.spline.year=catch.spline.year,
           zero.catch.year.season=zero.catch.year.season,
           zero.catch.season.age=zero.catch.season.age,
           fix.F.factor=rep(1,no.VPA.sp),
           est.calc.sigma   =c(0,0,0),
        size.selection      =rep(0,no.predators),
        L50.mesh            =rep(-1,no.VPA.sp),
        var.scale.stom      =rep(0,no.predators),
        size.other.food.suit=rep(0,no.predators),                                
        min.stom.cont       =rep(1E-4,no.predators),
        max.stom.sampl      =rep(1E4,no.predators),
        prey.pred.size.fac  =rep(0.5,no.predators),
        stom.type.include   =rep(1,no.predators)                               
       
        )
    }
    else {  # We re-use an FLSMS.control object 
            if (!inherits(FLSMS, "FLSMS"))
                stop("FLSMS must be an 'FLSMS' object!")

            res <- FLSMS@control

           # ... and possibly redefine some of its parameters
       if (!missing(test.output))
            res@test.output <- test.output
       if (!missing(VPA.mode))
                res@VPA.mode <- as.integer(VPA.mode)
       if (!missing(no.areas))
                res@no.areas <- as.integer(no.areas)
       if (!missing(first.year))
                res@first.year <- first.year
       if (!missing(first.year.model))
                res@first.year.model <- first.year.model
       if (!missing(last.year))
                res@last.year <- last.year
       if (!missing(last.year.model))
                res@last.year.model <- as.integer(last.year.model)
       if (!missing(last.season))
            res@last.season <- as.integer(last.season)
       if (!missing(last.season.last.year))
            res@last.season.last.year <- as.integer(last.season.last.year)
       if (!missing(no.species))
            res@no.species <- as.integer(no.species)
       if (!missing(first.age))
            res@first.age <- as.integer(first.age)
       if (!missing(rec.season))
            res@rec.season <- as.integer(rec.season)
       if (!missing(max.age.all))
            res@max.age.all <- as.integer(max.age.all)
       if (!missing(species.info))
            res@species.info <- as.matrix(species.info)

        # Verify that this object is valid
        test <- validObject(res)

        if (!test) stop("Invalid object:", test)
        }

    return(res)
 }


write.FLSMS.control<-function(control,file="sms.dat",path=NULL,write.multi=TRUE,nice=TRUE,writeSpNames=T,expand=F) {

  wr.matrix<-function(m,text){
    cat("# ",text,"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(m[j,],"\n",file=file,append=TRUE)
  }
  
  wr.matrix.nice<-function(m,sp){
    for (j in (1:dim(m)[1])) cat(m[j,]," #",sp[j],"\n",file=file,append=TRUE)
  }
  
  wr.vector.nice<-function(m,sp){
    cat("# ",formatC(sp,width=11),"\n  ",formatC(m,width=11),"\n",file=file,append=TRUE)
  }

  wr.vector.expand<-function(m,sp){
    for (j in 1:length(m)) cat(formatC(m[j],width=11)," # ",formatC(sp[j],width=11),"\n",file=file,append=TRUE)
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
  
  wr.list.expand<-function(l,text1,text2,sp){
    #cat("#",text1,"\n#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    cat("#",text1,"\n",file=file,append=TRUE)
    for (j in 1:length(l)) cat(formatC(length(l[[j]]),width=12), "  #",formatC(sp[j],width=11),'\n', file=file,append=TRUE) 
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
                                        
  if (!inherits(control, "FLSMS.control"))
        stop(paste("control" ,"must be an 'FLSMS.contol' object!")) 
  
  old.path<-getwd()
  if (!is.null(path)) setwd(path) else path<-old.path

  sepLine<-"########################################\n"
   last.pred<-1
  sp.names<-slot(control,"species.names")
  nsp<<-control@no.species
  for (ii in (1:nsp)) if (control@species.info[ii,'predator']!=2) {first.VPA<-ii; break;} #first VPA  species number
  VPA.species<-sp.names[first.VPA:length(sp.names)]
  for (ii in (1:nsp)) if (control@species.info[ii,'predator']==0) {last.pred<-ii-1; break;} #first VPA  species number
  pred.species<-sp.names[1:last.pred]
  
      
  cat("# sms.dat option file\n",file=file)
  cat('# the character "#" is used as comment character, such that all text and numbers\n# after # are skipped by the SMS program\n#\n',file=file, append=TRUE)
  n.<-slotNames(control)
  for (x in n.) {
      switch(x,
        "test.output"        ={ if (nice)  {
                                cat(sepLine,file=file,append=TRUE) 
                                cat("# Produce test output (option test.output)\n",
                                    "#  0 no test output\n",
                                    "#  1 output file sms.dat and  file fleet.info.dat as read in\n",
                                    "#  2 output all single species input files as read in\n",
                                    "#  3 output all multi species input files as read in\n",
                                    "#  4 output option overview\n",
                                    "#\n",
                                    "# 11 output between phases output\n",
                                    "# 12 output iteration (obj function) output\n", 
                                    "# 13 output stomach parameters\n",
                                    "# 19 Both 11, 12 and 13\n",
                                    "#\n",
                                    "# Forecast options\n",
                                    "# 51 output hcr_option.dat file as read in\n",
                                    "# 52 output prediction output summary\n",
                                    "# 53 output prediction output detailed\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },
         "OP.output"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Produce output for SMS-OP program. 0=no, 1=yes\n",
                                     slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },

        "VPA.mode"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Single/Multispecies mode (option VPA.mode)\n",
                                    "# 0=single species mode\n",
                                    "# 1=multi species mode, but Z=F+M (used for initial food suitability parm. est.)\n",
                                    "# 2=multi species mode, Z=F+M1+M2\n",
                                     slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "no.areas"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Number of areas for multispecies run (default=1)\n",
                                     slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
  
        "first.year"        ={if (nice) {
                                  cat("#\n#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                                     "# single species parameters\n#\n",
                                      "#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                                      file=file,append=T,sep='')
                                cat("## first year of input data (option first.year)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
       "first.year.model"   ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## first year used in the model (option first.year.model)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },


        "last.year"         ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## last year of input data (option last.year)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "last.year.model"   ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## last year used in the model (option last.year.model)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "last.season"       ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("##  number of seasons (option last.season). Use 1 for annual data\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "last.season.last.year"={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## last season last year (option last.season.last.year). Use 1 for annual data\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "no.species"        ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## number of species (option no.species)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "species.names"      ={ cat(sepLine,file=file,append=T)
                               cat("# Species names, for information only. See file species_names.in \n# ",sp.names,"\n",file=file,append=T)
                              },
        "first.age"         ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## first age all species (option first.age)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "rec.season"        ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## recruitment season (option rec.season). Use 1 for annual data\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "max.age.all"       ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## maximum age for any species(max.age.all)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
                             
        "species.info"      ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## various information by species\n",
                                    "# 1. last age \n",
                                    "# 2. first age where catch data are used (else F=0 assumed)\n",
                                    "# 3. last age with age dependent fishing selection\n",
                                    "# 4. Esimate F year effect from effort data. 0=no, 1=yes\n",
                                    "# 5. Last age included in the catch at age likelihood (normally last age)\n",
                                    "# 6. plus group, 0=no plus group, 1=plus group\n",
                                    "# 7. predator species, 0=no, 1=VPA predator, 2=Other predator\n",
                                    "# 8. prey species, 0=no, 1=yes\n",
                                    "# 9. Stock Recruit relation\n",
                                    "#      1=Ricker, 2=Beverton & Holt, 3=Geom mean,\n",
                                    "#      4= Hockey stick, 5=hockey stick with smoother,\n",
                                    "#      51=Ricker with estimated temp effect,\n",
                                    "#      52=Ricker with known temp effect,\n",
                                    "#      >100= hockey stick with known breakpoint (given as input)\n",
                                    "# 10. Spawning season (not used yet, but set to 1)\n",
                                    "# 11. Additional data for Stock Recruit relation\n",
                                    "##\n",
                                    file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),paste(1:length(sp.names),sp.names))
                                } else wr.matrix(slot(control,x),x)
                             },
        "use.known.rec"       ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## use input recruitment estimate (option use.known.rec)\n",
                                    "#   0=estimate all recruitments\n",
                                    "#   1=yes use input recruitment from file known_recruitment.in\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },

        "beta.cor"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## adjustment factor to bring the beta parameter close to one (option beta.cor)\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
                             
        "SSB.R.year.first"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## year range for data included to fit the R-SSB relation (option SSB.R.year.range)\n",
                                    "# first (option SSB.R.year.first) and last (option SSB.R.year.last) year to consider.\n",
                                    "# the value -1 indicates the use of the first (and last) available year in time series\n",
                                    "# first year by species\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "SSB.R.year.last"  ={if (nice) {
                                cat("# last year by species\n",
                                    file=file,append=T,sep="")
                                    if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
          
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "obj.func.weight"   ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Objective function weighting by species (option objective.function.weight)\n",
                                    "# first=catch observations,\n",
                                    "# second=CPUE observations,\n",
                                    "# third=SSB/R relations\n",
                                    "# fourth=stomach observations, weight proportions \n",
                                    "# fifth=stomach observations, number at length \n",
                                    "##\n",
                                    file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),paste(1:length(sp.names),sp.names))
                                } else wr.matrix(slot(control,x),x)
                             },
        "phase.rec"         ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## parameter estimation phases for single species parameters\n",
                                    "# phase.rec (stock numbers, first age) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "phase.rec.older"   ={if (nice) {
                                cat("# phase.rec.older (stock numbers, first year and all ages) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "phase.F.y"         ={if (nice) {
                                cat("# phase.F.y (year effect in F model) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "phase.F.y.spline"  ={if (nice) {
                                cat("# phase.F.y.spline (year effect in F model, implemented as spline function)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },

        "phase.F.q"         ={if (nice) {
                                cat("# phase.F.q (season effect in F model) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "phase.F.a"         ={if (nice) {
                                cat("# phase.F.a (age effect in F model) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "phase.catchability"={if (nice) {
                                cat("# phase.catchability (survey catchability) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "phase.SSB.R.alfa"  ={if (nice) {
                                cat("# phase.SSB.R.alfa (alfa parameter in SSB-recruitment relation) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "phase.SSB.R.beta"  ={if (nice) {
                                cat("# phase.SSB.R.beta (beta parameter in SSB-recruitment relation) (default=1)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "min.catch.CV"      ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## minimum CV of catch observation used in ML-estimation (option min.catch.CV)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "min.SR.CV"         ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## minimum CV of catch SSB-recruitment relation used in ML-estimation (option min.SR.CV)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "discard"           ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Use proportion landed information in calculation of yield (option calc.discard)\n",
                                    "#    0=all catches are included in yield\n",
                                    "#    1=yield is calculated from proportion landed (file proportion_landed.in)\n",
                                     file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },

        "combined.catches"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## use seasonal or annual catches in the objective function (option combined.catches)\n",
                                    "# do not change this options from default=0, without looking in the manual\n",
                                    "#    0=annual catches with annual time steps or seasonal catches with seasonal time steps\n",
                                    "#    1=annual catches with seasonal time steps, read seasonal relative F from file F_q_ini.in (default=0)\n",
                                      file=file,append=T,sep="")   
                                      if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                                } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },                                                                                       
        "seasonal.catch.s2" ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## use seasonal or common combined variances for catch observation\n",
                                    "# seasonal=0, common=1 (use 1 for annual data)\n", 
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },                      
        "catch.s2.group" ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## \n",file=file,append=T)
                                    if (expand) wr.list.expand(slot(control,x),"catch observations: number of separate catch variance groups by species",
                                             "first age group in each catch variance group",VPA.species)
                                    if (!expand) wr.list.nice(slot(control,x),"catch observations: number of separate catch variance groups by species",
                                                              "first age group in each catch variance group",VPA.species)
                              } else  wr.list(slot(control,x),"n.catch.s2.group",x)
                             },
        "catch.season.age"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## \n",file=file,append=T)
                                if (expand) wr.list.expand(slot(control,x),"catch observations: number of separate catch seasonal component groups by species",
                                             "first ages in each seasonal component group by species",VPA.species)                                

                                 if (!expand) wr.list.nice(slot(control,x),"catch observations: number of separate catch seasonal component groups by species",
                                             "first ages in each seasonal component group by species",VPA.species)
                              } else  wr.list(slot(control,x),"n.catch.s2.group",x)
                             },
        "avg.F.ages"        ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## first and last age in calculation of average F by species (option avg.F.ages)\n",
                                    file=file,append=T)
                                wr.matrix.nice(slot(control,x),VPA.species)
                               } else wr.matrix(slot(control,x),x)
                             },
        "min.catch"         = {if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## minimum 'observed' catch, (option min.catch). You cannot log zero catch at age!\n",
                                    "#\n",
                                    "# 0 ignore observation in likelihood\n#\n",
                                    "# negative value gives percentage (e.g. -10 ~ 10%) of average catch in age-group for input catch=0\n",
                                    "# negative value less than -100 substitute all catches by the option/100 /100 *average catch in the age group for catches less than (average catch*-option/10000\n",
                                    "#\n",
                                    "# if option>0 then will zero catches be replaced by catch=option\n",
                                    "#\n",
                                    "# else if option<0 and option >-100 and catch=0 then catches will be replaced by catch=average(catch at age)*(-option)/100\n",
                                    "# else if option<-100  and catch < average(catch at age)*(-option)/10000 then catches will be replaced by catch=average(catch at age)*(-option)/10000\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "catch.sep.year"    ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## \n",file=file,append=T)
                                if (expand) wr.list.expand(slot(control,x),"catch observations: number of year groups with the same age and seasonal selection","first year in each group (please note #1 will always be changed to first model year)",VPA.species)
                                if (!expand) wr.list.nice(slot(control,x),"catch observations: number of year groups with the same age and seasonal selection","first year in each group (please note #1 will always be changed to first model year)",VPA.species)
                                } else  wr.list(slot(control,x),"catch.sep.year",x)
                             },
        "catch.spline.year"    ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## \n",file=file,append=T)
                                    if (expand) wr.list.expand(slot(control,x),"number of nodes for year effect Fishing mortality spline\n# 1=no spline (use one Fy for each year), >1 number of nodes","first year in each group",VPA.species)
                                    if (!expand) wr.list.nice(slot(control,x),"number of nodes for year effect Fishing mortality spline\n# 1=no spline (use one Fy for each year), >1 number of nodes","first year in each group",VPA.species)
                                } else  wr.list(slot(control,x),"catch.spline.year",x)
                             },
        "zero.catch.year.season"={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## year season combinations with zero catch (F=0) (option zero.catch.year.season)\n",
                                    "# 0=no, all year-seasons have catchs,\n",
                                    "# 1=yes there are year-season combinations with no catch.\n",
                                    "#   Read from file zero_catch_seasons_ages.in\n",
                                    "# default=0\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "zero.catch.season.age"={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## season age combinations with zero catch (F=0) (option zero.catch.season.ages)\n",
                                    "# 0=no, all seasons have catchs,\n",
                                    "# 1=yes there are seasons with no catch. Read from file zero_catch_season_ages.in\n",
                                    "# default=0\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "fix.F.factor"      ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Factor for fixing last season effect in F-model (default=1) (fix.F.factor))\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "est.calc.sigma"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Uncertainties for catch, CPUE and SSB-R observations (option calc.est.sigma)\n",
                                   "#  values: 0=estimate sigma as a parameter (the right way of doing it)\n",
                                   "#          1=Calculate sigma and truncate if lower limit is reached \n",
                                   "#          2=Calculate sigma and use a penalty function to avoid lower limit \n",
                                   "#  catch-observation, CPUE-obs, Stock/recruit\n",
                                    file=file,append=T,sep="")
                                cat(formatC(slot(control,x),width=12),"\n",file=file,append=T,sep=" ")
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "read.HCR"          = {if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Read HCR_option file (option=read.HCR) default=0 \n",
                                    "#  0=no  1=yes\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                               } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                               if (!write.multi) break
                              },                             
        "incl.stom.all"     = {if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#\n#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                                    "# multispecies parameters\n#\n",
                                    "#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&\n#\n",
                                    file=file,append=TRUE,sep='')
                                cat("# Exclude year, season and predator combinations where stomach data are not incl.(option incl.stom.all)\n",
                                    "#   0=no, all stomach data are used in likelihood\n",
                                    "#   1=yes there are combinations for which data are not included in the likelihood.\n",
                                    "#      Read from file: incl_stom.in\n",
                                    "#   default(0)\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                               } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                               },                                
        "use.Nbar"        = { if (nice)  {                              
                                cat(sepLine,file=file,append=TRUE) 
                                cat("##  N in the beginning of the period or N bar for calculation of M2 (option use.Nbar)\n",
                                    "#  0=use N in the beginning of the time step (default)\n",
                                    "#  1=use N bar\n",
                                    slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },      
        "M2.iterations"   = {if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## Maximum M2 iterations (option M2.iterations) in case of use.Nbar=1\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },      
        "max.M2.sum2"      = {if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## convergence criteria (option max.M2.sum2) in case of use.Nbar=1\n",
                                    "#  use max.M2.sum2=0.0 and M2.iterations=7 (or another high number) to make Hessian\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                            
       "stom.likelihood"   = {if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## likelihood model for stomach content observations (option stom.likelihood)\n",
                                "#  1 =likelihood from prey weight proportions only (see option below)\n",
                                "#  2 =likelihood from prey weight proportions and from prey numbers to estimate size selection\n",
                                "#  3 =Gamma distribution for prey absolute weight and size selection from prey numbers\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },  
      "stomach.variance"   = {if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("# Variance used in likelihood model for stomach contents as prey weight proportion (option stomach.variance)\n",
                                "#  0 =not relevant, \n",
                                "#  1 =log normal distribution, \n",
                                "#  2 =normal distribution,\n",
                                "#  3 =Dirichlet distribution\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                             
                                           
        "simple.ALK"         = {if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## Usage of age-length-keys for calc of M2 (option simple.ALK))\n",
                                    "#  0=Use only one size group per age (file lsea.in or west.in)\n",
                                    "#  1=Use size distribution per age (file ALK_all.in)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },   
       "consum"            = {if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## Usage of food-rations from input values or from size and regression parameters (option consum)\n",
                                    "#  0=Use input values by age (file consum.in)\n",
                                    "#  1=use weight at age (file west.in) and regression parameters (file consum_ab.in)\n",
                                    "#  2=use length at age (file lsea.in), l-w relation and regression parameters (file consum_ab.in)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                        
      "size.select.model"   ={if (nice)  {
                                cat(sepLine,file=file,append=TRUE)
                                cat("## Size selection model based on (option size.select.model)\n",
                                    "#  1=length:\n",
                                    "#      M2 calculation:\n",
                                    "#         Size preference:\n",
                                    "#           Predator length at age from file: lsea.in\n",
                                    "#           Prey     length at age from file: lsea.in\n",
                                    "#         Prey mean weight is weight in the sea from file: west.in\n",
                                    "#      Likelihood:\n",
                                    "#         Size preference:\n",
                                    "#           Predator mean length per length group (file: stom_pred_length_at_sizecl.in) \n",
                                    "#           Prey mean length per ength group (file stomlen_at_length.in \n",
                                    "#         Prey mean weight from mean weight per prey length group (file: stomweight_at_length.in \n",
                                    "#  2=weight:\n",
                                    "#      M2 calculation:\n",
                                    "#         Size preference:\n",
                                    "#           Predator weight at age from file: west.in\n",
                                    "#           Prey     weight at age from file: west.in\n",
                                    "#         Prey mean weight is weight in the sea from file: west.in\n",
                                    "#      Likelihood:\n",
                                    "#         Size preference\n",
                                    "#           Predator mean weight is based on mean length per predator length group (file: stom_pred_length_at_sizecl.in)\n",
                                    "#              and l-w relation (file: length_weight_relations.in), \n",
                                    "#           Prey mean weight per prey length group (file: stomweight_at_length.in) \n",
                                    "#         Prey mean weight from mean weight per prey length group (file: stomweight_at_length.in \n",file=file,append=T,sep="")
                                cat("#  3=weight:\n",
                                    "#       M2 calculation: Same as option 2\n",
                                    "#       Likelihood:\n",
                                    "#         Size preference:\n",
                                    "#           Predator mean weight is based on mean length per predator length group (file: stom_pred_length_at_sizecl.in)\n",
                                    "#              and l-w relation (file: length_weight_relations.in), \n",
                                    "#           Prey mean weight per prey length group (file: stomlen_at_length.in) and l-w relation (file:length_weight_relations.in)\n",
                                    "#         Prey mean weight from prey mean length per prey length group (file: stomlen_at_length.in) and l-w relation (file: length_weight_relations.in) \n",
                                    "#  4=weight:\n",
                                    "#       M2 calculation:\n",
                                    "#         Size preference:\n",
                                    "#           Predator mean weight from file lsea.in (length in the sea) and l-w relation (file: length_weight_relations.in) \n",
                                    "#           Prey mean weight from file lsea.in (length in the sea) and l-w relation (file: length_weight_relations.in) \n",
                                    "#       Likelihood:  Same as option 3\n",
                                    "#  5=weight in combination with simple.ALK=1:\n",
                                    "#       M2 calculation:\n",
                                    "#         Size preference:\n",
                                    "#           Predator weight based on length from file ALK_all.in (length distribution at age) and l-w relation (file: length_weight_relations.in) \n",
                                    "#           Prey     weight based on length from file ALK_all.in (length distribution at age) and l-w relation (file: length_weight_relations.in) \n",
                                    "#         Prey mean weight based on length from file ALK_all.in (length distribution at age) and l-w relation (file: length_weight_relations.in) \n",
                                    "#       Likelihood: Same as for option 2\n",
                                    "#  6=weight in combination with simple.ALK=1:\n",
                                    "#       M2 calculation: Same as option 5\n",
                                    "#       Likelihood: Same as option 3\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              }
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },                                            
         "L50.mesh"           ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Adjust Length at Age distribution by a mesh selection function (option L50.mesh)\n",
                                    "#  Please note that options simple.ALK shoud be 1 and option size.select.model should be 5\n",
                                    "# L50 (mm) is optional given as input. Selection Range is estimated by the model\n",
                                    "# L50= -1 do not adjust\n",
                                    "# L50=0, estimate L50 and selection range\n",
                                    "# L50>0, input L50 (mm) and estimate selection range\n",
                                    "# by VPA species\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),VPA.species) else wr.vector.nice(slot(control,x),VPA.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },                  
     "size.selection"        ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                 cat("## spread of size selection (option size.selection)\n",
                                    "#   0=no size selection, predator/preys size range defined from observations\n",
                                    "#   1=normal distribution size selection\n",
                                    "#   3=Gamma distribution size distribution\n",
                                    "#   4=no size selection, but range defined by input min and max regression parameters (file pred_prey_size_range_param.in)\n",
                                    "#   5=Beta distributed size distribution, within observed size range\n",
                                    "#   6=log-Beta size distributed, within observed size range\n",
                                    "#\n",
                                    "# by predator\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             }, 
       "sum.stom.like"     ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## sum stomach contents over prey size for use in likelihood for prey weight proportions (option sum.stom.like)\n",
                                    "#   0=no, use observations as they are; 1=yes, sum observed and predicted stomach contents before used in likelihood for prey weight proportions\n",
                                    "#\n",
                                    "# by predator\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             }, 
        "stom.obs.var"     ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## # Use estimated scaling factor to link number of observation to variance for stomach observation likelihood (option stom_obs_var)\n",
                                    "#    0=no, do not estiamte factor (assumed=1);  1=yes, estimate the factor;  2=equal weight (1) for all samples\n",
                                    "#\n",
                                    "# by predator\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             }, 
        "stom.max.sumP"     ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## # Upper limit for Dirichlet sumP. A low value (e.g. 10) limits the risk of overfitting. A high value (e.g. 100) allows a full fit. (option stom_max_sumP)\n",
                                    "# by predator\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             }, 
        "var.scale.stom"    ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Scaling factor (to bring parameters close to one) for relation between no of stomachs sampling and variance\n",
                                    "#  value=0: use default values i.e. 1.00 for no size selection and otherwise 0.1 (option var.scale.stom)\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },

      "size.other.food.suit"={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## other food suitability size dependency  (option size.other.food.suit)\n",
                                    "#  0=no size dependency\n",
                                    "#  1=yes, other food suitability is different for different size classes\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "min.stom.cont"     ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Minimum observed relative stomach contents weight for inclusion in ML estimation (option min.stom.cont)\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
       "max.stom.sampl"     ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Upper limit for no of samples used for calculation of stomach observation variance (option max.stom.sampl)\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
        "prey.pred.size.fac" ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## Max prey size/ pred size factor for inclusion in M2 calc (option max.prey.pred.size.fac)\n",
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },                                 
        "stom.type.include"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("## inclusion of individual stomach contents observations in ML for weight proportions (option stom.type.include)\n",
                                    "# 1=Observed data\n",
                                    "# 2= + (not observed) data within the observed size range (=fill in)\n",
                                    "# 3= + (not observed) data outside an observed size range. One obs below and one above (=tails)\n",
                                    "# 4= + (not observed) data for the full size range of a prey species irrespective of predator size (=expansion)\n", 
                                    file=file,append=T,sep="")
                                if (expand) wr.vector.expand(slot(control,x),pred.species) else wr.vector.nice(slot(control,x),pred.species)
                                
                              } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                             },            
                            
        "use.overlap"       ={if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## use overlap input values by year and season (use.overlap)\n",
                                    "#   0: overlap assumed constant or estimated within the model \n",
                                    "#   1: overlap index from file overlap.in (assessment only, use overlap from last year in forecast)\n",
                                    "#   2: overlap index from file overlap.in (assessment and forecast)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                              
        "phase.vulnera"     ={if (nice)  {                               
                                cat(sepLine,file=file,append=TRUE) 
                                cat("## parameter estimation phases for predation parameters\n",
                                    "#  the number gives the phase, -1 means no estimation\n#\n",
                                    "#  vulnerability (default=2) (phase phase.vulnera)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                               
   "phase.other.suit.slope"  ={if (nice)  {                                
                                cat("# other food suitability slope (default=-1) (option phase.other.suit.slope)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                          
  "phase.pref.size.ratio"  ={if (nice)  {                                
                                cat("# prefered size ratio (default=2) (option phase.pref.size.ratio)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                          
  "phase.pref.size.ratio.correction"={if (nice)  {                                
                                cat("# predator size ratio adjustment factor (default=-1) (option phase.pref.size.ratio.correction))\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                 
   "phase.prey.size.adjustment" ={if (nice)  {                                
                                cat("# prey species size adjustment factor (default=-1) (option phase.prey.size.adjustment)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                      
  "phase.var.size.ratio" =  {if (nice)  {                                
                                cat("# variance of prefered size ratio (default=2) (option phase.var.size.ratio)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                              
  "phase.season.overlap"   ={if (nice)  {                                
                                cat("# season overlap (default=-1) (option phase.season.overlap)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                             
       "phase.stom.var"     ={if (nice)  {                                
                                cat("# Stomach variance parameter (default=2) (option phase.Stom.var)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },  
     "phase.mesh.adjust"     ={if (nice)  {                                
                                cat("# Mesh size selection of stomach age length key (default=-1) (option phase.mesh.adjust)\n",
                                slot(control,x),"\n",file=file,append=T,sep="")
                                cat(sepLine,file=file,append=T)
                              } 
                              else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE) 
                             },                                                                                                               
        #other wise                       
                              # cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
   )  #end switch
  }
  
  if (writeSpNames) {
      sp.names<-slot(control,"species.names")
      sp.names<-substr(paste(sp.names,"___________",sep=''),1,11)
      sp.names<-gsub(' ','_',sp.names)
      write(sp.names,file=file.path(path,"species_names.in"))
      cat("12345678901\nPlease note. exactly 11 charaters for species names !!!!\n",file=file.path(path,"species_names.in"),append=TRUE) 
  }
  setwd(old.path)
}

read.FLSMS.control<-function(dir='.',file="sms.dat",test=FALSE) {       
  #print(file.path(dir,file))
  opt<-scan(file=file.path(dir,file), comment.char = "#",quiet=T) 
   
  n<-1
  control<-new("FLSMS.control")
  n.<-slotNames(control)
  for (x in n.) {
    switch(x,
       "no.species"          = {slot(control,x)<-as.integer(opt[n]); nsp<-as.integer(opt[n]); n<-n+1;
                                    species.names<-readLines(file.path(dir,"species_names.in"), n=control@no.species)
                                    species.names<-gsub('_',' ',species.names)
                                    species.names<-sub('[[:space:]]+$', '', species.names)
                                },
       "species.names"       = { slot(control,x)<-species.names; },
  
       "species.info"        = {    ncols<-11
                                    tmp<-matrix(opt[n:(n-1+nsp*ncols)],ncol=ncols,nrow=nsp,byrow=TRUE,
                                         dimnames<-list(species.names,c("last-age","first-age F>0","last-age-selec","effort",
                                                "last-age-likelihood","+group","predator","prey","SSB/R","SpawningQ","RecAdd2")));
                                   slot(control,x)<-tmp;
                                   n<-n+nsp*ncols;
                                   n.prey<-0; n.pred<-0; n.oth.pred<-0
                                   for (s in (1:nsp)) { 
                                     if(tmp[s,7]>=1) n.pred<-n.pred+1;
                                     if(tmp[s,7]==2) n.oth.pred<-n.oth.pred+1;
                                   }
                                   first.VPA<-n.oth.pred+1
                                   n.VPA.sp<-nsp-first.VPA+1
                                },       
                                
        "beta.cor"            = {slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
        "SSB.R.year.first"    ={slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
        "SSB.R.year.last"     ={slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
        "obj.func.weight"     = {slot(control,x)<-matrix(opt[n:(n-1+nsp*5)],ncol=5,nrow=nsp,byrow=TRUE,
                                  dimnames=list(species.names,c("catch","survey","SSB/R","stomach1","stomach2"))); n<-n+nsp*5},  
        "min.catch.CV"        = {slot(control,x)<-as.numeric(opt[n]); n<-n+1},
        "min.SR.CV"           = {slot(control,x)<-as.numeric(opt[n]); n<-n+1},
        "discard"             ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp},
        "combined.catches"    ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp},
        "seasonal.catch.s2"   = {slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp},
        "catch.s2.group"      = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
                                 group<-vector("list", length=n.VPA.sp); 
                                 for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; } 
                                 slot(control,x)<-group},
        "catch.season.age"    = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
                                 group<-vector("list", length=n.VPA.sp); 
                                 for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; } 
                                 slot(control,x)<-group },                                 
        "avg.F.ages"          = {slot(control,x)<-matrix(opt[n:(n-1+n.VPA.sp*2)],ncol=2,nrow=n.VPA.sp,byrow=TRUE,
                                  dimnames=list(species.names[first.VPA:nsp],c("first-age","last-age"))); n<-n+n.VPA.sp*2},
        "min.catch"           = {slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
        "catch.sep.year"      = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
                                 group<-vector("list", length=n.VPA.sp); 
                                 for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; } 
                                 slot(control,x)<-group },
       "catch.spline.year"    = {v<-as.vector(as.integer(opt[n:(n-1+n.VPA.sp)])); n<-n+n.VPA.sp;
                                 group<-vector("list", length=n.VPA.sp);
                                 for (j in 1:n.VPA.sp) {group[[j]]<-as.integer(opt[n:(n-1+v[j])]); n<-n+v[j]; }
                                 slot(control,x)<-group },
        "fix.F.factor"        = {slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},

        "est.calc.sigma"      = {slot(control,x)<-as.vector(opt[n:(n-1+3)]); n<-n+3},
        "L50.mesh"            ={slot(control,x)<-as.vector(opt[n:(n-1+n.VPA.sp)]); n<-n+n.VPA.sp},
        "size.selection"      ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
        "sum.stom.like"       ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
        "stom.obs.var"        ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
        "stom.max.sumP"        ={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
        "var.scale.stom"      ={slot(control,x)<-as.vector((opt[n:(n-1+n.pred)])); n<-n+n.pred},
        "size.other.food.suit"={slot(control,x)<-as.vector(as.integer(opt[n:(n-1+n.pred)])); n<-n+n.pred},
        "min.stom.cont"       ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},
        "max.stom.sampl"       ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},
        "prey.pred.size.fac"  ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},
        "stom.type.include"   ={slot(control,x)<-as.vector(opt[n:(n-1+n.pred)]); n<-n+n.pred},                          
         # otherwise                        
                                 {slot(control,x)<-opt[n];n<-n+1}                                 
      )
    if (test) {
      cat(x,'\n')
      #cat(class(slot(control,x)),'\n')
      if (class(slot(control,x))!='list') cat(slot(control,x),'\n') else print(slot(control,x))
    }
  }
  control
}



## show (a replacement for print of S3 classes)
setMethod("show", signature(object="FLSMS.control"),
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
