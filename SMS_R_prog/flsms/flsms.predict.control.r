
### class ######################################################################

validFLSMS.predict.control <- function(object){
     if (object@no.MCMC.iterations <1)
        return("value of no.MCMC.iterations must be >=1")
 

    # UPDATA but so far everything is fine
    return(TRUE)
}

setClass("FLSMS.predict.control",
representation(
    last.prediction.year    ="numeric",
    no.MCMC.iterations      ="numeric",
    years.wsea              ="matrix",
    years.weca              ="matrix",
    years.prop.landed       ="matrix",
    years.propmat           ="matrix",
    growth.model            ="matrix",
    last.season.adj.factor  ="matrix",
    stochastic.recruitment  ="matrix",
    rec.noise               ="matrix",
    rec.noise.input         ="matrix",
    HCR                     ="matrix",
    Trigger                 ="matrix",
    Trigger.a.b             ="matrix",
    HCR.F.TAC               ="matrix",   
    constant.F              ="matrix",
    constant.TAC            ="matrix",
    target.SSB              ="matrix",
    real.time.F             ="matrix",
    TAC.cap                 ="matrix",
    TAC.min                 ="matrix",
    F.cap                   ="matrix",
    F.constraints           ="matrix",
    TAC.constraints         ="matrix",
    SSB.constraints         ="matrix",
    obs.noise               ="matrix",
    real.time               ="matrix",
    survey                  ="matrix",
    assessment              ="matrix",
    implementation          ="matrix",
    intermediate.TAC        ="matrix",
    intermediate.F          ="matrix",
    inter.year              ="matrix",
    inter.F.TAC             ="matrix",
    rec.autocor             ="matrix",
    recruit.adjust          ="matrix",
    recruit.adjust.CV       ="matrix",
    read.rec.SSB.parm       ="numeric",
    age.group.output        ="matrix",
    read.stock.N            ="matrix",
    no.ini.pop              ="numeric",
    read.F                  ="matrix",
    use.read.F              ="numeric",
    other.predator          ="matrix"
  )
  ,
  prototype=prototype(
    last.prediction.year    =as.numeric(0),
    no.MCMC.iterations      =as.numeric(1),
    years.wsea              =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))), 
    years.weca              =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
    years.prop.landed       =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
    years.propmat           =matrix(0,ncol=1,nrow=2,dimnames=list(c("first-year","last-year"),c("sp1"))),
    growth.model            =matrix(0,ncol=1,nrow=5,dimnames=list(c("Type","first.age","last.age","add.info1","add.info2"),c("sp1"))),
    last.season.adj.factor  =matrix(1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    stochastic.recruitment  =matrix(1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    rec.noise               =matrix(rep(c(-10,10),1),ncol=1,nrow=2,dimnames=list(c("lower","upper"),c("sp1"))),
    rec.noise.input         =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    HCR                     =matrix(1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    Trigger                 =matrix(0,ncol=1,nrow=2,dimnames=list(c("T1","T2"),c("sp1"))),
    Trigger.a.b             =matrix(0,ncol=1,nrow=6,dimnames=list(c("FT1a","FT1b","FT12a","FT12b","FT2a","FT2b"),c("sp1"))),
    HCR.F.TAC               =matrix(1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),   
    constant.F              =matrix(-1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    constant.TAC            =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    target.SSB              =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    real.time.F             =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    TAC.cap                 =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    TAC.min                 =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    F.cap                   =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    F.constraints           =matrix(0,ncol=1,nrow=2,dimnames=list(c("min","max"),c("sp1"))),
    TAC.constraints         =matrix(0,ncol=1,nrow=2,dimnames=list(c("min","max"),c("sp1"))),
    SSB.constraints         =matrix(0,ncol=1,nrow=2,dimnames=list(c("min","max"),c("sp1"))),
    obs.noise               =matrix(rep(c(-10,10),1),ncol=1,nrow=2,dimnames=list(c("lower","upper"),c("sp1"))),
    real.time               =matrix(rep(c(-1,0,0,0),1),ncol=1,nrow=4,dimnames=list(c("dist","bias",'std',"same"),c("sp1"))),
    survey                  =matrix(rep(c(-1,0,0,0),1),ncol=1,nrow=4,dimnames=list(c("dist","bias",'std',"same"),c("sp1"))),
    assessment              =matrix(rep(c(-1,0,0,0),1),,ncol=1,nrow=4,dimnames=list(c("dist","bias",'std',"same"),c("sp1"))),
    implementation          =matrix(rep(c(-1,0,0,0),1),ncol=1,nrow=4,dimnames=list(c("dist","bias",'std',"same"),c("sp1"))),
    intermediate.TAC        =matrix(-1,ncol=1,nrow=2,dimnames=list(c("first","second"),c("sp1"))),
    intermediate.F          =matrix(1,ncol=1,nrow=2,dimnames=list(c("first","second"),c("sp1"))),
    inter.year              =matrix(2,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    inter.F.TAC             =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    rec.autocor             =matrix(0,ncol=1,nrow=2,dimnames=list(c("years","terms"),c("sp1"))),
    recruit.adjust          =matrix(1,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    recruit.adjust.CV       =matrix(0,ncol=1,nrow=1,dimnames=list(" ",c("sp1"))),
    read.rec.SSB.parm       =as.numeric(0),
    age.group.output        =matrix(0,ncol=8,nrow=1,dimnames=list(c(""),c("F","M2","Z","N","C","FLR","constraints","closure"))),
    read.stock.N            =matrix(0,ncol=2,nrow=1,dimnames=list(c(""),c("first","second"))),
    no.ini.pop              =as.numeric(1),
    read.F                  =matrix(0,ncol=2,nrow=1,dimnames=list(c(""),c("first","second"))),
    use.read.F              =as.numeric(0),
    other.predator          =matrix(-1,ncol=1,nrow=3,dimnames=list(c("factor","first","second"),c("sp1")))
    )                                                           
   ,
    validity=validFLSMS.predict.control
)

setValidity("FLSMS.predict.control", validFLSMS.predict.control)

### End class ###########################################################

  

### Methods #############################################################

FLSMS.predict.control <- function( 
        first.prediction.year=1900, 
        last.prediction.year=1900,
        no.species=1,
        no.other.predators=0,
        species.names=c("sp1"))
{
  if (no.species == 1) {
    no.VPA.sp<-no.species
    no.other.predators<-0
  }
  if (no.species>1) {
    if (no.other.predators>=no.species) stop("no.other.predators cannot be larger than no.species")
    no.VPA.sp<-no.species-no.other.predators
    if (length(species.names )!= no.species) stop("Number of species names is diffrent from no.species")
    
  }
  
 #if (species.names[1] != c("sp1") & length(species.names)!=no.species) 
 #           stop("no.species is diffrent from number of species names")
 sp.names<-species.names[(no.other.predators+1):no.species]
 if (no.other.predators==0) oth.sp.names<-NULL else oth.sp.names<-species.names[1:no.other.predators]


 res <- new("FLSMS.predict.control",
    last.prediction.year    =last.prediction.year,
    no.MCMC.iterations      =1,
    years.wsea              =matrix(rep(first.prediction.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),sp.names)), 
    years.weca              =matrix(rep(first.prediction.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),sp.names)),
    years.prop.landed       =matrix(rep(first.prediction.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),sp.names)),
    years.propmat           =matrix(rep(first.prediction.year-1,2*no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("first-year","last-year"),sp.names)),
    growth.model            =matrix(0,ncol=no.VPA.sp,nrow=5,dimnames=list(c("Type","first.age","last.age","add.info1","add.info2"),sp.names)),
    last.season.adj.factor  =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    stochastic.recruitment  =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    rec.noise               =matrix(rep(c(-10,10),no.VPA.sp),ncol=no.VPA.sp,nrow=2,dimnames=list(c("lower","upper"),sp.names)),
    rec.noise.input         =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    HCR                     =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    Trigger                 =matrix(0,ncol=no.VPA.sp,nrow=2,dimnames=list(c("T1","T2"),sp.names)),
    Trigger.a.b             =matrix(0,ncol=no.VPA.sp,nrow=6,dimnames=list(c("FT1a","FT1b","FT12a","FT12b","FT2a","FT2b"),sp.names)),
    HCR.F.TAC               =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),   
    constant.F              =matrix(-1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    constant.TAC            =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    target.SSB              =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    real.time.F             =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    F.cap                   =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    TAC.cap                 =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    TAC.min                 =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    F.constraints           =matrix(0,ncol=no.VPA.sp,nrow=2,dimnames=list(c("min","max"),sp.names)),
    TAC.constraints         =matrix(0,ncol=no.VPA.sp,nrow=2,dimnames=list(c("min","max"),sp.names)),
    SSB.constraints         =matrix(0,ncol=no.VPA.sp,nrow=2,dimnames=list(c("min","max"),sp.names)),
    obs.noise               =matrix(rep(c(-10,10),1),ncol=no.VPA.sp,nrow=2,dimnames=list(c("lower","upper"),sp.names)),
    real.time               =matrix(rep(c(-1,0,0,0),1),ncol=no.VPA.sp,nrow=4,dimnames=list(c("dist","bias",'std',"same"),sp.names)),
    survey                  =matrix(rep(c(-1,0,0,0),1),ncol=no.VPA.sp,nrow=4,dimnames=list(c("dist","bias",'std',"same"),sp.names)),
    assessment              =matrix(rep(c(-1,0,0,0),1),ncol=no.VPA.sp,nrow=4,dimnames=list(c("dist","bias",'std',"same"),sp.names)),
    implementation          =matrix(rep(c(-1,0,0,0),1),ncol=no.VPA.sp,nrow=4,dimnames=list(c("dist","bias",'std',"same"),sp.names)),
    intermediate.TAC        =matrix(-1,ncol=no.VPA.sp,nrow=2,dimnames=list(c("first","second"),sp.names)),
    intermediate.F          =matrix(1,ncol=no.VPA.sp,nrow=2,dimnames=list(c("first","second"),sp.names)),
    inter.year              =matrix(2,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    inter.F.TAC             =matrix(0,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",sp.names)),
    rec.autocor             =matrix(0,ncol=no.VPA.sp,nrow=2,dimnames=list(c("years","terms"),sp.names)),
    recruit.adjust          =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",c(sp.names))),
    recruit.adjust.CV       =matrix(1,ncol=no.VPA.sp,nrow=1,dimnames=list(" ",c(sp.names))),
    read.rec.SSB.parm       =as.numeric(0),
    age.group.output        =matrix(0,ncol=8,nrow=1,dimnames=list(c(""),c("F","M2","Z","N","C","FLR","constraints","closure"))),
    read.stock.N            =matrix(0,ncol=2,nrow=1,dimnames=list(c(""),c("first","second"))),
    no.ini.pop              =as.numeric(1),
    read.F                  =matrix(0,ncol=2,nrow=1,dimnames=list(c(""),c("first","second"))),
    use.read.F              =as.numeric(0),
    other.predator          =matrix(rep(c(1,-1,-1),no.other.predators),ncol=no.other.predators,nrow=3,dimnames=list(c("factor","first","second"),oth.sp.names))
  )

    # Verify that this object is valid
    test <- validObject(res)

    if (!test) stop("Invalid object:", test)
  return(res)
}





read.FLSMS.predict.control<-function(control=NULL,file) { 
  if (is.null(control)){
    stop("You have to supply a FLSMS.control object");
  }
  n.sp<-control@no.species 
  info<-control@species.info
  n.pred<-0; n.oth<-0
  for (s in (1:n.sp)) { 
        if(info[s,'predator']>=1) n.pred<-n.pred+1;
        if(info[s,'predator']==2) n.oth<-n.oth+1;
  }
  first.VPA<-n.oth+1
  n.VPA<-n.sp-first.VPA+1
  sp.names<-control@species.names
  VPA.sp.names<-sp.names[first.VPA:n.sp]

  opt<-scan(file, comment.char = "#",quiet=TRUE) 
  n<-1
  cl<-new("FLSMS.predict.control")
  n.<-slotNames(cl)
  for (x in n.) {
    switch(x,
     "years.wsea"              = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","second"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.weca"              = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","second"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "years.prop.landed"       = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","second"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
    "years.propmat"            = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","second"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},    
     "growth.model"            = {slot(cl,x)<-matrix(opt[n:(n-1+5*n.VPA)],ncol=n.VPA,nrow=5,dimnames=list(c("Type","first.age","last.age","add.info1","add.info2"),VPA.sp.names),byrow=TRUE); n<-n+5*n.VPA},
     "last.season.adj.factor"  = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "stochastic.recruitment"  = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "rec.noise"               = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("lower","upper"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "rec.noise.input"         = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "HCR"                     = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "Trigger"                 = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("T1","T2"),VPA.sp.names)); n<-n+2*n.VPA},
     "Trigger.a.b"             = {slot(cl,x)<-matrix(opt[n:(n-1+6*n.VPA)],ncol=n.VPA,nrow=6,byrow=TRUE,
                                   dimnames=list(c("FT1a","FT1b","FT12a","FT12b","FT2a","FT2b"),VPA.sp.names)); n<-n+6*n.VPA},
     "HCR.F.TAC"               = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "constant.F"              = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "constant.TAC"            = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "target.SSB"              = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "real.time.F"             = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "F.cap"                   = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "TAC.cap"                 = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "TAC.min"                 = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "F.constraints"           = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("min","max"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "TAC.constraints"         = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("min","max"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "SSB.constraints"         = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("min","max"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "obs.noise"               = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("lower","upper"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "real.time"               = {slot(cl,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("dist","bias",'std',"same"),VPA.sp.names),byrow=TRUE); n<-n+4*n.VPA},
     "survey"                  = {slot(cl,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("dist","bias",'std',"same"),VPA.sp.names),byrow=TRUE); n<-n+4*n.VPA},
     "assessment"              = {slot(cl,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("dist","bias",'std',"same"),VPA.sp.names),byrow=TRUE); n<-n+4*n.VPA},
     "implementation"          = {slot(cl,x)<-matrix(opt[n:(n-1+4*n.VPA)],ncol=n.VPA,nrow=4,
                                   dimnames=list(c("dist","bias",'std',"same"),VPA.sp.names),byrow=TRUE); n<-n+4*n.VPA},
     "intermediate.TAC"        = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","second"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "intermediate.F"          = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("first","second"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "inter.year"              = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "inter.F.TAC"             = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "rec.autocor"             = {slot(cl,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,
                                   dimnames=list(c("years","terms"),VPA.sp.names),byrow=TRUE); n<-n+2*n.VPA},
     "recruit.adjust"          = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "recruit.adjust.CV"       = {slot(cl,x)<-matrix(opt[n:(n-1+n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(" ",VPA.sp.names)); n<-n+n.VPA},
     "age.group.output"        = {slot(cl,x)<-matrix(opt[n:(n+7)],ncol=8,nrow=1,dimnames=list(c(""),c("F","M2","Z","N","C","FLR","constraints","closure"))); n<-n+8},
     "read.stock.N"            = {slot(cl,x)<-matrix(opt[n:(n-1+2)],ncol=2,nrow=1,
                                   dimnames=list(" ", c("first","second")),byrow=TRUE); n<-n+2},
     "read.F"                  = {slot(cl,x)<-matrix(opt[n:(n-1+2)],ncol=2,nrow=1,
                                   dimnames=list(" ",c("first","second")),byrow=TRUE); n<-n+2},
     "other.predator"          = {if (n.oth>1) {
                                   slot(cl,x)<-matrix(opt[n:(n-1+3*n.oth)],ncol=n.oth,nrow=3,
                                   dimnames=list(c("factor","first","second"),sp.names[1:n.oth]),byrow=TRUE); n<-n+3*n.VPA
                                 }
                                 },

     # otherwise                      
                                 {slot(cl,x)<-(opt[n]);n<-n+1}   
      )
  }
  cl
}



setMethod("show", signature(object="FLSMS.predict.control"),
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
           #for (i in (1:dim(m)[1]))  cat("\t",m[i,],"\n")
           print(m)
         }
       }
    }
)
  write.FLSMS.predict.control<-function(HCR,SMS,file,path=NULL,write.multi=TRUE,nice=TRUE) {

  if (!inherits(HCR, "FLSMS.predict.control"))
        stop(paste("HCR" ,"must be an 'FLSMS.predict.contol' object!"))

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


  old.path<-getwd()
  if (!is.null(path)) setwd(path)
  sepLine<-"########################################\n"

  sp.names<-slot(SMS,"species.names")
  nsp<<-SMS@no.species
  for (ii in (1:nsp)) if (SMS@species.info[ii,'predator']!=2) {first.VPA<-ii; break;} #first VPA  species number
  VPA.species<-sp.names[first.VPA:length(sp.names)]
  for (ii in (1:nsp)) if (SMS@species.info[ii,'predator']==0) {last.pred<-ii-1; break;} #first VPA  species number
  pred.species<-sp.names[1:last.pred]

  
  cat("#  HCR_option.dat option file\n",file=file)
  cat('# the character "#" is used as comment character, such that all text and numbers after #',
      " in a line are skipped by the SMS program\n#\n",file=file, append=TRUE)
  n.<-slotNames(HCR)
  for (x in n.) {
      switch(x,
    "last.prediction.year"  ={ if (nice)  {
                                cat(sepLine,file=file,append=TRUE)
                                cat("# last year in prediction\n",
                                    slot(HCR,x),"\n",file=file,append=T,sep="")
                              }
                              else  cat(slot(HCR,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
    "no.MCMC.iterations"    ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# no of iteration within each MCMC prediction (default=1)\n",
                                     slot(HCR,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(HCR,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
       "years.wsea"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calc of mean weight in the sea\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
      "years.weca"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calc of mean weight in the catch\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
      "years.prop.landed"   ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calc of proportion of the catch landed\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
    "years.propmat"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# first and last year for calc of proportion mature\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
     "growth.model"          ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Growth model:\n",
                                    "#   1  : Type  0=no growth;  1=density dependent\n",
                                    "#   2-3: First and last age for growth\n",
                                    "#   4-5: Additional info1 and info2 \n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
 "last.season.adj.factor"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# assumed F year adjustment factor for 'missing' seasons in the last assessment year (default=1)\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
 "stochastic.recruitment" ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# Stochastic recruitment. 0=No, 1=Yes\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
             "rec.noise"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# truncation of standardised normal distribution used to produce noise on recruitment\n",
                                    "# lower and upper   (values -15.0 and 15.0 give no truncation, -2 and 2 give approximately 95% of the distribution)\n",
                                    "#   values 0 and 0 gives no noise\n",file=file,append=T,sep="")
                                   wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
        "rec.noise.input"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# use recruitment noise from input parameters\n",
                                    "#   0=no, estimate from input R/SSB parameters\n",
                                    "#   1=yes, read residuals from file  recruit_residuals.in\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
        "HCR"               ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#  option HCR :\n",
                                    "#           0=make data for SMS simulations\n",
                                    "#           1=constant F \n",
                                    "#           2=constant TAC\n",
                                    "#          10=  F from trigger T1&T2 and SSB in the beginning of the TAC year\n",
                                    "#          11=TAC from trigger T1&T2 and SSB in the beginning of the TAC year\n",
                                    "#\n",
                                    "#          15=  F from target SSB (target.SSB) in the beginning of the TAC year+1\n",
                                    "#\n",
                                    "#          20=  Norway Pout 2018, Escapement strategies\n",
                                    "#\n",
                                    "#\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
          "Trigger"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option T1.  Trigger 1,  (SSB, TSB, F  or stock N depending on HCR in use)\n",
                                    "# option T2.  Trigger 2,  (SSB, TSB, F  or stock N depending on HCR in use)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
           "Trigger.a.b"  ={if (nice) {                                                                                                                     
                                cat(sepLine,file=file,append=T)
                                cat("# options FT1a and FT1b. a1 and b1: intercept and slope for regression to calc max F or TAC for observed values below T1.\n",
                                    "#           e.g.    F  =a1+b1 * SSB using HCR=10 and SSB in TAC year\n",
                                    "# options FT12a and FT12b. a2 and b2: intercept and slope for regression to calc max F or TAC for observed values above T1 but below T2.\n",
                                    "#           e.g.    F = a2+b2 *(SSB-T1) using HCR=10 and SSB in TAC year\n",
                                    "# options FT2a and FT2b. a3 and b3: intercept and slope for regression to calc max F or TAC for observed values above T2.\n",
                                    "#          e.g.     F  = a3+b3 *(SSB-T2) using HCR=10 and SSB in TAC year\n",
                                    "#  one column gives a1, b1, a2, b2, a3, and b3\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
           "Trigger.a.b"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# options FT1a and FT1b. a1 and b1: \n",
                                    "#   intercept and slope for regression to calc max F or TAC for observed values below T1\n",
                                    "#   e.g.     F  =a1+b1 * SSB using HCR=10 and SSB in TAC year\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
            "HCR.F.TAC"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option HCR.F.TAC. Implement HCR result F as effort (option=0) or use TAC (option=1) to estimate real F\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "constant.F"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# value constant.F. F for constant F scenarios (HCR=1)\n",
                                    "#    A value >0 gives absolute F, a value<0 gives fraction of F status quo, e.g. opiton=-0.5 gives F=0.5*Fsq\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
                "constant.TAC"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# value constant.TAC. TAC for constant TAC scenarios (HCR=2). Values as for constant.F\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "target.SSB"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option target.SSB.  Target SSB, if relevant (for HCR options in 15, 16, 22 or 23)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "real.time.F"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option real.time.F. 	Mean F to obtain real time estimate, or minimum F irrespective of TAC if relevant  \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "TAC.cap"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option cap.TAC	Max TAC(true) irrespective of HCR and constraints (=0 is no max TAC) \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
                "TAC.min"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option min.TAC	Min TAC(true) irrespective of HCR and constraints (=0 is no min TAC) \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },

               "F.cap"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option cap.F 		Maximum F(true) irrespective of HCR and constraints (=0 is no max F)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "F.constraints"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# #### %%%%%%%%%%%%  constraints %%%%%%%%%%%%%%%% ### \n",
                                    "# options min.F.change and max.F.change\n",
                                    "#    min and max variation in F between years calculated as (F_forecast_year/F_forcast_previous_year)\n",
                                    "#    0=no constrains\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "TAC.constraints"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# options  min.yield.change and max.yield.change\n",
                                    "#    min and max variation in YIELD between years calc. as < (YIELD forecast year/YIELD forecast previous year)\n",
                                    "#    0=no constrains\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "SSB.constraints"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# options min.SSB.change and max.SSB.change\n",
                                    " #    min and max variation in SSB between years calc.\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "obs.noise"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%%  'observation and implementation errors'  %%%%%%%%%%%%%%%%%%% ###\n",
                                    "# options obs.noise.lower and obs.noise.upper\n",
                                    "#      truncation of standardised normal distribution used to produce noise on observations\n",
                                    "#      lower and upper   (values -15.0 and 15.0 give no truncation, -2 and 2 give approximately 95% of the distribution\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
               "real.time"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# options real.T.dist and real.T.bias and  real.T.std\n",
                                    "#       real time monitoring stock numbers observation uncertainties: model, mean and std deviation,\n",
                                    "#            distribution model (-1=not used, 0=normal, 1=log normal)\n",
                                    "#            mean (mean is mean bias factor, mean=1 produces no bias)\n",
                                    "#            std dev for log normal dist, or CV for normal dist\n",
                                    "#            same noise. Use same noise for all ages (same noise=1) or independent noise per age(same noise=0)\n",
                                    "#        special for HCR 334 (Normally used for real-time monitoring, see above)\n",
                                    "#        first=annual decrease percentage in F until target has been reached\n",
                                    "#        second= HCR when target has been reached\n",
                                    "#        third= adjustment factor for TAC constraints in rebuilding (init) phase.\n",
                                    "#        i.e. factor=x. Init min.yield.change=0.85 and max.yield.change=1.15 are change to 0.85*x and 1.15/x\n",
                                    "#        fourth=not relevant \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "survey"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# options survey.dist and survey.bias and  survey.std (used for Real Time monitoring options)\n",
                                    "#        survey indices observation uncertainties: model, mean and std deviation,\n",
                                    "#             	distribution model (-1=not used, 0=normal, 1=log normal)\n",
                                    "#             	mean (mean is mean bias factor, mean=1 produces no bias)\n",
                                    "#             	std dev for log normal dist, or CV for normal dist\n",
                                    "#              same noise. Use same noise for all ages (same noise=1) or independent noise per age(same noise=0)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "assessment"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# options asess.error.dist and assess.bias and assess.std\n",
                                    "#      ssessment observation uncertainties (on stock numbers) - model, mean and std deviation\n",
                                    "#      1.distribution model:\n",
                                    "#            1=not used, 1=log normal distributed N,\n",  
                                    "#            2=log normal N using covariance matrix from file covariance_n.in \n",
                                    "#            3=log normal N using decomposed matrix from file decomposition_n.in \n",
                                    "#            4=log normal N & F using covariance matrix from file covariance_nf.in \n",
                                    "#            5=log normal N & F using decomposed matrix from file decomposition_nf.in \n",
                                    "#            6=log normal using std by age from file assessment_CV_age.in, does not work!!!)\n",
                                    "#      2. mean (mean is mean bias factor, mean=1 produces no bias)\n",
                                    "#      3. std dev for log normal dist (distribution model=1)  \n",
                                    "#      4. various input dependent on type of assessment noise \n",
                                    "#            distribution model=1. Use same noise for all ages (same noise=1) or noise per age(same noise=0)\n",
                                    "#            distribution model=2-5. Number of forecasts simulated (1=deterministic, >1 stochastic)\n"
                                    ,file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "implementation"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# options implemt.error.dist and implemt.bias and implemt.std\n",
                                    "#         implementation uncertainties on result of HCR (TAC or F).  model, mean and std deviation,\n",
                                    "#             	distribution model (-1= not used, 0=normal, 1=log normal)\n",
                                    "#             	mean (mean is mean bias factor, mean=1 produces no bias)\n",
                                    "#             	std dev for log normal dist, or CV for normal dist\n",
                                    "#              same noise. dummy value(same value always used) \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "intermediate.TAC"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%% various options and input values for the first two years after the last assessment year  %%%%%%%%%%%%%%%%%%% ###\n",
                                    "# TAC.first.	TAC in the first year after the last assessment year\n",
                                    "#                 -1= not relevant\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "intermediate.F"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# F.first.  F factor for scaling of F staus quo in the first two years after the last assessment year\n",
                                    "#              -1= not relevant \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "inter.year"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option inter.year  	No. of intermediate years (1 | 2)\n",
                                    "#   intermidiat year is the number of years between the last year with known catches and the year for which a TAC is set\n",
                                    "#    example: Sandeel catches for 2006 are known (at the 2006 WGNSSK) and TAC for 2007 has to be produces. Intermediateyear=2007-2006=1\n",
                                    "#         Cod catches for 2005 are known (at the 2006 WGNSSK) and TAC for 2007 has to be produces. Intermediate year=2007-2005=2 \n",file=file,append=T,sep="")
                                      wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "inter.F.TAC"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# option Inter.F.TAC. 	 Implement HCR result in the second intermediate year as F (option=0) or use TAC (option=1) to estimate real F\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "rec.autocor"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%% various for recruitment %%%%%%%%%%%%%%%%%%% ### \n",
                                    "# no of years in recruitment noise autocorrelation and\n",
                                    "#   autocorrelation term(s) use 0 for no terms\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "recruit.adjust"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# factor to adjust expected recruits (adjust.recruits)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
           "recruit.adjust.CV"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# adjust recruitment with half of the variance (factor exp(-(CV^2)/2) option recruit.adjust.CV\n",
                                    "# 0=no adjustment, 1=do adjustment\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),VPA.species)
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "read.rec.SSB.parm"={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# read SSB recruitment parameters from file ssb_r.in\n",
                                    "#  (0=no use estimated parm, 1=yes, read in new parameters)\n",
                                     slot(HCR,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(HCR,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
              "age.group.output"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%% various for output %%%%%%%%%%%%%%%%%%% ###\n",
                                    "#         F          M2           Z           N           C         FLR    constraints    closure\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),"  0=no output, 1=output")
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "read.stock.N"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%% various for predicted N (for FLR operating model) %%%%%%%%%%%%%%%%%%% ###\n",
                                    "# read prediction stock numbers incl. recruits from file predict_stock_n.in\n",
                                    "# first and last year for including stock numbers\n",
                                    "# use years before first prediction year for no inclusion of data= use values from assessment\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x)," first and last year")
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "no.ini.pop"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("# number of initial population (default=1)\n",
                                     slot(HCR,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(HCR,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
             "read.F"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%% various for F and exploitation pattern %%%%%%%%%%%%%%%%%%% ###\n",
                                    "# read F/exploitation pattern from file exploitation_pattern.in\n",
                                    "# first and last year for including  values\n",
                                    "# use years before first prediction year for no inclusion of data, that is to use values from exploitation pattern and HCR\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),"")
                                } else wr.matrix(slot(HCR,x),x)
                             },
              "use.read.F"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#  use values from file exploitation_pattern.in as\n",
                                    "#  0 = absolute F (without applying HCR afterwards) or 1=exploitation pattern\n",
                                     slot(HCR,x),"\n",file=file,append=T,sep="")
                              } else  cat(slot(HCR,x),"\t#",x,"\n",file=file,append=TRUE)
                             },
              "other.predator"  ={if (nice) {
                                cat(sepLine,file=file,append=T)
                                cat("#### %%%%%%%%%%%%% various for other predators %%%%%%%%%%%%%%%%%%% ###\n",
                                    "# annual change factor for population number\n",
                                    "# first year (relative to first prediction year) year of change  (-1 is no change)\n",
                                    "# last year (relative to first prediction year) of change (-1 is no change)\n",
                                    "# other predator stock numbers  \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(HCR,x),sp.names[1:(first.VPA-1)])
                                } else wr.matrix(slot(HCR,x),x)
                             }

    )  #end switch
  }
  setwd(old.path)
}


  if (F) {
      SMS<-read.FLSMS.control(dir=data.path,file='SMS.dat')
      HCR<-read.FLSMS.predict.control(control=SMS,file='HCR_options.dat')
      write.FLSMS.predict.control(HCR=HCR,SMS=SMS,file='a.dat',nice=T)
  } 

