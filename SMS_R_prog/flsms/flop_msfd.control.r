
### class ######################################################################



validFLOP.MSFD.control <- function(object){
    if ( !(object@do.community.biomass.demersal %in% c(0,1))) return("option do.community.biomass.demersal must have the value 0 or 1")
    # Everything is fine
    return(TRUE)
}

setClass("FLOP.MSFD.control",
    representation(
    do.community.biomass.demersal  ="numeric",
    community.biomass.demersal.ages="matrix",
    do.community.biomass.small  ="numeric",
    community.biomass.small.ages="matrix",   
    do.community.biomass.pelagic  ="numeric",
    community.biomass.pelagic.ages="matrix",    
    do.community.biomass.forage  ="numeric",
    community.biomass.forage.ages="matrix",
    do.M2.bar                    ="numeric",
    M2.bar.ages                  ="matrix",   
    do.mean.weight.at.age        ="numeric",
    mean.weight.at.age.sp        ="matrix",   
    do.community.mean.weight     ="numeric",
    community.mean.weight.sp     ="matrix",    
    do.mean.weight.C             ="numeric",
    mean.weight.C.sp             ="matrix",   
    do.community.mean.weight.C   ="numeric",
    community.mean.weight.C.sp   ="matrix",    
    do.community.F               ="numeric",
    community.F.sp               ="matrix",   
    do.community.M               ="numeric",
    community.M.sp               ="matrix",   
    do.life.expectancy           ="numeric",
    life.expectancy.first.age    ="matrix",  
    do.community.life.expectancy ="numeric",
    community.life.expectancy.options="matrix",  
    do.size.spectra              ="numeric",
    size.spectra.sp              ="matrix", 
    do.LFI                       ="numeric",
    LFI.sp                       ="matrix", 
    LFI.age                      ="matrix" 
 )
  ,
  prototype=prototype(
    do.community.biomass.demersal  =0,
    community.biomass.demersal.ages =matrix(0,ncol=1,nrow=2,dimnames=list(c("first.age","last.age"),c("sp1"))),
    do.M2.bar                    =0,
    M2.bar.ages                  =matrix(0,ncol=1,nrow=2,dimnames=list(c("first.age","last.age"),c("sp1"))),
    do.mean.weight.at.age        =0,
    mean.weight.at.age.sp        =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    do.community.mean.weight     =0,
    community.mean.weight.sp     =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    do.mean.weight.C             =0,
    mean.weight.C.sp             =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    do.community.mean.weight.C   =0,
    community.mean.weight.C.sp   =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    do.community.F               =0,
    community.F.sp               =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    do.community.M               =0,
    community.M.sp               =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    do.life.expectancy           =0,
    life.expectancy.first.age    =matrix(1,ncol=1,nrow=1,dimnames=list(c("first.age"),c("sp1"))),
    do.community.life.expectancy =0,
    community.life.expectancy.options=matrix(1,ncol=1,nrow=2,dimnames=list(c("first.age","weighting"),c("sp1"))),
    do.size.spectra              =0,
    size.spectra.sp              =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    do.LFI                       =0,
    LFI.sp                       =matrix(1,ncol=1,nrow=1,dimnames=list(c("include"),c("sp1"))),
    LFI.age                      =matrix(1,ncol=1,nrow=1,dimnames=list(c("first.age"),c("sp1"))),
    do.community.biomass.small  =0,
    community.biomass.small.ages=matrix(0,ncol=1,nrow=2,dimnames=list(c("first.age","last.age"),c("sp1"))),
    do.community.biomass.pelagic  =0,
    community.biomass.pelagic.ages=matrix(0,ncol=1,nrow=2,dimnames=list(c("first.age","last.age"),c("sp1"))),
    do.community.biomass.forage  =0,
    community.biomass.forage.ages=matrix(0,ncol=1,nrow=2,dimnames=list(c("first.age","last.age"),c("sp1")))
    ) 
    ,
    validity=validFLOP.MSFD.control
)                                                         
setValidity("FLOP.MSFD.control", validFLOP.MSFD.control)


# in final version remove(validOP.control)  # We do not need this function any more
### End class ###########################################################


### Methods #############################################################
 
FLOP.MSFD.control <- function()
  {
    Init.function()
    VPA.species.names<-sp.names[first.VPA:nsp]
    all.species.names<-sp.names[1:nsp]
    no.species<-nsp-first.VPA+1
    
    fala<-matrix(c(rep(fa,no.species),SMS.control@species.info[first.VPA:nsp,'last-age']),byrow=T,ncol=no.species,nrow=2,dimnames=list(c("first.age","last.age"),VPA.species.names))
    fala2<-matrix(c(rep(fa,no.species),SMS.control@species.info[first.VPA:nsp,'last-age']),byrow=T,ncol=no.species,nrow=2,dimnames=list(c("first.age","weighting"),VPA.species.names))
    fala.all<-matrix(c(rep(fa,nsp),SMS.control@species.info[1:nsp,'last-age']),byrow=T,ncol=nsp,nrow=2,dimnames=list(c("first.age","last.age"),all.species.names))

    inclsp<-matrix(1,ncol=no.species,nrow=1,dimnames=list(c("include"),VPA.species.names))
    inclsp.all<-matrix(1,ncol=nsp,nrow=1,dimnames=list(c("include"),all.species.names))

  
   res <- new("FLOP.MSFD.control",
    do.community.biomass.demersal  =as.integer(0),
    community.biomass.demersal.ages =fala.all,
    do.M2.bar                    =as.integer(0),
    M2.bar.ages                  =fala,
    do.mean.weight.at.age        =as.integer(0),
    mean.weight.at.age.sp        =inclsp,
    do.community.mean.weight     =as.integer(0),
    community.mean.weight.sp     =inclsp,
    do.mean.weight.C             =as.integer(0),
    mean.weight.C.sp             =inclsp,
    do.community.mean.weight.C   =as.integer(0),
    community.mean.weight.C.sp   =inclsp,
    do.community.F               =as.integer(0),
    community.F.sp               =inclsp,
    do.community.M               =as.integer(0),
    community.M.sp               =inclsp,
    do.life.expectancy           =as.integer(0),
    life.expectancy.first.age    =inclsp,
    do.community.life.expectancy =as.integer(0),
    community.life.expectancy.options=fala2,
    do.size.spectra              =as.integer(0),
    size.spectra.sp              =inclsp.all,
    do.LFI                       =as.integer(0),
    LFI.sp                       =inclsp.all,
    LFI.age                       =inclsp.all,
    do.community.biomass.small   =as.integer(0),
    community.biomass.small.ages =fala.all,
    do.community.biomass.pelagic =as.integer(0),
    community.biomass.pelagic.ages=fala.all,
    do.community.biomass.forage  =as.integer(0),
    community.biomass.forage.ages =fala.all)
    return(res)
 }

###

###
write.FLOP.MSFD.control<-function(control,file="op_msfd.dat",path=NULL,nice=TRUE,writeSpNames=T) {

  wr.matrix<-function(m,text){
    cat("# ",text,"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(m[j,],"\n",file=file,append=TRUE)
  }

  wr.matrix.nice<-function(m,sp){
   cat("#",formatC(sp,width=11),"\n",file=file,append=TRUE)
    for (j in (1:dim(m)[1])) cat(formatC(m[j,],width=11),"\n",file=file,append=TRUE)
  }

                                        
  if (!inherits(control, "FLOP.MSFD.control"))
        stop(paste("control" ,"must be an 'FLOP.MSFD.contol' object!"))
  
  old.path<-getwd()
  if (!is.null(path)) setwd(path)
  sepLine<-"########################################\n"

  sp.names<-dimnames(slot(control,"community.biomass.demersal.ages"))[[2]]
  VPA.sp.names<-sp.names[first.VPA:nsp]
 
  cat("# options for calculation of MSFD and other indicatores\n",file=file)
  cat('# the character "#" is used as comment character, such that all text and numbers after # are skipped\n#\n',file=file, append=TRUE)
  
  n.<-slotNames(control)
  for (x in n.) {
      switch(x,
      "do.community.biomass.demersal" ={if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate Biomass of demersal fish (option do.community.biomass.demersal)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.biomass.demersal.ages"  ={if (nice) {
                                 cat("# First and last ages to be used for calc of demersal fish biomass. Negative value excludes the species\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },
      "do.M2.bar"                    ={if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate average M2 by species (option do.M2.bar)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "M2.bar.ages"                 ={if (nice) {
                                 cat("# First and last ages to be used for calc of mean M2, Negative value excludes the species\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },
     "do.mean.weight.at.age" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate mean weight at age in the sea (option do.mean.weight.at.age)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "mean.weight.at.age.sp"     ={if (nice) {
                                 cat("# Include species 0=no, 1=yes\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },                               
     "do.community.mean.weight" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate mean weight in the sea for the whole comunity (option do.community.mean.weight)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.mean.weight.sp"     ={if (nice) {
                                 cat("# Include species 0=no, 1=yes\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },
      "do.mean.weight.C"     =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate mean weight in the catch (option do.mean.weight.C)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "mean.weight.C.sp"     ={if (nice) {
                                 cat("# Include species 0=no, 1=yes\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },                               
     "do.community.mean.weight.C" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate mean weight in the catch for the whole comunity (option do.community.mean.weight.C)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.mean.weight.C.sp"     ={if (nice) {
                                 cat("# Include species 0=no, 1=yes\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },                               
 
     "do.community.F" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate catch biomass/ stock biomass ratio (option do.community.F)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.F.sp"     ={if (nice) {
                                 cat("# Include species 0=no, 1=yes\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },  
      "do.community.M" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate dead (M) biomass/ stock biomass ratio (option do.community.M)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.M.sp"     ={if (nice) {
                                 cat("# Include species 0=no, 1=yes\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },                               
                             
     "do.life.expectancy" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate life expectancy (option do.life.expectancy)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "life.expectancy.first.age"  ={if (nice) {
                                 cat("# First age used. Negative means not used\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },                               
      "do.community.life.expectancy" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate community life expectancy (option do.community.life.expectancy)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.life.expectancy.options"  ={if (nice) {
                                 cat("# First age used. Negative means not used\n",file=file,append=T,sep="")
                                 cat("# Weighting factor, positve=input value, -1=recruitment  \n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),VPA.sp.names)
                                } else wr.matrix(slot(control,x),x)
                             }, 
      "do.size.spectra" =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate Size spectrum slope and intercept (option do.size.spectra)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "size.spectra.sp"     ={if (nice) {
                                 cat("# Include species 0=no, 1=yes\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },                               
                              
       "do.LFI"            =   {if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate Large Fish Indicator (option do.LFI)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "LFI.sp"            ={if (nice) {
                                 cat("# LFI, include species in LFI (0=no, 1=yes)\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },               
       "LFI.age"           ={if (nice) {
                                 cat("# LFI, first age considerd as a Large Fish\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },                  
     "do.community.biomass.small" ={if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate Biomass of small fish (option do.community.biomass.small)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.biomass.small.ages"  ={if (nice) {
                                 cat("# First and last ages to be used for calc of small fish biomass. Negative value excludes the species\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },

     "do.community.biomass.pelagic" ={if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate Biomass of pelagic fish (option do.community.biomass.pelagic)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.biomass.pelagic.ages" ={if (nice) {
                                 cat("# First and last ages to be used for calc of pelagic fish biomass. Negative value excludes the species\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             },
  
     "do.community.biomass.forage" ={if (nice) {
                                       cat(sepLine,file=file,append=T)
                                       cat("## Calculate Biomass of forage fish (option do.community.biomass.forage)\n#  0=no, 1=yes\n",
                                          slot(control,x),"\n",file=file,append=T,sep="")
                                     } else  cat(slot(control,x),"\t#",x,"\n",file=file,append=TRUE)
                                    },
      "community.biomass.forage.ages" ={if (nice) {
                                 cat("# First and last ages to be used for calc of forage fish biomass. Negative value excludes the species\n",file=file,append=T,sep="")
                                    wr.matrix.nice(slot(control,x),sp.names)
                                } else wr.matrix(slot(control,x),x)
                             }


   )  #end switch
  }
 
 setwd(old.path)
}

#####


read.FLOP.MSFD.control<-function(file="op_msfd.dat",n.VPA,n.other.pred) {
  
  n.sp<-n.VPA+n.other.pred
  species.names<-readLines("species_names.in", n=n.sp)
  species.names<-species.names[(n.other.pred+1):n.sp]
  species.names<-gsub('_',' ',species.names)
  species.names<-sub('[[:space:]]+$', '', species.names)
  
  all.species.names<-readLines("species_names.in", n=n.sp)
  all.species.names<-all.species.names[1:n.sp]
  all.species.names<-gsub('_',' ',all.species.names)
  all.species.names<-sub('[[:space:]]+$', '', all.species.names)

  opt<-scan(file, comment.char = "#",quiet=TRUE) 
   
  n<-1
  control<-new("FLOP.MSFD.control")
  n.<-slotNames(control)
  for (x in n.) {
    switch(x,
      "community.biomass.demersal.ages" = {slot(control,x)<-matrix(opt[n:(n-1+2*n.sp)],ncol=n.sp,nrow=2,dimnames=list(c("first.age","last.age"),all.species.names),byrow=TRUE); n<-n+2*n.sp},
      "M2.bar.ages"               = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,dimnames=list(c("first.age","last.age"),species.names),byrow=TRUE); n<-n+2*n.VPA},
      "mean.weight.at.age.sp"   = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("include"),species.names),byrow=TRUE); n<-n+1*n.VPA},
     "community.mean.weight.sp" = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("include"),species.names),byrow=TRUE); n<-n+1*n.VPA},
     "mean.weight.C.sp"         ={slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("include"),species.names),byrow=TRUE); n<-n+1*n.VPA},
     "community.mean.weight.C.sp" = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("include"),species.names),byrow=TRUE); n<-n+1*n.VPA},
     "community.F.sp"           = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("include"),species.names),byrow=TRUE); n<-n+1*n.VPA},
     "community.M.sp"           = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("include"),species.names),byrow=TRUE); n<-n+1*n.VPA},
     "life.expectancy.first.age"  = {slot(control,x)<-matrix(opt[n:(n-1+1*n.VPA)],ncol=n.VPA,nrow=1,dimnames=list(c("first.age"),species.names),byrow=TRUE); n<-n+1*n.VPA},
     "community.life.expectancy.options"  = {slot(control,x)<-matrix(opt[n:(n-1+2*n.VPA)],ncol=n.VPA,nrow=2,dimnames=list(c("first.age","weighting"),species.names),byrow=TRUE); n<-n+2*n.VPA},
     "size.spectra.sp"           = {slot(control,x)<-matrix(opt[n:(n-1+1*n.sp)],ncol=n.sp,nrow=1,dimnames=list(c("include"),all.species.names),byrow=TRUE); n<-n+1*n.sp},
     "LFI.sp"                    = {slot(control,x)<-matrix(opt[n:(n-1+1*n.sp)],ncol=n.sp,nrow=1,dimnames=list(c("include"),all.species.names),byrow=TRUE); n<-n+1*n.sp},
     "LFI.age"                   = {slot(control,x)<-matrix(opt[n:(n-1+1*n.sp)],ncol=n.sp,nrow=1,dimnames=list(c("first.age"),all.species.names),byrow=TRUE); n<-n+1*n.sp},
     "community.biomass.small.ages"  = {slot(control,x)<-matrix(opt[n:(n-1+2*n.sp)],ncol=n.sp,nrow=2,dimnames=list(c("first.age","last.age"),all.species.names),byrow=TRUE); n<-n+2*n.sp},
     "community.biomass.pelagic.ages"  = {slot(control,x)<-matrix(opt[n:(n-1+2*n.sp)],ncol=n.sp,nrow=2,dimnames=list(c("first.age","last.age"),all.species.names),byrow=TRUE); n<-n+2*n.sp},
     "community.biomass.forage.ages"  = {slot(control,x)<-matrix(opt[n:(n-1+2*n.sp)],ncol=n.sp,nrow=2,dimnames=list(c("first.age","last.age"),all.species.names),byrow=TRUE); n<-n+2*n.sp},
                               
         # otherwise                        
                              {slot(control,x)<-opt[n];n<-n+1}                                 
      )
      #print(x)
  }
  control
}

###



## show (a replacement for print of S3 classes)
setMethod("show", signature(object="FLOP.MSFD.control"),
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


if (F) {
  MSFD<-new("FLOP.MSFD.control")
  print(MSFD)
  
  MSFD<-FLOP.MSFD.control()
  
  print(MSFD)
  write.FLOP.MSFD.control(MSFD)
  a<-read.FLOP.MSFD.control(n.VPA=10,n.other.pred=16) 
}