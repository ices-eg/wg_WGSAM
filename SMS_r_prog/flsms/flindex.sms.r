
setClass("FLIndex.SMS",
    contains="FLIndex",
    representation(
        range.SMS="vector"
    ) ,
     prototype=prototype(range.SMS=list(season=1, power.age=-1, q.age=0,
        var.age.group=as.vector(0,mode="list"),minCV=0.3))
) 



FLIndex.SMS <- function(name=character(0), desc=character(0), distribution=character(0),
    type=character(0), startf=NA, endf=NA, plusgroup=NA, season=NA, power.age=NA, 
    q.age=NA,var.age.group=NA,minCV=0.3, ...) {

    args <- list(...)
    if(length(args)==0)
        args <- list(index=FLQuant())

    dimnames <- dimnames(args[[names(lapply(args, is.FLQuant)==TRUE)[1]]])
    sdimnames <- dimnames
    sdimnames[1] <- "all"

    if(!is.FLQuant(args['index']))
        index <- FLQuant(dimnames=dimnames)

    dims <- dims(index)

    new <- new("FLIndex.SMS", name = name, desc = desc, distribution = distribution,
        type=type,
        index = index, index.var = FLQuant(dimnames=dimnames),
        index.q = FLQuant(dimnames=dimnames), sel.pattern = FLQuant(dimnames=dimnames),
        catch.n = FLQuant(dimnames=dimnames), catch.wt = FLQuant(dimnames=dimnames),
        effort = FLQuant(dimnames=sdimnames), 
        range = unlist(list(min=dims$min, max=dims$max,
        plusgroup=NA, minyear=dims$minyear, maxyear=dims$maxyear, startf=startf, endf=endf)))
        range.SMS = unlist(list(season=season, power.age=power.age,
                 q.age=q.age,var.age.group=var.age.group, minCV=minCV))
  
    # Add extra arguments
    for(i in names(args)[names(args)!='iniFLQuant'])
        slot(new, i) <- args[[i]]

    return(new)
}   


SMS2FLIndices<-function(control,path=NULL,fleet.inf="fleet_info.dat",fleet.index="fleet_catch.in",
                        fleet.name="fleet_names.in") {
                  
    old.wd<-getwd()
    if (!is.null(path)) setwd(path)
     
    nsp<-slot(control,"no.species")
    nq<-slot(control,"last.season")
    
    #count number of other predators    
    info<-slot(control,"species.info")[,"predator"]
    no.oth<-sum(info==2) 
    nsp<-nsp-no.oth  
   
    s<-readLines(fleet.name, n=1000)
    s<-gsub('_',' ',s)
    fl.names<-sub('[[:space:]]+$', '', s)

    info<-scan(fleet.inf,comment.char = "#",quiet=TRUE) 
    minCV<-info[1]
    i<-2
    n.fleet<-as.vector(info[i:(i-1+nsp)])
    i<-i+nsp
    sum.fleet<-sum(n.fleet)
    fl.info<-matrix(info[i:(i-1+sum.fleet*10)],ncol=10,nrow=sum.fleet,byrow=TRUE)
    i<-i+sum.fleet*10
    
    sum.var.age<-sum(fl.info[,10])
    fl.var<-as.vector(info[i:(i-1+sum.var.age)])
 
    CE<-scan(fleet.index,comment.char = "#",quiet=TRUE) 
 
    # creates empty FLIndices object
    FLIndices. <- FLIndices()       

    i<-1
    v<-1
    sp.fl<-0
    for (sp in 1:nsp) {
     for (fl in 1:n.fleet[sp]) {
      sp.fl<-sp.fl+1
      fy<-fl.info[sp.fl,1]
      ly<-fl.info[sp.fl,2]
      alfa<-fl.info[sp.fl,3]
      beta<-fl.info[sp.fl,4]
      fa<-fl.info[sp.fl,5]
      la<-fl.info[sp.fl,6]
      la.q<-fl.info[sp.fl,7]
      la.p<-fl.info[sp.fl,8]
      seas<-fl.info[sp.fl,9]
      n.var<-fl.info[sp.fl,10]
     
      nyr<-ly-fy+1
      nages<-la-fa+1
      
        
       # template for input to quant
      
      dim<-c(nages,nyr,1,1,1,1) 
      dim2<-c(1,nyr,1,1,1,1) 
      dimnames<-list(age=fa:la,year=fy:ly,unit="all",season=seas,area="all",iter="none")
      dimnames2<-list(age="all",year=fy:ly,unit="all",season=seas,area="all",iter="none")  

      tmp<-matrix(CE[i:(nyr*(nages+1)+i-1)],ncol=nages+1,nrow=nyr,byrow=TRUE)

      effort<-array(tmp[,1],dim=dim2,dimnames=dimnames2)
      catch<-matrix(tmp[,2:(nages+1)],ncol=nyr,nrow=nages,byrow=TRUE)
      #print(catch)
      catch<-array(catch,dim=dim,dimnames=dimnames)
      #print(catch)
      index<-catch/rep(effort,each=nages)

      indx<-FLIndex.SMS(index=as.FLQuant(index),effort=as.FLQuant(effort),catch.n=as.FLQuant(catch),
                 name = fl.names[sp.fl], desc = fl.names[sp.fl])

 
      indx@range<-unlist(list("min"=fa,"max"=la,"plusgroup"=NA,"minyear"=fy,"maxyear"=ly,"startf"=alfa,"endf"=beta))
     
      indx@range.SMS<-list("season"=seas, "power.age"=la.p, 
                            "q.age"=la.q,"var.age.group"=as.vector(fl.var[v:(v-1+n.var)]),"minCV"=minCV)
      
      v<-v+n.var
      i<-i+nyr*(nages+1)
      FLIndices.[[sp.fl]]<-indx 
    } 
   } 
   setwd(old.wd)
   FLIndices.
 }



FLIndices2SMS<-function(out.path=NULL,indices=NULL,control=NULL,fleet.inf="fleet_info.dat",
                        fleet.index="fleet_catch.in",fleet.name="fleet_names.in") {
    old.wd<-getwd()
    if (!is.null(out.path)) setwd(out.path)
    
    if (is.null(indices))
        stop("A 'FLIndices' must be given")
    
    if (!inherits(indices, "FLIndices"))
        stop("indices must be an 'FLIndices' object!")
    
    for (i in 1:length(indices)) {
     if (is.na(indices[[i]]@range["startf"]) || is.na(indices[[i]]@range["endf"]))
         stop(paste("Must supply startf & endf for range in FLIndex",i))

      if (!all(names(indices[[i]]@range) == c("min","max","plusgroup","minyear","maxyear","startf","endf")))
         stop("Range must have names 'min','max','plusgroup','minyear','maxyear','startf','endf'")
    }
        
    if (!inherits(control, "FLSMS.control"))
    stop("control must be an 'FLSMS.control' object!")

    if (!validObject(control)) stop("control is not valid!")
    
    nsp<-slot(control,"no.species")
    #count number of other predators    
    info<-slot(control,"species.info")[,7]
    no.oth<-sum(info==2) 
    nsp<-nsp-no.oth  # no of VPA species

    first.year<-slot(control,"first.year")
    last.year<-slot(control,"last.year.model")
    last.season<-slot(control,"last.season")
    n.season<-last.season
    
    no.indices<-rep(0,nsp)
    info<-matrix(0,ncol=10,nrow=length(indices))
    fl.name<-rep('',length(indices))
    v.age<-list()
    
    sp<-1; n<-1
    old.sp<-substr(indices[[1]]@desc,1,3)
        
    cat("# file fleet_catch.in\n",file=fleet.index)
    for (idc in indices) {
        fl.name[n]<-idc@name
        sp.name<-substr(idc@desc,1,3)
        if (nsp>1 & sp.name!=old.sp) {sp<-sp+1; old.sp<-sp.name}
        cat("# ",sp.name,",",fl.name[n],"\n",file=fleet.index,append=TRUE)
        no.indices[sp]=no.indices[sp]+1
        range<-idc@range
        info[n,1]<-range["minyear"]
        info[n,2]<-range["maxyear"]
        info[n,3]<-range["startf"]
        info[n,4]<-range["endf"]
        info[n,5]<-range["min"]
        info[n,6]<-range["max"]
        info[n,7]<-idc@range.SMS$q.age
        info[n,8]<-idc@range.SMS$power.age
        info[n,9]<-idc@range.SMS$season
        info[n,10]<-length(idc@range.SMS$var.age.group)
        v.age<-c(v.age,list(idc@range.SMS$var.age.group))
        minCV<-idc@range.SMS$minCV
        write.table(cbind(as.vector(idc@effort),
                    t(matrix(idc@catch.n,ncol=info[n,2]-info[n,1]+1,byrow=FALSE))),
                    file=fleet.index,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
        n<-n+1
    }  
    cat("-999 #  Check value\n",file=fleet.index,append=TRUE)
    
    cat(paste("# file: fleet_info.dat\n",minCV," #min CV of CPUE observations\n"),file=fleet.inf)
    cat("# number of fleets by species\n",file=fleet.inf,append=TRUE)
    write(no.indices,file=fleet.inf,ncolumns=nsp,append=TRUE)
    cat("#############", 
        "\n# 1-2, First year last year,",
        "\n# 3-4. Alpha and beta - the start and end of the fishing period for the fleet given as fractions of the season (or year if annual data are used),",
        "\n# 5-6   first and last age,",
        "\n# 7.   last age with age dependent catchability,", 
        "\n# 8.   last age for stock size dependent catchability (power model), -1 indicated no ages uses power model,",
        "\n# 9.   season for survey,",
        "\n# 10.  number of variance groups for estimated cathability,",
        "\n# by species and fleet",
        "\n#############\n", file=fleet.inf,append=TRUE)
    i<-0
    for (s in (1:nsp)) {
      cat("# ",control@species.names[s+no.oth],"\n",file=fleet.inf,append=TRUE)
      for (id in (1:no.indices[s])) {
        i<-i+1
        cat("# ",fl.name[i],"\n",file=fleet.inf,append=TRUE)
        write(file=fleet.inf ,info[i,],ncolumns=10,append=TRUE)
      }
    }

#          write.table(file=fleet.inf ,info,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)

    cat("# variance groups\n",file=fleet.inf,append=TRUE)
    #for (a in v.age) write(a,file=fleet.inf,append=TRUE)
    i<-0
    for (s in (1:nsp)) {
      cat("# ",control@species.names[s+no.oth],"\n",file=fleet.inf,append=TRUE)
      for (id in (1:no.indices[s])) {
        i<-i+1
        cat("# ",fl.name[i],"\n",file=fleet.inf,append=TRUE)
        write(file=fleet.inf ,v.age[[i]],append=TRUE)
      }
    }
    cat("-999 #  Check value\n",file=fleet.inf,append=TRUE)
    
    fl.name<-substr(paste(fl.name,"__________________________",sep=''),1,26)
    fl.name<-gsub(' ','_',fl.name)
    write(fl.name,file=fleet.name)
    
    setwd(old.wd)
}

   # test FLIndices2SMS(out.path=out.path,indices=SMS.indices,control=SMS.dat)
