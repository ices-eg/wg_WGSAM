# FLStockMulti.R - FLStockMulti class and methods

# Author: MV and reuse of code from the FLR Team
# Maintainer: MV
# Additions:

# Reference:
# Notes:

## class :: FLStockMulti	
validFLStockMulti <- function(object) {
	
	names <- names(getSlots('FLStockMulti')[getSlots('FLStockMulti')=="FLQuant"])
	for(i in names)
	{
		# all dimnames but iter are the same
		if(!identical(unlist(dimnames(object@catch.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in FLStockMulti@', i))
		# no. iter are equal or one
	}
	for (i in names[!names%in%c('catch', 'landings', 'discards', 'stock')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@catch.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in FLStockMulti', i))
	}
	for (i in c('catch', 'landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in FLStockMulti'))
	}
	# check range
	dim <- dim(object@catch.n)
	dimnm <- dimnames(object@catch.n)
	if(all(as.numeric(object@range[4:5]) != c(as.numeric(dimnm$year[1]),
		as.numeric(dimnm$year[dim[2]]))))
		return('Range does not match object dimensions')
	
	return(TRUE)
}

setClass("FLStockMulti",
    contains="FLStock",
    representation(
        m1          ="FLQuant",
        m2          ="FLQuant",
        ration      ="FLQuant",
        stomach     ="data.frame"
    ),
    prototype=prototype(
        m1       = new("FLQuant"),
        m2       = new("FLQuant"),
        ration   = new("FLQuant"),
        stomach=new("data.frame")
    ) #,validity=validFLStockMulti
)
#setValidity("FLStockMulti", validFLStockMulti)
remove(validFLStockMulti)
#invisible(createFLAccesors(new("FLStockMulti"), exclude=c('range', 'harvest')))

FLStockMulti <- function(name=character(0), desc=character(0), plusgroup=NA, minfbar=0, maxfbar=0, quant='quant', ...){
    args <- list(...)

    if(length(args)==0)
        args <- list(catch.n=FLQuant())

    # Set FLQuant dimensions
    qnames <- names(lapply(args, is.FLQuant)==TRUE)
    qnames <- qnames[!qnames%in%c('catch', 'discards', 'landings')]
   if(length(qnames) > 0)
   {
     dimnames <- dimnames(args[[qnames[1]]])
   } else
     dimnames <- dimnames(args[[names(lapply(args, is.FLQuant)==TRUE)[1]]])


	# template FLQuants
	if (missing(quant)) quant <- names(dimnames)[1]
	iniFLQ <- FLQuant(dimnames=dimnames, quant=quant)
	agrFLQ <- FLQuant(dimnames=c(quant='all', dimnames(iniFLQ)[-1]), quant=quant)

    dims <- dims(iniFLQ)
    
                            
    res <- new("FLStockMulti",
        name        = name,
        desc        = desc,
        catch       = agrFLQ,
        catch.n     = iniFLQ,
        catch.wt    = iniFLQ,
        discards    = agrFLQ,
        discards.n  = iniFLQ,
        discards.wt = iniFLQ,
        landings    = agrFLQ,
        landings.n  = iniFLQ,
        landings.wt = iniFLQ,
        stock       = agrFLQ,
        stock.n     = iniFLQ,
        stock.wt    = iniFLQ,
        mat         = iniFLQ,
        m           = iniFLQ,
        harvest     = iniFLQ,
        harvest.spwn= iniFLQ,
        m.spwn      = iniFLQ,
        range       = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
                             minyear=dims$minyear, maxyear=dims$maxyear, minfbar=minfbar, maxfbar=maxfbar)),
        m1          = iniFLQ,
        m2          = iniFLQ,
        ration      = iniFLQ 
  )
      
    # Load given slots
    for(i in names(args))
        if (i != 'iniFLQuant') slot(res, i) <- args[[i]]

    return(res)
}   


######################################################
# is.FLStockMulti	{{{
is.FLStockMulti <- function(x)
	return(inherits(x, "FLStockMulti"))	
# }}}
######################################################

SMS2FLStocks<-function(sumfile="summary.out",read.input=TRUE,read.output=TRUE,
                       bio.interact=FALSE,control=NULL,FLStocksMulti=NULL) {

    if (is.null(control) | (!inherits(control, "FLSMS.control")))
        stop("A valid 'FLSMS.control'  must be given!")
    aaa<-list()
    nsp<-slot(control,"no.species")
    if (slot(control,"last.season")==1) annual<-TRUE else annual<-FALSE
    for (ii in (1:nsp)) if (control@species.info[ii,'predator']!=2) {first.VPA<<-ii; break;} #first VPA  species number

    pl.grp<-slot(control,"species.info")[,"+group"]
    max.age<-slot(control,"species.info")[,"last-age"]
    pl.grp[pl.grp==1]<-max.age[pl.grp==1]
    pl.grp[pl.grp==0]<-NA
    Fbars<-slot(control,"avg.F.ages")


    #read summary.out file from SMS run
    sms<-read.table(sumfile,header=TRUE)
    sms$unit<-1  # to fit FLquant
    sms$area<-1
    sms$iter<-1
    
    sms<-droplevels(subset(sms,Species.n>=first.VPA))
    sms$discard<-sms$CWsum-sms$Yield
    sms$discard.n<-ifelse(sms$discard<=0,0,sms$C.obs*sms$discard/sms$CWsum)
    sms$discard.w<-ifelse(sms$discard<=0,0,sms$weca)

    sms$landings<-sms$Yield
    sms$landings.n<-ifelse(sms$C.obs==0,0,sms$C.obs*sms$landings/sms$CWsum)
    sms$landings.w<-ifelse(sms$Yield>=0,sms$weca,0)
    #sms$chck<-sms$discard.n+sms$landings.n
    #subset(sms,Year==2010 & Quarter==1 & Species.n==1)
    var.names<-names(sms)
    index<-list(sms$Species.n,sms$Age,sms$Year,sms$unit,sms$Quarter,sms$area,sms$iter)

    condense<-function(x,index) {
      y<-tapply(x,index,sum)
      y[is.na(y)]<-0
      return(y)
    }
    
    # catch information is allways available
    if ("C.obs" %in% var.names) c.n<-condense(sms$C.obs,index)  # catch numbers observed
    #if ("C.hat" %in% var.names) c.n<-condense(sms$C.hat,index)  # catch numbers, prediction
    
    if (read.input) {
      if ("weca" %in% var.names) {
         c.wt<-condense(sms$weca,index)  # catch mean weigth 
         catch<-c.n*c.wt                 # catch weigth
         catch<-apply(catch,c(1,3,4,5,6),sum)

         dis.wt<-condense(sms$discard.w,index)  # discard mean weigth
         dis.n<-condense(sms$discard.n,index)
         dis<-dis.n*dis.wt
         dis<-apply(dis,c(1,3,4,5,6),sum)

         lan.wt<-condense(sms$landings.w,index)  # discard mean weigth
         lan.n<-condense(sms$landings.n,index)
         lan<-lan.n*lan.wt
         lan<-apply(lan,c(1,3,4,5,6),sum)
      }
      if ("west" %in% var.names) s.wt<-condense(sms$west,index)  # stock mean weigth   
      if ("M" %in% var.names) m<-condense(sms$M,index)        # natural mortality, single species
      if ("propmat" %in% var.names) mat<-condense(sms$propmat,index)# proportion mature
      if (bio.interact) {
        if ("M1" %in% var.names) m1<-condense(sms$M1,index)        # Residual natural mortality, multispecies species
      }

    }
    
    if (read.output) {
      if ("N" %in% var.names) s.n<-condense(sms$N,index)      # stock numbers
      if ("F" %in% var.names) harvest<-condense(sms$F,index)  # harvest rate=F 
      if (bio.interact) {
        if ("M2" %in% var.names) m2<-condense(sms$M2,index)     # Predation mortality, multispecies species
      }
    }

    # read species names
    s<-slot(control,"species.names")

    if (is.null(FLStocksMulti)) {
      if (nsp>1) new.fls <- new("FLStocks") 
    } else new.fls<-FLStocksMulti
    

  # template for input to quant
    c1<-c.n[1,,,,,,]
    dc<-dim(c1)
    nc<-dimnames(c1)
    if (annual) {nc[[3]]="all"; dc[3]<-1}
    dimnames<-list(age=nc[[1]],year=nc[[2]],unit="all",season=nc[[3]],area="all",iter="none")
    dim<-c(dc[1],dc[2],1,dc[3],1,1)
    dimnames2<-list(age='all',year=nc[[2]],unit="all",season=nc[[3]],area="all",iter="none")
    dim2<-c(1,dc[2],1,dc[3],1,1)

 
    for (si in (first.VPA:nsp)) {
        sp.no<-si-first.VPA+1
        if (is.null(FLStocksMulti)) {  # create a new Stock object
            if (nsp==1) q<-c.n else q<-c.n[sp.no,,,,,,]  
            s.<-FLStockMulti(name =s[si], desc =s[si],plusgroup=as.numeric(pl.grp[si]), minfbar=Fbars[sp.no,1], maxfbar=Fbars[sp.no,2],
                     iniFLQuant=FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="thousands"))
        } else s.<-new.fls[[si]]        # take the old one

        if (read.input) { 
            s.@catch.n <-FLQuant(    q,dim=dim,dimnames=dimnames,quant="age",units="thousands")

            if (nsp==1) q<-c.wt else q<-c.wt[sp.no,,,,,,]   
            s.@catch.wt<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="kg")
            s.@catch   <-FLQuant(apply(s.@catch.n*s.@catch.wt,c(2,3,4),sum),dimnames=dimnames2,quant="age",units="tonnes")

            if (nsp==1) q<-dis.wt else q<-dis.wt[sp.no,,,,,,] 
            s.@discards.wt<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="kg")

            if (nsp==1) q<-dis.n else q<-dis.n[sp.no,,,,,,] 
            s.@discards.n<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="thousands")
            s.@discards   <-FLQuant(apply(s.@discards.n*s.@discards.wt,c(2,3,4),sum),dimnames=dimnames2,quant="age",units="tonnes")

            if (nsp==1) q<-lan.wt else q<-lan.wt[sp.no,,,,,,] 
            s.@landings.wt<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="kg")

            if (nsp==1) q<-lan.n else q<-lan.n[sp.no,,,,,,] 
            s.@landings.n<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="thousands")
            s.@landings   <-FLQuant(apply(s.@landings.n*s.@landings.wt,c(2,3,4),sum),dimnames=dimnames2,quant="age",units="tonnes")

            if (nsp==1) q<-s.wt else q<-s.wt[sp.no,,,,,,]     
            if ("west" %in% var.names) s.@stock.wt<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="kg")

            if (nsp==1) q<-m else q<-m[sp.no,,,,,,]  
            if ("M" %in% var.names) s.@m          <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units=" ")

            if (nsp==1) q<-mat else q<-mat[sp.no,,,,,,]  
            if ("propmat" %in% var.names) s.@mat  <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="proportion")  

            if (bio.interact) {
               if (nsp==1) q<-m1 else q<-m1[sp.no,,,,,,]   
               if ("M1" %in% var.names) s.@m1    <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units=" ")
            }
        }                
        if (read.output) {
            if (nsp==1) q<-s.n else q<-s.n[sp.no,,,,,,]    
            if ("N" %in% var.names) s.@stock.n  <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="thousands")

            if (nsp==1) q<-harvest else q<-harvest[sp.no,,,,,,]  
            if ("F" %in% var.names) s.@harvest  <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="f")

            if (("west" %in% var.names) & ("N" %in% var.names)) s.@stock<-FLQuant(apply(s.@stock.n*s.@stock.wt,c(2,3,4),sum),dimnames=dimnames2,quant="age",units="tonnes")

            if (bio.interact) {
              if (nsp==1) q<-m2 else q<-m2[sp.no,,,,,,]   
               if ("M2" %in% var.names) s.@m2    <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units=" ")
            }
        }                                       
      
       #print(landings.n(s.))
        if (nsp>1) new.fls[[sp.no]]<-s. 
 
    } 
    #if (nsp==1) new.fls<-1  # Hack
  
    if(nsp>1) return(new.fls) else return(s.)

   
}



FLStocks2SMS<-function(FLStock=NULL,control=NULL,path=NULL, bio.interact=FALSE) {

  old.wd<-getwd()
  if (!is.null(path)) setwd(path)

  if (is.null(control) | (!inherits(control, "FLSMS.control")))
       stop("A valid 'FLSMS.control'  must be given!")

  if (is.null(FLStock))
     stop("A 'FLStock' or 'FLStocks' must be given")

  if (!validObject(FLStock)) stop("FLStock is not valid!")
   
  if (inherits(FLStock, "FLStock") || inherits(FLStock, "FLStockMulti")) {
     new.fls <- new("FLStocks");
     #FLStock<-FLStocks(FLStock)
     new.fls[[1]]<-FLStock
     FLStock<-new.fls
  }

  for (i in 1:length(FLStock)){   
    FLS<-FLStock[[i]]
    if ( "min"  %in% names(FLS@range)) minage <- FLS@range["min"]  else  stop("'minage' not found in range")
    if ("max"   %in% names(FLS@range)) maxage <- FLS@range["max"]  else stop("'maxage' not found in range")
  }

   first.year   <-control@first.year 
   last.year    <-control@last.year
   last.year.model<-control@last.year.model
   first.season <-1
   last.season  <-control@last.season
   last.season.last.year<-control@last.season.last.year
   no.species   <-control@no.species
   first.age    <-control@first.age
   rec.season   <-control@rec.season
   max.age.all  <-control@max.age.all

   age.range<-as.character(first.age:max.age.all)
   year.range<-as.character(first.year:last.year.model)
   ncol<-max.age.all-first.age+1

   info<-control@species.info
   no.predators<-sum(info[,"predator"]>0)
   no.other.predators<-sum(info[,"predator"]==2)
   no.VPA.sp<-no.species-no.other.predators

   #removeMethod("apply", signature(X="FLQuant"))


   for (i in 1:length(FLStock)) {  
     tmp.catch.n<-matrix(apply(FLStock[[i]]@catch.n[age.range,year.range , , ,,],c(4,2,1),sum),ncol=ncol)
     tmp.catch.wt<-matrix(apply(FLStock[[i]]@catch.wt[age.range,year.range , , , ,],c(4,2,1),sum),ncol=ncol)
     tmp.m<-matrix(apply(FLStock[[i]]@m[age.range,year.range , , ,, ],c(4,2,1),sum),ncol=ncol)
     tmp.stock.wt<-matrix(apply(FLStock[[i]]@stock.wt[age.range,year.range , , , , ],c(4,2,1),sum),ncol=ncol)
     tmp.mat<-matrix(apply(FLStock[[i]]@mat[age.range,year.range , , , ,],c(4,2,1),sum),ncol=ncol)

     if (i==1) {
       all.catch.n<-tmp.catch.n 
       all.catch.wt<-tmp.catch.wt 
       all.m<-tmp.m 
       all.stock.wt<-tmp.stock.wt 
       all.mat<-tmp.mat 
     } else { 
       all.catch.n<-rbind(all.catch.n,tmp.catch.n)
       all.catch.wt<-rbind(all.catch.wt,tmp.catch.wt)
       all.m<-rbind(all.m,tmp.m)
       all.stock.wt<-rbind(all.stock.wt,tmp.stock.wt)
       all.mat<-rbind(all.mat,tmp.mat)
     }
   }
   write.table(file="canum.in" ,all.catch.n,row.names=FALSE,col.names=FALSE,quote=FALSE)
   write.table(file="weca.in"  ,all.catch.wt,row.names=FALSE,col.names=FALSE,quote=FALSE)
   write.table(file="natmor.in",all.m,row.names=FALSE,col.names=FALSE,quote=FALSE)
   write.table(file="west.in"  ,all.stock.wt,row.names=FALSE,col.names=FALSE,quote=FALSE)
   write.table(file="propmat.in",all.mat,row.names=FALSE,col.names=FALSE,quote=FALSE) 
   setwd(old.wd)
}


