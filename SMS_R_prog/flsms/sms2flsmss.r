
SMS2FLSMSs<-function(sumfile=file.path(data.path,'summary.out'),residualsFile="Catch_survey_residuals.out",bio.interact=FALSE,read.input=FALSE,read.output=TRUE,
                      control=NULL,FLStocksMulti=NULL) {

    if (is.null(control) | (!inherits(control, "FLSMS.control")))
        stop("A valid 'FLSMS.control'  must be given!")

    nsp<-slot(control,"no.species")
    if (slot(control,"last.season")==1) annual<-TRUE else annual<-FALSE
    info<-slot(control,"species.info")[,"predator"]
    first.VPA<-max(length(info[info==2])+1,1) # other predator have predator code =2
    pl.grp<-slot(control,"species.info")[,"+group"]
    max.age<-slot(control,"species.info")[,"last-age"]
    pl.grp[pl.grp==1]<-max.age[pl.grp==1]
    pl.grp[pl.grp==0]<-NA


    #read summary.out file from SMS run
    sms<-read.table(sumfile,header=TRUE)
    var.names<-names(sms)
    index<-list(sms$Species.n,sms$Age,sms$Year,sms$Quarter)

    condense<-function(x,index) {
      y<-tapply(x,index,sum)
      y[is.na(y)]<-0
      y
    }
    if (read.input) {
      if ("C.obs" %in% var.names) c.n<-condense(sms$C.obs,index)  # catch numbers observed
      if ("west" %in% var.names) s.wt<-condense(sms$west,index)  # catch mean weight
      if ("weca" %in% var.names) c.wt<-condense(sms$weca,index)
      if ("M" %in% var.names) m<-condense(sms$M,index)
      if ("propmat" %in% var.names) mat<-condense(sms$propmat,index)
    }
    if (read.output) {
       if ("C.hat" %in% var.names) c.n.hat<-condense(sms$C.hat,index) # catch numbers, prediction
       if ("N" %in% var.names) s.n<-condense(sms$N,index)          # stock numbers
       if ("F" %in% var.names) harvest<-condense(sms$F,index)      # harvest rate=F
       if (bio.interact) {
         if ("M2" %in% var.names) m2<-condense(sms$M2,index)     # Predation mortality, multispecies species
       }
    }

      condense2<-function(x,index) {
      y<-tapply(x,index,sum)
      y[y==-99.9]<-NA
      y
    }
    if (read.output) {
      #read Catch_survey_residuals.out file from SMS run
      resids<-read.table(residualsFile,header=TRUE)

      # catch data
      resid.c<-subset(resids,data=="catch")
      index<-list(resid.c$Species.n,resid.c$Age,resid.c$Year,resid.c$Quarter)

      c.n.resid<-condense2(resid.c$residual,index)  # catch residuals
    }

    # species names
    s<-slot(control,"species.names")

    if (nsp>1) new.fls <- new("FLSMSs") else new.fls<-FLSMS

    # template for quant
    c1<-c.n[1,,,]
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
            if (nsp==1) q<-c.n else q<-c.n[sp.no,,,]   # Why ?

            s.<-FLStockMulti(name =s[si], desc =s[si],plusgroup=pl.grp[si],
                     iniFLQuant=FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="1000"))
        } else s.<-new.fls[[si]]        # take the old one
        if (read.input) {

            s.@catch.n <-FLQuant(    q,dim=dim,dimnames=dimnames,quant="age",units="1000")

            if (nsp==1) q<-c.wt else q<-c.wt[sp.no,,,]
            if ("weca" %in% var.names) s.@catch.wt<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="kg")
            if ("weca" %in% var.names) s.@catch   <-FLQuant(apply(s.@catch.n*s.@catch.wt,c(2,3,4),sum),dimnames=dimnames2,quant="all",units="ton")

            if (nsp==1) q<-s.wt else q<-s.wt[sp.no,,,]
            if ("west" %in% var.names) s.@stock.wt<-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="kg")

            if (nsp==1) q<-m else q<-m[sp.no,,,]
            if ("M" %in% var.names) s.@m          <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units=" ")

            if (nsp==1) q<-mat else q<-mat[sp.no,,,]
            if ("propmat" %in% var.names) s.@mat  <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="proportion")

            if (bio.interact) {
               if (nsp==1) q<-m1 else q<-m1[sp.no,,,]
               if ("M1" %in% var.names) s.@m1    <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units=" ")
            }
        }
        if (read.output) {
            if (nsp==1) q<-s.n else q<-s.n[sp.no,,,]
            if ("N" %in% var.names) s.@stock.n  <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="1000")

            if (nsp==1) q<-harvest else q<-harvest[sp.no,,,]
            if ("F" %in% var.names) s.@harvest  <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units="f")

            if (("west" %in% var.names) & ("N" %in% var.names)) s.@stock    <-FLQuant(apply(s.@stock.n*s.@stock.wt,c(2,3,4),sum),dimnames=dimnames2,quant="all",units="ton")

            if (bio.interact) {
              if (nsp==1) q<-m2 else q<-m2[sp.no,,,]   # Why ?
               if ("M2" %in% var.names) s.@m2    <-FLQuant(q,dim=dim,dimnames=dimnames,quant="age",units=" ")
            }

        }

        if (nsp>1) new.fls[[sp.no]]<-s.
    }
    ifelse(nsp>1, return(new.fls),return(s.))
}



