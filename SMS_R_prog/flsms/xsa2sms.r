XSA2SMS<-function(SMS.path=NULL, XSA.path=NULL,index.name,tuning.name,stock.name="my stock",
                    plusgroup=NA,Fbarmin=1,Fbarmax=2,N1=1E6) {
### import data
# -------------


  # set default directory
  setwd(XSA.path)

  # Catch data
  # --------------

  # read VPA file
  stock <- readFLStock(file=index.name,name=stock.name)

  # no discards => catch=landings and discards set equal to 0
  stock@catch.n<-stock@landings.n
  stock@catch.wt<-stock@landings.wt

  # set units
  for (i in c("stock.n",  "catch.n",  "landings.n", "discards.n"))  { units(slot(stock,i)) <- "thousands"}
  for (i in c("stock.wt", "catch.wt", "landings.wt","discards.wt")) { units(slot(stock,i)) <- "kg"       }
  for (i in c("stock",    "catch",    "landings",   "discards"))    { units(slot(stock,i)) <- "tons"     }
                                                                      units(stock@harvest) <- " "

  # substitute mean weight of 0 by NA
  stock@stock.wt[stock@stock.wt==0]<-NA

  subMis<-function(W){
    W[W==0]<-NA
    meanW<-yearMeans(W)
    #print(meanW)
    for (a in (dims(W)$min:dims(W)$max)) {
      aa<-as.character(a)
      b<-W[aa,,,,,]
      b[is.na(b)]<-meanW[aa,1,1,1,1,1]
      W[aa,,,,,]<-b
    }
    W
  }

  stock@stock.wt<-subMis(stock@stock.wt)
  stock@catch.wt<-subMis(stock@catch.wt)

  # set plusgroup
  stock@range["plusgroup"]  <- plusgroup

  #stock<-setPlusGroup(stock, plusgroup = plusgroup, na.rm=FALSE)


   
  # Fleet data
  # --------------
  
  # read VPA file
  fleet <- readFLIndices(file=tuning.name)

  # check the fleet names
  nlst <- lapply(fleet,name)
  nlst <- lapply(nlst, gsub, pattern=" ", replacement="_")
  nlst <- lapply(nlst, gsub, pattern="-", replacement="_")
  nlst <- lapply(nlst, gsub, pattern="/", replacement="_")
 
  names(fleet)<-unlist(nlst)
  
  # set fleet desciption to the name of the stock
  for (k in 1:length(fleet)) fleet[[k]]@desc<-stock.name

################################################
### we have now a stock (FLStock object) and 
##    a fleet (FLindex object)
################################################
 
### convert the FLStock object to a FLStockMulti object
# ---
ms<-new("FLStockMulti",stock)

print(summary(ms))

### Create a FLSMS.control object
# ---
r<-ms@range
control<-FLSMS.control(first.age=r["min"],max.age.all=r["max"],
              first.year=r["minyear"],last.year=r["maxyear"],species.names=ms@name)
   
   
# set plusgroup flag. 1=yes last age is a plusgroup, 0=no plusgroup
if (is.na(r["plusgroup"])) control@species.info[1,5]<-0 else control@species.info[1,5]<-1

# set ages for mean F
control@avg.F.ages[1,]<-c(Fbarmin,Fbarmax)

# write the control object on ASCII format for use by SMS
write.FLSMS.control(control,path=SMS.path,write.multi=FALSE,nice=TRUE)


### write the FLStockMulti object on ASCII format for use by SMS
# ---
FLStocks2SMS(FLStock=ms,control=control,path=SMS.path, bio.interact=FALSE)


### convert FLIndex objects into FLIndex.SMS objects  and collct them into a FLIndices object 
# --- 
SMS.fleet<-new("FLIndices")
for (fl in (1:length(fleet))) {
 ff<-new("FLIndex.SMS",fleet[[fl]])
 ff@range.SMS$q.age<-max(ff@range["max"]-1,ff@range["min"])
 ff@range.SMS$var.age.group<-seq(ff@range["min"],max(ff@range["max"]-1,ff@range["min"]))
 SMS.fleet[[fl]]<-ff
}

# functon to scale effort, such that catchability becomes close to one,
# mean.Z is mean Z at age
# N1 is mean stock number at age 1
#
scale.effort<-function(FLIndices,N,mean.Z=1,N1=300000){
    n.ages<-20
    
    N<-rep(NA,n.ages)
    N[1]<-N1*exp(mean.Z)
    N[2]<-N1
    for (i in (3:n.ages)) N[i]<-N[i-1]*exp(-mean.Z)
    j<-1
    fact<-rep(NA,length(FLIndices))
    for (fl in (FLIndices)) {
        catch<-as.vector(yearMeans(fl@catch.n))
        eff<-as.numeric(yearMeans(fl@effort))
        CPUE<-catch/eff
        a1<-fl@range["min"]+1
        a2<-fl@range["max"]+1
        q<-CPUE/N[a1:a2]
        fac<-mean(q)
        i<-1E12
        while (TRUE) if ((fac/i)>1) break else i<-i/10
        fact[j]<-i
        fl@effort<-fl@effort*i
        fl@index<-fl@index/i
        FLIndices[[j]]<-fl
        j<-j+1

    }
    cat("\nScaling factors by index:\n",fact,"\n")
    FLIndices
}

# Scale effort, such that catchability becomes close to one, given
# N1 ("guestimated" mean stock number at age 1) and mean Z
SMS.fleet<-scale.effort(SMS.fleet,mean.Z=1,N1=N1)

# write the SMS input file: fleet_catch.dat, fleet_info.dat and fleet_names.in
FLIndices2SMS(out.path=SMS.path,indices=SMS.fleet,control=control) 

#### Additonal tasks
# make file just_one.in
setwd(SMS.path)
cat(rep(1,1000),file="just_one.in")

cat(
  "# refernec points Flim Fpa blim Bpa. -1 means no reference point defined\n",
  "# -------------- VPA species  -------------\n",
  " -1 -1 -1 -1\n",  
  file="reference_points.in",sep="")

cat("\nNo errors found, SMS files were produced\n")
}
