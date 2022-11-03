
# get the SAM, used to read input files on "Lowestoft format"
# first time only
#  devtools::install_github("fishfollower/SAM/stockassessment")
# to install a newer version
# remove.packages('stockassessment') # remember also to delete the directory stockassessment in the win-library (if not the old SAM version is kept, in some cases) 

library('stockassessment')


rawDataDir<- file.path("C:","MV","SMS-git","SMS-input-key-run-2017","INPUT_by_Stock")



SMS<-SMS.control  # option with a shorter name

fa<-SMS@first.age

#

arr2dfName <- function(arr,idx=NULL,name='y') {
  if(is.null(dimnames(arr))){dimnames(arr)<-lapply(dim(arr), FUN=function(x)1:x)}
  dn <- dimnames(arr)
  #if (any(unlist(lapply(dn,is.null)))) stop('Length of dimnames must equal length of dimension.')
  for (i in 1:length(dim(arr))) if (is.null(dn[[i]])) dn[[i]]<-as.character(1:(dim(arr)[i]))
  if(is.null(names(dn))){names(dn)<-paste('index', 1:length(dim(arr)), sep=".")}
  ans <- cbind(expand.grid(dn,stringsAsFactors = FALSE),as.vector(arr))
  if (is.null(idx)) idx<-c(colnames(ans)[-ncol(ans)])
  colnames(ans)<-c(idx,name)
  return(as.data.frame(ans))
}


doCatch<-function(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='AAA') { 
  a<-merge( arr2dfName(cn,idx=c('year','age'),name='CATCHN'), arr2dfName(cw,idx=c('year','age'),name='WCATCH'))
  
  b<-arr2dfName(ln,idx=c('year','age'),name='lanN')
  a<-merge(a,b)
  
  a[as.numeric(a$age)>la,'age']<-la
  a$sop<-a$CATCHN*a$WCATCH
  a<-aggregate(cbind(CATCHN,sop,lanN)~year+age,sum,na.rm=T,data=a)
  a$WCATCH<-a$sop/a$CATCHN
  a[is.na(a$WCATCH),'WCATCH']<-0

  a$PROP_CAT<-a$lanN/a$CATCHN
  a[is.na(a$PROP_CAT),'PROP_CAT']<-1
  a[a$PROP_CAT>1,'PROP_CAT']<-1
  a$species<-species
  a$quarter<-1
  a$sop<-a$lanN<-NULL
  save(a,file=paste0(species,'_catch.Rdata'))
}


doBio01<-function(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='AAA') { 
  cat('Species:',species,'\n')
  a<-merge(arr2dfName(mo,idx=c('year','age'),name="PROPMAT"), arr2dfName(nm,idx=c('year','age'),name='M'))
  a<-merge(a,arr2dfName(sw,idx=c('year','age'),name='ICES_WSEA'))
  a<-subset(a,as.numeric(a$age)<=la)
  a$species<-species
  a$quarter<-1
  save(a,file=paste0(species,'_bio01.Rdata'))
}


############################
# Cod

la<-SMS@species.info['Cod','last-age']

setwd(file.path(rawDataDir,'Cod'))

cn <- read.ices("cn.dat")
cw <- read.ices("cw.dat")

ln <- read.ices("lf.dat")
lw <- read.ices("lw.dat")

mo <- read.ices("mo.dat")
nm <- read.ices("nm.dat")
sw <- read.ices("sw.dat")

pf <- read.ices("pf.dat")
pm <- read.ices("pm.dat")
surveys <- read.ices("survey.dat")

doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='COD') 
#load(file=paste0('Cod','_catch.Rdata'),verbose=T)

doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='COD')

all.surveys<-list(COD=surveys)

############################
# Whiting

la<-SMS@species.info['Whiting','last-age']

setwd(file.path(rawDataDir,'Whiting'))

cn1 <- read.ices("whi47d_ln.txt")
cn2 <- read.ices("whi47d_dn.txt")
cn3 <- read.ices("whi47d_ibn.txt")
cn<-cn1+cn2+cn3
ln<-cn1+cn3

cw1 <- read.ices("whi47d_lw.txt")
cw2 <- read.ices("whi47d_dw.txt")
cw3 <- read.ices("whi47d_ibw.txt")
cw<-(cw1*cn1+cw2*cn2+cw3*cn3)/cn
round(cw,3)

mo <- read.ices("whi47d_mo.txt")
nm <- read.ices("whi47d_nm.txt")

#sw <- read.ices("whi47d_sw.txt")  #no stock weights
sw<-cw

pf <- read.ices("whi47d_pf.txt")
pm <- read.ices("whi47d_pm.txt")
surveys <- read.ices("whi47d_ef.txt")


doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='WHG') 

doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='WHG')

all.surveys[['WHG']]<-surveys

############################
# haddock

la<-SMS@species.info['Haddock','last-age']

setwd(file.path(rawDataDir,'Haddock'))

cn <- read.ices("nor_had_cn.txt")
cw <- read.ices("nor_had_cw.txt")

c1 <- read.ices("nor_had_cn_dis.txt")
ln<-cn-c1

mo <- read.ices("nor_had_mo.txt")
nm <- read.ices("nor_had_nm.txt")
sw <- read.ices("nor_had_sw.txt")

pf <- read.ices("nor_had_pf.txt")
pm <- read.ices("nor_had_pm.txt")
surveys <- read.ices("nor_had_ef.txt")

all.surveys[['HAD']]<-surveys
doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='HAD') 
doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='HAD')

############################
# Saithe

la<-SMS@species.info['Saithe','last-age']

setwd(file.path(rawDataDir,'Saithe'))

cn <- read.ices("cn.dat")
cw <- read.ices("cw.dat")

lf <- read.ices("lf.dat")
ln<-cn*lf

mo <- read.ices("mo.dat")
nm <- read.ices("nm.dat")
sw <- read.ices("sw.dat")

pf <- read.ices("pf.dat")
pm <- read.ices("pm.dat")
surveys <- read.ices("survey.dat")
all.surveys[['POK']]<-surveys

doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='POK') 
doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='POK')


############################
# Mackerel

la<-SMS@species.info['Mackerel','last-age']

setwd(file.path(rawDataDir,'Mackerel'))

cn <- read.ices("cn.dat")
cw <- read.ices("cw.dat")

lf <- read.ices("lf.dat")
ln<-cn*lf

mo <- read.ices("mo.dat")
nm <- read.ices("nm.dat")
sw <- read.ices("sw.dat")

pf <- read.ices("pf.dat")
pm <- read.ices("pm.dat")
surveys <- read.ices("survey.dat")

all.surveys[['MAC']]<-surveys
doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='MAC') 
doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='MAC')

#####################################################
# Herring

la<-SMS@species.info['Herring','last-age']

setwd(file.path(rawDataDir,'Herring'))

cn <- read.ices("canum.txt")
cw <- read.ices("weca.txt")

ln<-cn

mo <- read.ices("matprop.txt")
nm <- read.ices("natmor.txt")
sw <- read.ices("west.txt")

pf <- read.ices("fprop.txt")
pm <- read.ices("mprop.txt")
surveys <- read.ices("fleet.txt")

all.surveys[['HER']]<-surveys
doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='HER') 
doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='HER')



############################
# Plaice
la<-SMS@species.info['Plaice','last-age']
setwd(file.path(rawDataDir,'Plaice'))

cn1 <- read.ices("dinum.txt")
ln <- read.ices("lanum.txt")
cn<-cn1+ln

cw1 <- read.ices("wedi.txt")
cw2 <- read.ices("wela.txt")
cw<-(cw1*cn1+cw2*ln)/cn

mo <- read.ices("matprop.txt")
nm <- read.ices("natmor.txt")
sw <- read.ices("west.txt")

pf <- read.ices("fprop.txt")
pm <- read.ices("mprop.txt")
surveys <- read.ices("fleet.txt")

doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='PLE') 
doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='PLE')

all.surveys[['PLE']]<-surveys

############################
# Sole
la<-SMS@species.info['Sole','last-age']
setwd(file.path(rawDataDir,'Sole'))

cn1 <- read.ices("dinum.txt")
ln <- read.ices("lanum.txt")
cn<-cn1+ln

cw1 <- read.ices("wedi.txt")
cw2 <- read.ices("wela.txt")
cw<-(cw1*cn1+cw2*ln)/cn

mo <- read.ices("matprop.txt")
nm <- read.ices("natmor.txt")
sw <- read.ices("west.txt")

pf <- read.ices("fprop.txt")
pm <- read.ices("mprop.txt")
surveys <- read.ices("fleet.txt")

doCatch(fa=fa,la=la,cn=cn,cw=cw,ln=ln,species='SOL') 
doBio01(fa=fa,la=la,mo=mo,sw=sw,nm=nm,cn=cn,species='SOL')

all.surveys[['SOL']]<-surveys