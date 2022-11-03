library(remotes)
#install_github("ices-tools-prod/icesTAF")
library(icesTAF)
# ?icesTA

library(stockassessment)
old.dir<-getwd()


SMS<-SMS.control  # option with a shorter name

fa<-SMS@first.age


# old catches
OldCat<-read.csv(file=file.path(root,exchangeDir,paste0('VPA_Ca01_',old_key_label,'.csv')))

SMS@species.info


NewCat<-subset(OldCat,species=='SPR')

NewCat$sub_area<-NewCat$cat<-NULL

OldBio<-read.csv(file=file.path(root,exchangeDir,paste0('VPA_Bi01_',old_key_label,'.csv')))


# copy old bio to newbio as a start
NewBio<-subset(OldBio,year>=firstY & species=='SPR')
b<-subset(NewBio,year==lastYold)
for (y in ((lastYold+1):lastY)) {
  b$year<-y
  NewBio<-rbind(NewBio,b)
}
NewBio$ICES_WSEA<- -1



source(file.path(root.prog,'r_prog','MakeANewEnvironment','read_csv_fish.R'))

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
  a$age<-as.numeric(a$age)
  a$year<-as.numeric(a$year)
  
  save(a,file=paste0(species,'_catch.Rdata'))
}


doBio01<-function(fa=fa,la=la,mo=mo,sw=sw,nm=nm,species='AAA') { 
  cat('Species:',species,'\n')
  a<-merge(arr2dfName(mo,idx=c('year','age'),name="PROPMAT"), arr2dfName(nm,idx=c('year','age'),name='M'))
  a<-merge(a,arr2dfName(sw,idx=c('year','age'),name='ICES_WSEA'))
  a<-subset(a,as.numeric(a$age)<=la)
  a$species<-species
  a$quarter<-9
  a$age<-as.numeric(a$age)
  a$year<-as.numeric(a$year)
  save(a,file=paste0(species,'_bio01.Rdata'))
}


updBIO<-function(sp_name=sp_name,sp=species,fdir=fdir,firstY=firstY,q=FALSE) {
  load(file=file.path(fdir,paste0(sp,'_bio01.Rdata')),verbose=F) #load a
  
  a<-subset(a,year>=firstY & year <=lastY,select=c(species,year,quarter,age,PROPMAT,M,ICES_WSEA)) 
  newYear<-sort(unique(a$year))
  if (!q) a$quarter<-NULL
  b<-subset(NewBio,(species==sp & year %in% newYear))
  if (q) a$newM<-a$M else a$newM<-a$M/4  
  #if (!q) a$newM<-a$M/4;
  a$newPROPMAT<-a$PROPMAT;
  a$M<-a$PROPMAT<-NULL
  a$newICES_WSEA<-a$ICES_WSEA; a$ICES_WSEA<-NULL
  if (q) ab<-merge(x=a,y=b,by=c("species","year","quarter","age"), all.y=T)

  if (!q) ab<-merge(x=a,y=b,by=c("species","year","age"), all.y=T)
  ab$PROPMAT<-ifelse(is.na(ab$newPROPMAT),ab$PROPMAT,ab$newPROPMAT)
  ab$M<-ifelse(is.na(ab$newM),ab$M,ab$newM)
  ab$ICES_WSEA<-ifelse(is.na(ab$newICES_WSEA),ab$ICES_WSEA,ab$newICES_WSEA)
  ab$newPROPMAT<-ab$newM<-ab$newICES_WSEA<-NULL
  NewBio<-subset(NewBio,!(species==sp & year %in% newYear))

  NewBio<-rbind(ab,NewBio)
  return(NewBio)
}

check_bio<-function(sp='COD',y=1992,a=3){
  subset(NewBio,species==sp & year==y  & age==a) 
}


updCATCH<-function(sp_name=sp_name,sp=species,fdir=fdir,firstY=firstY) {
  load(file=file.path(fdir,paste0(sp,'_catch.Rdata')),verbose=F) #load a
  a<-subset(a,year>=firstY & year<=lastY)
  newYear<-sort(unique(a$year))
  NewCat<-subset(NewCat,!(species==sp & year %in% newYear))
  NewCat<-rbind(a,NewCat)
  return(NewCat)
}

plot_bio<-function(oldD,oldLabel=paste(lastYold+1,'run',sep='-'),
                   newD,newLabel=paste(lastY+1,'run',sep='-'),
                   addD=NA,sp,years=firstY:lastY) {
  oldD$label<-oldLabel
  newD$label<-newLabel
  oldD<-subset(oldD,species==sp & year %in% years, select=c(year,species,quarter,age,WSEA, PROPMAT,M, M1,PROP_M2,label))
  newD<-subset(newD,species==sp & year %in% years, select=c(year,species,quarter,age,WSEA, PROPMAT,M, M1,PROP_M2,label))
  a<-rbind(oldD,newD)
  if (!is.na(addD)) a<-rbind(a,add)
  a<-as_tibble(a) %>% mutate(aq=paste0("Q",quarter," Age:", formatC(age,width=2)),year=as.double(year))
  
  p<-ggplot(a, aes(x=year, y=WSEA, group=label, color=label)) +
    geom_line()+
    geom_point()+
    scale_x_continuous(name="Year") +
    scale_y_continuous(name="Weight (Kg)")+
    
    facet_wrap(vars(aq))
  p
}





#Sprat from SMS assessment
species<-'SPR'
fdir <-file.path(root,exchangeDir,'ByStock',species,'SMS-data')
sp_name<-'Sprat'

la<-SMS@species.info[sp_name,'last-age']
cat(species,' last age:',la,'\n')

setwd(fdir)
a<-read.table(file.path(fdir,"summary.out"),header=TRUE)

a<-data.frame(year=a$Year,species=species,age=a$Age,PROP_CAT=1,quarter=a$Quarter,CATCHN=a$C.obs,WCATCH=a$weca,M=a$M,ICES_WSEA=a$west,PROPMAT=a$propmat)   
s1<-round(tapply(a$CATCHN*a$WCATCH,list(a$year,a$quarter),sum))
qShift<-function(a) {
  q12<-a$quarter %in% c(1,2)
  q34<-a$quarter %in% c(3,4)
  a[q12,'quarter']<-a[q12,'quarter']+2
  a[q34,'quarter']<-a[q34,'quarter']-2
  a[q34,'year']<-a[q34,'year']+1
  a[q34,'age']<-a[q34,'age']+1
  return(a)
}
a<-qShift(a)

s2<-round(tapply(a$CATCHN*a$WCATCH,list(a$year,a$quarter),sum))
cbind(s1,s2)
aa<-a
a<-subset(aa,year<=lastY ,select=c( year, species, age, PROP_CAT, quarter,WCATCH,CATCHN))

# plus group or not
#a[a$age==4,'age']<-3
a<-subset(a,age<=3)

a2<-aggregate(cbind(WCATCH,CATCHN,WCATCH*CATCHN)~year+ species+ age+ PROP_CAT+ quarter,data=a,sum)
a2$WCATCH2<-a2$V3/a2$CATCHN

subset(a2,age==3 &year==1975)
a[a$CATCHN<100,'CATCHN']<-0

# SPR year: 1974 Q1 & Q2 from old SMS
ad<-data.frame(year=1974,species='SPR',age=rep(0:3,2),PROP_CAT=1,quarter=rep(c(1,2),each=4),
               CATCHN=c(0.00, 6272325.12,790695.82,85688.62 ,0.00 ,  2222931.24,    155564.40 ,    13872.68),
               WCATCH=c(   0.0000, 0.0060,  0.0129,0.0428 ,0.0000 , 0.0055, 0.0113, 0.0390))

a<-rbind(a,ad)


# Q2 (North Sea) catches 
q2<-read.csv(file.path(root,exchangeDir,'ByStock',species,"SPRAT_NS_July_to_june_quarterly_catch_in_numbers_and_mean_weight_benchmark_IV_inc_Q2_2019.csv")) 
q2<-subset(q2,year>=1975 & year<=lastY)
head(q2)
a0<-q2; a0$age<-0; a0$WCATCH2<-q2$mw0; a0$CATCHN2<-q2$n0;
a1<-q2; a1$age<-1; a1$WCATCH2<-q2$mw1; a1$CATCHN2<-q2$n1;
a2<-q2; a2$age<-2; a2$WCATCH2<-q2$mw2; a2$CATCHN2<-q2$n2;
a3<-q2; a3$age<-3; a3$WCATCH2<-q2$mw3; a3$CATCHN2<-q2$n3; 
q2<-rbind(a0,a1,a2,a3)
q2<-subset(q2,quarter %in% c(4),select=c(year,quarter,age,CATCHN2,WCATCH2))
head(q2)
q2<-qShift(q2)
head(q2)
q2[q2$age==4,'age']<-3
q2<-aggregate(CATCHN2 ~year+quarter+age,data=q2,sum)
q2$quarter<-NULL

a<-merge(x=a,y=q2,all.x=TRUE)
a$difC<-a$CATCHN-a$CATCHN2

head(subset(a,year==1980 & age==2),20)
head(subset(a,year==1976 & quarter==3),20)
head(subset(a,year==1976 & quarter==4),20)


key<-a$quarter==2 & !is.na(a$CATCHN2) 
summary(key)

a[key,'CATCHN']<-a[key,'CATCHN2']
key<-a$quarter==1 & !is.na(a$CATCHN2) & a$difC>0
a[key,'CATCHN']<-a[key,'CATCHN']-a[key,'CATCHN2']
head(subset(a,year==1980 & age==2),20)
 a$CATCHN2<-a$difC<-NULL
summary(a)
subset(a,CATCHN<0)


ftable(round(tapply(a$CATCHN,list(a$year,a$quarter,a$age),sum)))
save(a,file=paste0(species,'_catch.Rdata'))

a<-subset(aa,year<=lastY & age <=la,select=c( year, quarter, species, age, M, ICES_WSEA,PROPMAT))


#update catch  and bio data
NewCat<-updCATCH(sp_name=sp_name,sp=species,fdir=fdir,firstY=firstY) 


# special proportion mature
ap<-subset(a,quarter==3,select=c(year, species, age,PROPMAT))
ap$newPROPMAT<-ap$PROPMAT
ap$PROPMAT<-NULL
a<-merge(x=a,y=ap,all.x=TRUE)
head(subset(a,quarter==1))
a$PROPMAT<-a$newPROPMAT
a$newPROPMAT<-NULL
save(a,file=paste0(species,'_bio01.Rdata'))
#

check_bio(sp=species)
NewBio<-updBIO(q=TRUE,sp_name=sp_name,sp=species,fdir=fdir,firstY=firstY)
check_bio(sp=species)

#Sprat survey
a<-read.table(file.path(fdir,'catch_survey_residuals.out'),header=TRUE)
a<-subset(a,fleet>0)
head(a)
a<-data.frame(year=a$Year,quarter=a$Quarter,species=species,age=a$Age,fleet=a$fleet, obs=a$observed/1000)
unique(paste(a$fleet,'Q:',a$quarter))  
a<-qShift(a)

head(a)
data.frame(fleet=1:3,name=c("IBTS_Q1","IBTS_Q3","Acoustic"))
a<-merge(a,data.frame(fleet=1:3,fname=c("IBTS_Q1","IBTS_Q3","Acoustic")))

unique(paste(a$fleet,'Q:',a$quarter))
ftable(round(tapply(a$obs,list(a$fleet,a$year,a$age),sum)))

fl2<-by(a,a$fname,function(x) { 
  y<-tapply(x$obs,list(x$year,x$age),sum)
  attr(y,"time")<-c(0,1)
  return(y)
})
all.surveys<-list()
all.surveys[[species]]<-fl2






# tst<-subset(NewCat,species=='POK'); tapply(tst$CATCHN*tst$WCATCH,list(tst$year),sum)

b<-expand.grid(species=unique(NewCat$species),year=unique(NewCat$year),age=unique(NewCat$age),quarter=unique(NewCat$quarter))
b<-merge(x=NewCat,y=b,all.y=T)
b[is.na(b$CATCHN),'CATCHN']<-0
b[is.na(b$WCATCH),'WCATCH']<-0
b<-b[order(b$species,b$year,as.numeric(b$age)+10,b$quarter),]
p<-1
for (i in (2:dim(b)[[1]])) {
  if (b[i,'species'] %in% c('COD','WHG','HAD','POK','MAC','PLE','SOL')) {
    if (b[i,'quarter']==1) p<-b[i,'PROP_CAT'] else b[i,'PROP_CAT']<-p 
  }
}

NewCat<-b

#head(subset(NewCat,species=='COD' & year==1989 & age==7),20)
# tst<-subset(NewCat,species=='POK'); tapply(tst$CATCHN*tst$WCATCH,list(tst$year),sum)
# tapply(b$CATCHN*b$WCATCH,list(b$species,b$year),sum)



vpa.code<-       c('COD','WHG','HAD','POK','MAC','HER','NSA','SSA','NOP','SPR','PLE','SOL')
la<-SMS@species.info[SMS@species.info[,'last-age-selec']>0,'last-age']
spOrder<-data.frame(nr=1:length(vpa.code),species=vpa.code,la=la)

fy<-SMS@first.year
ly<-SMS@last.year

a<-merge(NewCat,spOrder)

qPresent<-1   # 1:4

a<-subset(a,quarter %in% qPresent)
x<-a$CATCHN
la<-SMS@species.info[SMS@species.info[,'last-age-selec']>0,'last-age']
b<-tapply(x,list(a$species,a$year,a$quarter,a$age),sum,na.rm=T)
# b
dim(b)

# tst<-subset(a,species=='POK'); tapply(tst$CATCHN*tst$WCATCH,list(tst$year),sum)


### change mean weight using the old MSVPA data for the youngest ages, and scale the old MSVPA mean weight to the used single species mean weights 


old<-read.table(file=file.path(root,"Data_NorthSea","Old_data",'NS_1998_all_sp','VPA_B01.DAT'),header=T)
a<-subset(old, year==1974 & age <=10 & ((age>=1) | (age==0 & quarter %in% c(3,4))),select=c(-year))

ftable(round(tapply(a$WSEA,list(a$species,a$quarter,a$age),sum),3))

# mean w
b<-aggregate(WSEA~species+age,mean,data=a)
head(b)
b$meanWSEA<-b$WSEA; b$WSEA<-NULL
head(b)
subset(b,species=='COD' & age==3 )

# minimum W
b2<-aggregate(WSEA~species+age,min,data=a)
head(b2)
b2$minWSEA<-b2$WSEA; b2$WSEA<-NULL
head(b2)
subset(b2,species=='COD' & age==3 )

b<-merge(b,b2)

a<-subset(a,species %in% c('COD','WHG','HAD','POK','MAC','HER','PLE','SOL'),select=c(species,quarter,age,WSEA))
head(a)
subset(a,species=='WHG' & age==1 )

ab<-merge(x=a,y=b,all.x=T)
ab$wFac<-ab$WSEA/ab$meanWSEA
ab$wFacMin<-ab$WSEA/ab$minWSEA
head(ab)
subset(ab,species=='COD' & age==3 )

oldwsea<-subset(ab, select=c(species, quarter,age, WSEA))
oldwsea$oldWsea<-oldwsea$WSEA; oldwsea$WSEA<-NULL
head(oldwsea)
subset(oldwsea,species=='COD' & age==3 )

NewBio2<-merge(x=NewBio,y=oldwsea,all.x=T)
head(NewBio2)


ab<-subset(ab, species=='COD' & age>=3 | species=='WHG' & age >=1 | species=='HAD' & age>2 | species=='POK' & age>3 | species=='MAC' & age>2 |
             species=='HER' & age>2 | species=='PLE' & age >0 | species=='SOL' & age >0,
           select=c(species,quarter,age,wFac,wFacMin))
head(ab)
subset(ab,species=='WHG')

#ftable(round(tapply(ab$wFac,list(ab$species,ab$quarter,ab$age),sum),3))
#ftable(round(tapply(NewBio2$ICES_WSEA,list(NewBio2$species,NewBio2$quarter,NewBio2$age),mean),3))


b<-merge(x=NewBio2,y=ab,all.x=TRUE)
sort(unique(b$species))

bmean<-subset(b,!(species %in% c('WHG')))
sort(unique(bmean$species))


bmin<-subset(b,species %in% c('WHG'))
sort(unique(bmin$species))

#subset(bmean,year==1978 & species=='COD' & age==4)
bmean$WSEA<-ifelse(is.na(bmean$wFac) | is.na(bmean$ICES_WSEA),bmean$WSEA, ifelse(bmean$ICES_WSEA<=0,bmean$WSEA,bmean$wFac*bmean$ICES_WSEA))
bmean[bmean$WSEA<1.12 & bmean$species=='COD' & bmean$age=='3','WSEA']<-1.125
#subset(bmean,year==1978 & species=='COD' & age==4)

subset(bmin,year==1983 & species=='WHG' & age==4)
bmin$WSEA<-ifelse(is.na(bmin$wFacMin) | is.na(bmin$ICES_WSEA),bmin$WSEA, ifelse(bmin$ICES_WSEA<=0,bmin$WSEA,bmin$wFacMin*bmin$ICES_WSEA))
subset(bmin,year==1983 & species=='WHG' & age==4)
b78<-subset(bmin,year==1978 & species=='WHG')
bb78<-subset(b78,select=c(species,quarter,age,WSEA))
bb78$newWSEA<-bb78$WSEA; bb78$WSEA<-NULL

b7477<-subset(bmin,year<1978 & species=='WHG')
head(bb78)
bb<-merge(x=b7477,y=bb78,all.x=TRUE)

bb$WSEA<-bb$newWSEA; bb$newWSEA<-NULL;
bmin<-subset(bmin,!(year<1978 & species=='WHG'))
bmin<-rbind(bb,bmin)
sort(unique(bmin$species))
#ftable(round(tapply(bmin$WSEA,list(bmin$year,bmin$quarter,as.numeric(bmin$age)+10),sum),3))

b<-rbind(bmean,bmin)

b[b$WSEA<=0,'WSEA']<-NA
ftable(round(tapply(b$WSEA,list(b$species,b$quarter,as.numeric(b$age)+10),mean),3))
head(b)
head(NewBio)

NewBio<-subset(b,select=names(NewBio))

# b<-subset(NewBio,species=='WHG')
# ftable(round(tapply(b$WSEA,list(b$year,b$quarter,as.numeric(b$age)+10),mean),3))



#####################################################################################
# make basis input catch file for 2020 keyrun
CAT.01<-NewCat
save(CAT.01,file=file.path(root,exchangeDir,'CAT_01.Rdata'))


# Biological data (Wsea,propmat, M and M1) and an new variable (proportion of the stock within area 4 (the North Sea))
BIO.01<-NewBio 

# subset(NewBio,species=='NOP' & age==1 & year==1987)

save(BIO.01,file=file.path(root,exchangeDir,'BIO_01.Rdata'))

