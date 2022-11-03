# this program reads data from an existing (non updated) SMS and updates data with new data 
# the my.stock.dir path should point to the directory with the old (non updated) data 
# after running this script, you have to change the my.stock.dir to the new key-run directory (by running init.r) and import the new data by From_list_to_SMS_format_config.R; 
 
# general settings
# directory for data input (and output from this script)
start<-file.path('c:','MV','SMS','data_northSea','2014-data')

SAS.out<-file.path('C:','MV','Bootsmave','SAS-NorthSea','SMS-input')   # output from the old stomach , catch and bio processing program

last.data.year<-2010
new.last.year<-2013
new.first.year<-1974


# read existing input files and transport them into the old 4M exchange format
source(file.path(prog.path,"From_SMS_format_to_list.r"))
# this script write the files  VPA_Bi01.in,  VPA_Ca01.in  in the default data directory 
From_SMS_format_to_list(otherPredExist=T,catchMultiplier=1000,code.name=c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'COD','WHG','HAD','POK','HER','SAN','NOP','SPR','PLE','SOL'))

CA<-read.csv(file.path(data.path,'VPA_Ca01.in')); CA.old<-CA   ;   CA.old<-subset(CA.old,!(species=='SPR' & age==4))
BIO<-read.csv(file.path(data.path,'VPA_Bi01.in'));BIO.old<-BIO ;  BIO.old<-subset(BIO.old,!(species=='SPR' & age==4))

other<-read.csv(file.path(data.path,'other_sp.dat'));
meanl<-read.csv(file.path(data.path,'mean_l.dat'));
cons<-read.csv(file.path(data.path,'consum.dat'));


#expand time series;
a<-subset(CA,year==last.data.year)
b<-subset(BIO,year==last.data.year)
o<-subset(other,year==last.data.year)
l<-subset(meanl,year==last.data.year)
co<-subset(cons,year==last.data.year)

for(y in (last.data.year+1):new.last.year) {
  a$year<-y; CA<-rbind(CA,a)
  b$year<-y; BIO<-rbind(BIO,b)
  o$year<-y; other<-rbind(other,o)
  l$year<-y; meanl<-rbind(meanl,l)
  co$year<-y; cons<-rbind(cons,co)
}


Update_catch_bio<-function(a,sp='HAD',sp.long='Haddock',first.year=1974, last.year=2013, IBC=T , n.mult=1000,obj="had-346a FLStock object.Rdata",FLname=NULL,CA=NULL,BIO=NULL) {

  plusgroup<-a@range[3]
  if (is.na(plusgroup)) plusgroup<-100

  b<-as.data.frame(a)
  b1<-droplevels(subset(b,age!='all' & year>=first.year & year<=last.year,select=c(year,age,data,slot)))
  head(b1)
  
  b2<-reshape(b1,direction='wide',idvar=c('year','age'),timevar='slot')
  n<- names(b2)
  for (i in 1:20) {
   n<-sub("data.","",n)
  }
  names(b2)<-n
  
  if (IBC) b2$IBC.n<-b2$catch.n-(b2$landings.n+b2$discards.n) else  b2$IBC.n<-0
  b2$species<-sp
  
  
  b2.sum<-droplevels(subset(b,age=='all' & year>=first.year & year<=last.year,select=c(year,age,data,slot)))
  head(b2.sum)
  
  b2.sum<-reshape(b2.sum,direction='wide',idvar=c('year','age'),timevar='slot')
  n<- names(b2.sum)
  for (i in 1:10) {
   n<-sub("data.","",n)
  }
  names(b2.sum)<-n
  
  if (IBC) b2.sum$IBC<-b2.sum$catch-(b2.sum$landings+b2.sum$discards) else b2.sum$IBC<-0
  b2$prop.land<-(b2$landings.n+b2$IBC.n)/b2$catch.n
  b2$prop.land[b2$catch.n==0]<- 1
  #b2<<-b2
  CA.sp<-subset(CA,CA$species==sp)
  CA.sp$update<-FALSE
  CA.sp[CA.sp$year>=first.year & CA.sp$year<=last.year,'update'] <-T
  CA.sp<-merge(CA.sp,subset(b2,select=c(year,species,age,catch.n,catch.wt,prop.land)),by=c('year','species','age'))
  CA.sp[CA.sp$update,'CATCHN']<-CA.sp[CA.sp$update,'catch.n']*n.mult
  CA.sp[CA.sp$update & CA.sp$quarter!=1,'CATCHN']<-0
  CA.sp[CA.sp$update,'WCATCH']<-CA.sp[CA.sp$update,'catch.wt']
  CA.sp[CA.sp$update,'PROP_CAT']<-CA.sp[CA.sp$update,'prop.land']
  
  SMS.plus<-SMS.control@species.info[sp.long,'last-age']
  if (plusgroup < SMS.plus ) {
    CA.sp<-subset(CA.sp, (update==F | (update==T  &  age<=plusgroup)) )
    if (first.year<SMS.control@first.year) { #Change plusgroup for the period before update years
      CA.sp[CA.sp$update==F & CA.sp$species==sp & CA.sp$age>=plusgroup,'age']<-plusgroup
      CA.sp$CW<-CA.sp$WCATCH * CA.sp$CATCHN
      CA.sp$Cprop<-CA.sp$PROP_CAT * CA.sp$CATCHN
      
      CA.sp<-aggregate( cbind(CATCHN,CW,Cprop)~ year+ species+ quarter+ sub_area +fleet+ cat +age,data=CA.sp,sum)
      CA.sp$WCATCH<- CA.sp$CW/CA.sp$CATCHN
      CA.sp$PROP_CAT<-CA.sp$Cprop/CA.sp$CATCHN
      CA.sp$CW<-CA.sp$Cprop<-NULL
   }
  }

  CA.sp[CA.sp$age>plusgroup,'PROP_CAT']<-0
  CA.sp$update<-CA.sp$catch.n<-CA.sp$catch.wt<-CA.sp$prop.land<-NULL
  CA<-rbind(subset(CA,sp!=sp),CA.sp)

  
  BIO.sp<-subset(BIO,BIO$species==sp)
  BIO.sp$update<-FALSE
  BIO.sp[BIO.sp$year>=first.year & BIO.sp$year<=last.year , 'update'] <-T
  BIO.sp<-merge(x=BIO.sp,y=subset(b2,select=c(year,species,age,mat)),by=c('year','species','age'),all.x = TRUE)
  BIO.sp[BIO.sp$update & !is.na(BIO.sp$mat),'PROPMAT']<-BIO.sp[BIO.sp$update & !is.na(BIO.sp$mat),'mat']
  
  if (plusgroup < SMS.plus ) {   # adjust mean weight in the sea of the plus group(
    BIO.sp[BIO.sp$age>=plusgroup,'age']<-plusgroup
    BIO.sp.W<-aggregate( WSEA~ year+ species+ quarter+ sub_area  +age,data=BIO.sp,subset=BIO.sp$age>=plusgroup,mean)
    BIO.sp.W2<-aggregate( WSEA~ year+ species+         sub_area  +age,data=BIO.sp.W,mean)
    names(BIO.sp.W2)<- sub('WSEA','MW',names(BIO.sp.W2))
    BIO.sp.W<-merge(BIO.sp.W,BIO.sp.W2)
    BIO.sp.W<-merge(x=BIO.sp.W,y=subset(b2,select=c(year,species,age,stock.wt)),by=c('year','species','age'),all.x = TRUE)
    
    BIO.sp.W[!is.na(BIO.sp.W$stock.wt),'WSEA'] <-BIO.sp.W[!is.na(BIO.sp.W$stock.wt),'WSEA']/BIO.sp.W[!is.na(BIO.sp.W$stock.wt),'MW']*BIO.sp.W[!is.na(BIO.sp.W$stock.wt),'stock.wt']
   
    head(BIO.sp.W)
    BIO.sp.W$MW<-BIO.sp.W$stock.wt<-NULL
    names(BIO.sp.W)<- sub('WSEA','newWSEA',names(BIO.sp.W))
    BIO.sp<-merge(x=BIO.sp,y=BIO.sp.W,all.x=TRUE)
    BIO.sp[!is.na(BIO.sp$newWSEA),'WSEA']<-BIO.sp[!is.na(BIO.sp$newWSEA),'newWSEA']
  }

  
  BIO$update<-NULL
  BIO$mat<-NULL
  return(list(BIO,CA))
}


 extract.flr <-defmacro(obj,FLname,expr={
   print(load(file=file.path(start,obj),verbose=TRUE))
   return(FLname)
   }
  )


old<-CA.old



plot.dif<-defmacro(old,new,years=2000:2002,ages=0:8,sp='HAD',minmax=NULL,variable=a,vars='catch N',expr={
  old2<-droplevels(subset(old,age %in% ages & year %in% years & species==sp))
  a<-tapply(old2$variable,list(old2$year,old2$age),sum)
  # print(a)
  new2<-droplevels(subset(new,age %in% ages & year %in% years & species==sp))
  b<-tapply(new2$variable,list(new2$year,new2$age),sum)
  #print(b)
  ba<-b/a
  #print(ba)
  if (vars=='CANUM') {
    png(filename =file.path(start,paste('comp_',sp,'.png',sep='')), width = 1200, height = 1400,units = "px", pointsize = 30, bg = "white")
    par(mfcol=c(2,1))
  } 
  plot(dimnames(ba)[[1]],(ba)[,1],type='b',pch=dimnames(ba)[[2]][1],col=1,ylim=minmax,xlab='',ylab="new:old",main=paste(sp,vars))
  abline(a=1,b=0)
  for (i in (2:dim(ba)[2])) lines(x= dimnames(ba)[[1]],y=ba[,i], type='b',pch=dimnames(ba)[[2]][i],col=i)
  if (vars!='CANUM') cleanup()
} )

update.BIO.CA<-function(sp){
  BIO<<-rbind(subset(BIO.CA[[1]],species==sp),subset(BIO,species!=sp))
  CA<<-rbind(subset(BIO.CA[[2]],species==sp),subset(CA,species!=sp))
}

# Cod

setwd(file.path(start,'cod','data'))
FL <- readFLStock(file="index.txt",name='cod',no.discards =TRUE)
FL@landings.n<-readVPAFile( "lf.dat",quiet = FALSE) # landings
FL@discards.n[]<- FL@catch.n[]-FL@landings.n[]
setwd(data.path)
#summary(FL)
a<-FL@catch.n

#round(FL@landings.n,0)
#round(FL@catch.n,0)

ul<-c(0.90,0.98,1.10,0.93,0.84,0.70,0.77,0.92,1.33,1.12,1.68,1.20,1.18) # unallocated 1993-2005 
ul<-c(rep(1,1992-1963+1),ul,rep(1,2013-2006+1))

UL<-FL@discards.n # copy structure
UL[]<- 1
UL[]<-rep(ul,each=15)


FL@landings.n[]<-FL@landings.n[]*UL[]
FL@catch.n[]<-FL@catch.n[]*UL[]
FL@discards.n[]<-FL@discards.n[]*UL[]
# round(a/FL@catch.n,2)
#round(FL@catch.n,0)
#round(FL@catch.wt,3)
FL@catch.wt[is.na(FL@catch.wt)]<-0  

FL<-setPlusGroup(FL, 10)
round(FL@catch.n,0)
round(FL@catch.wt,3)
BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='COD',sp.long='Cod',first.year=1963, last.year=2013, IBC=F , n.mult=1000)
new<-BIO.CA[[2]]
cleanup()

plot.dif(old,new,years=1963:2010,ages=1:7,sp='COD',minmax=c(0.5,1.5),variable=CATCHN,vars='CANUM') 
plot.dif(old,new,years=1963:2010,ages=1:7,sp='COD',minmax=c(0.5,1.5),variable=WCATCH,vars='sum of WECA') 
update.BIO.CA(sp='COD')


#whiting

setwd(file.path(start,'whiting'))
FL <- readFLStock(file="whi47d_idx.txt",name='Whi',no.discards =F)  ; summary(FL)



FL@landings.n<-readVPAFile( "whi47d_ln.txt",quiet = FALSE) # landings
FL@discards.n[]<- FL@catch.n[]-FL@landings.n[]
setwd(data.path)
summary(FL)
a<-FL@catch.n

round(FL@landings.n,0)
round(FL@catch.n,0)

round(FL@catch.n,0)
round(FL@catch.wt,3)
FL@catch.wt[is.na(FL@catch.wt)]<-0  

FL<-setPlusGroup(FL, 8)
round(FL@catch.n,0)
round(FL@catch.wt,3)
BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='WHG',sp.long='Whiting',first.year=1978, last.year=2013, IBC=T , n.mult=1000)
new<-BIO.CA[[2]]
cleanup()

plot.dif(old,new,years=1978:2010,ages=1:8,sp='WHG',minmax=c(0.5,1.5),variable=CATCHN,vars='CANUM') 
plot.dif(old,new,years=1978:2010,ages=1:8,sp='WHG',minmax=c(0.5,1.5),variable=WCATCH,vars='sum of WECA') 
update.BIO.CA(sp='WHG')


if (FALSE) {
  FL<-extract.flr("whit47_FLStockObject.Rdata",FLname=x.stock) ;summary(FL)
  FL@catch.n-FL@landings.n-FL@discards.n
  BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='WHG',sp.long='Whiting',first.year=1990, last.year=2013, IBC=T , n.mult=1000)
  new<-BIO.CA[[2]]
  
  plot.dif(old,new,years=1990:2010,ages=1:8,sp='WHG',minmax=c(0.5,1.5),variable=CATCHN,vars='CANUM') 
  plot.dif(old,new,years=1990:2010,ages=1:8,sp='WHG',minmax=c(0.5,1.5),variable=WCATCH,vars='sum of WECA') 
  update.BIO.CA(sp='WHG')
}


#CAW<-droplevels(subset(CA,species=='WHG'))
#ftable(tapply(CAW$PROP_CAT,list(CAW$year,CAW$quarter,CAW$age),sum) )

# haddock

if (FALSE) {
  FL<-extract.flr("had-346a FLStock object.Rdata",FLname=x.pg) ;summary(FL)
  BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='HAD',sp.long='Haddock',first.year=1972, last.year=2013, IBC=T , n.mult=1000)
  new<-BIO.CA[[2]]
  plot.dif(old,new,years=1972:2010,ages=0:8,sp='HAD',minmax=c(0.5,3),variable=CATCHN,vars='CANUM') 
  plot.dif(old,new,years=1972:2010,ages=0:8,sp='HAD',minmax=c(0.5,2),variable=WCATCH,vars='sum WCATCH') 
  update.BIO.CA(sp='HAD')
}

setwd(file.path(start,'haddock'))
FL <- readFLStock(file="index.txt",name='Had',no.discards =T,quiet = F)  ; summary(FL)   ; #it does not work !



FL@landings.n<-readVPAFile( "nor_had_cn_lan.txt",quiet = FALSE) # landings
FL@discards.n<-readVPAFile( "nor_had_cn_dis.txt",quiet = FALSE) # discard
FL@catch.n<-readVPAFile( "nor_had_cn.txt",quiet = FALSE) # catch

FL@landings.wt<-readVPAFile( "nor_had_cw_lan.txt",quiet = FALSE) # landings
FL@discards.wt<-readVPAFile( "nor_had_cw_dis.txt",quiet = FALSE) # discard
FL@catch.wt<-readVPAFile( "nor_had_cw.txt",quiet = FALSE) # catch

setwd(data.path)

FL@catch.wt[is.na(FL@catch.wt)]<-0  

FL<-setPlusGroup(FL, 10)
summary(FL)
round(FL@catch.n,0)
round(FL@catch.wt,3)
BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='HAD',sp.long='Haddock',first.year=1974, last.year=2013, IBC=F , n.mult=1000)
new<-BIO.CA[[2]]
cleanup()

plot.dif(old,new,years=1974:2010,ages=1:10,sp='HAD',minmax=c(0.5,1.5),variable=CATCHN,vars='CANUM') 
plot.dif(old,new,years=1974:2010,ages=1:10,sp='HAD',minmax=c(0.5,1.5),variable=WCATCH,vars='sum of WECA') 
update.BIO.CA(sp='HAD')




# Saithe
FL<-extract.flr("Saithe xsa.stock.object.Rdata",FLname=xsa.stock) ;summary(FL)
FL@catch.n-FL@landings.n-FL@discards.n   # IBC included ?
BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='POK',sp.long='Saithe',first.year=1967, last.year=2013, IBC=F , n.mult=1000)
new<-BIO.CA[[2]]
plot.dif(old,new,years=1967:2010,ages=3:10,sp='POK',minmax=c(0.2,2),variable=CATCHN,vars='CANUM') 
plot.dif(old,new,years=1967:2010,ages=3:10,sp='POK',minmax=c(0.5,2),variable=WCATCH,vars='sum WCATCH') 
update.BIO.CA(sp='POK')

 
#herring
setwd(file.path(start,'NS-herring'))
FL <- readFLStock(file="index.txt",name='Herring')
summary(FL)
setwd(data.path)
FL<-setPlusGroup(FL, 7)
FL@catch.n<-FL@landings.n
FL@catch.wt<-FL@landings.wt
BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='HER',sp.long='Herring',first.year=1963, last.year=2013, IBC=F , n.mult=1000)
new<-BIO.CA[[2]]
plot.dif(old,new,years=1963:2010,ages=0:7,sp='HER',minmax=c(0.2,1.5),variable=CATCHN,vars='CANUM') 
plot.dif(old,new,years=1963:2010,ages=0:7,sp='HER',minmax=c(0.5,1.5),variable=WCATCH,vars='sum WCATCH') 
update.BIO.CA(sp='HER')


# Plaice
FL<-extract.flr("ple-nsea_XSA_Results.Rdata",FLname=xsa.stock) ;summary(FL)
FL@catch.n-FL@landings.n-FL@discards.n   # IBC included ?
BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='PLE',sp.long='Plaice',first.year=1963, last.year=2013, IBC=F , n.mult=1000)
new<-BIO.CA[[2]]
plot.dif(old,new,years=1963:2010,ages=1:10,sp='PLE',minmax=c(0.2,1.5),variable=CATCHN,vars='CANUM') 
plot.dif(old,new,years=1963:2010,ages=1:10,sp='PLE',minmax=c(0.5,1.5),variable=WCATCH,vars='sum WCATCH') 
update.BIO.CA(sp='PLE')


# Sole
FL<-extract.flr("sol-nsea_XSA_Results.Rdata",FLname=xsa.stock) ;summary(FL)
#FL@catch.n-FL@landings.n-FL@discards.n   # IBC included ?
BIO.CA<-Update_catch_bio(a=FL,BIO=BIO,CA=CA,sp='SOL',sp.long='Sole',first.year=1963, last.year=2013, IBC=F , n.mult=1000)
new<-BIO.CA[[2]]
plot.dif(old,new,years=1963:2010,ages=1:10,sp='SOL',minmax=c(0.2,1.5),variable=CATCHN,vars='CANUM') 
plot.dif(old,new,years=1963:2010,ages=1:10,sp='SOL',minmax=c(0.5,1.5),variable=WCATCH,vars='sum WCATCH') 
update.BIO.CA(sp='SOL')


############
la<-SMS.control@max.age.all
fa<-SMS.control@first.age
years<-c(1,1)
years[1]<-SMS.control@first.year
years[2]<-SMS.control@last.year
ny<-years[2]-years[1]+1
npr<-sum(SMS.control@species.info[,'predator']>=1)
nsp<-SMS.control@no.species
nq<-SMS.control@last.season
noAreas<-SMS.control@no.areas

#############  catch data
read_catch_bio<-function(data.path=data.path,years=c(1967,2014),quarters=1:2,ages=1:3,noAreas=1,species='COD',catchMultiplier=1000,propLandExist=F) {

  data.length<-(years[2]-years[1]+1)*length(quarters)*length(ages)*noAreas; print(data.length)
  CATCHN<-scan(file.path(data.path,'canum.in'),comment.char='#')[1:data.length]
  WCATCH<-scan(file.path(data.path,'weca.in'),comment.char='#')[1:data.length]
  if (propLandExist) {
    Prop.landed<-scan(file.path(data.path,'proportion_landed.in'),comment.char='#')[1:data.length]
  } else {
    Prop.landed<-WCATCH
    Prop.landed[]<-1
  }
  b<-expand.grid(sub_area=1:noAreas,species.n=1,year=years[1]:years[2],quarter=quarters,age=ages,fleet=1,cat='A')
  b$species<-species
  b<-b[order(b$sub_area,b$species.n,b$year,b$quarter,b$age),]
  b<-data.frame(b,CATCHN=CATCHN,WCATCH=WCATCH,PROP_CAT=Prop.landed)
  b<-subset(b,select=c(year,species,quarter,sub_area,fleet,cat,age,WCATCH,CATCHN,PROP_CAT))
  b$CATCHN<-catchMultiplier*b$CATCHN
  catch<-b
  
  #write.table(b,file=file.path(data.path,'VPA_Ca01.in'),row.names = F,quote = T,sep=',')
  
  ############## bio data
  
  WSEA<-scan(file.path(data.path,'west.in'),comment.char='#')[1:data.length]
  head(WSEA)
   
  PROPMAT<-scan(file.path(data.path,'propmat.in'),comment.char='#')[1:data.length]
  M<-scan(file.path(data.path,'natmor.in'),comment.char='#')[1:data.length]
  #M1<-scan(file.path(data.path,'natmor1.in'),comment.char='#') [1:data.length]
  M1<-M   # will be overwritten later on

  b<-expand.grid(sub_area=1:noAreas,species.n=1,year=years[1]:years[2],quarter=quarters,age=ages)
  b$species<-species
  b<-b[order(b$sub_area,b$species.n,b$year,b$quarter,b$age),]
  
  b<-data.frame(b,WSEA=WSEA, PROPMAT=PROPMAT,M=M,M1=M1)
  b<-subset(b,select=c(year,species,quarter,age,sub_area,WSEA,PROPMAT,M,M1))
  #write.table(b,file=file.path(data.path,'VPA_Bi01.in'),row.names = F,quote = T,sep=',')
  bio<-b
  return(list(BIO=bio,CA=catch))
}

# North South Sandeel
sa1<-read_catch_bio(data.path=file.path(start,"Industri","Input_data","tobis_A1"),years=c(1983,2013),quarters=1:2,ages=0:4,noAreas=1,species='SA1',catchMultiplier=1000)
sa2<-read_catch_bio(data.path=file.path(start,"Industri","Input_data","tobis_A2"),years=c(1983,2013),quarters=1:2,ages=0:4,noAreas=1,species='SA1',catchMultiplier=1000)
sa3<-read_catch_bio(data.path=file.path(start,"Industri","Input_data","tobis_A3"),years=c(1983,2013),quarters=1:2,ages=0:4,noAreas=1,species='SA1',catchMultiplier=1000)


 
read.SAN.SXSA<-function(my.file,vari='C',catchMultiplier=1000) {  
  tob_old<-file.path(start,'Industri','Input_data','Tobis_99_one_stock')
  g<-read.table(file=file.path(tob_old,my.file),header=F)
  names(g)<-c('year','half','fleet','a0','a1','a2','a3','a4')

  g<-reshape(g,direction='long',idvar=c('year','half','fleet'),varying = list(4:8), v.names = vari)
  if (vari=='CATCHN') g$CATCHN<-g$CATCHN*catchMultiplier
  if (vari=='WCATCH') g$WCATCH<-g$WCATCH/1000
  g$age<-g$time-1
  g$time<-NULL
  return(g)
}

ns.C<-read.SAN.SXSA(my.file='CANUM4.hyr',vari='CATCHN')
ns.W<-read.SAN.SXSA(my.file='WECA4.hyr',vari='WCATCH')
subset(ns.W,age==4)


ns.C<-merge(ns.C,ns.W)
subset(ns.C,CATCHN>0 & WCATCH==0 & !(age==0 & half==1))

ns.C$species<-'AAA'
ns.C[ns.C$fleet==1,'species']<-'NSA'
ns.C[ns.C$fleet==2,'species']<-'SSA'
ns.C$fleet<-1
ns.C$quarter<-2
ns.C[ns.C$half==2,'quarter']<-3
ns.C$sub_area<-1
ns.C$cat<-'A'
ns.C$PROP_CAT<-1


ns.C2<-subset(ns.C,year<1983,select=c(year,species,quarter,sub_area,fleet,cat,age,WCATCH,CATCHN,PROP_CAT))

g<-read.csv(file=file.path(start,'Industri','Sandeel_C_BY_HALF_year.csv')) 
g$n_samples<-NULL

g<-reshape(g,direction='long',idvar=c('Area','aar','hy'),varying = list(4:14), v.names = 'vari')
g$age<-0;g$CATCHN<-0
g[g$time>=1 &g$time<=5,'age']<-g[g$time>=1 & g$time<=5,'time']-1 
g[g$time>=1 &g$time<=5,'CATCHN']<-g[g$time>=1 & g$time<=5,'vari'] 
g.C<-subset(g,g$time>=1 &g$time<=5,select=c(Area,aar, hy,age,CATCHN))

g[g$time>=6 &g$time<=10,'age']<-g[g$time>=6 & g$time<=10,'time']-6 
g[g$time>=6 &g$time<=10,'WCATCH']<-g[g$time>=6 & g$time<=10,'vari'] 
g.W<-subset(g,g$time>=6 &g$time<=10,select=c(Area,aar, hy,age,WCATCH))
head(g.C)
head(g.W)
g.C<-merge(g.C,g.W)
g.C$species<-'AAA'
g.C[g.C$Area==3 |g.C$Area==4 | g.C$Area==5 | g.C$Area==7,'species']<-'NSA'
g.C[g.C$Area==1 |g.C$Area==2 ,'species']<-'SSA'
subset(g.C,species=='AAA')
g.C$fleet<-1
g.C$quarter<-2
g.C[g.C$hy==2,'quarter']<-3
g.C$sub_area<-1
g.C$cat<-'A'
g.C$PROP_CAT<-1
g.C$year<-g.C$aar

g.C<-subset(g.C,year>1982 & Area !=6,select=c(year,species,quarter,sub_area,fleet,cat,age,WCATCH,CATCHN,PROP_CAT)) 


g.C<-aggregate(cbind(CATCHN,WCATCH*CATCHN) ~ year+species+quarter+sub_area+fleet+cat+age+PROP_CAT, data=g.C,sum)
g.C$WCATCH<-0
g.C[g.C$V2>0,'WCATCH']<-g.C[g.C$V2>0,'V2']/g.C[g.C$V2>0,'CATCHN'] 
g.C$V2<-NULL


san.C<-rbind(ns.C2,g.C)
san.C$CATCHN<-san.C$CATCHN*1000

#just checking
a1<-tapply(san.C$CATCHN*san.C$WCATCH,list(san.C$year,san.C$species),sum)
a2<-rowSums(a1)
ftable(round(cbind(a1,a2),0))
ftable(round(tapply(san.C$WCATCH,list(san.C$species,san.C$year,san.C$quarter,san.C$age),sum),3))
ftable(round(tapply(san.C$WCATCH*san.C$CATCHN,list(san.C$species,san.C$year,san.C$quarter,san.C$age),sum),0))

# mean weight in the sea,start with the old ones
w<-subset(BIO,species=='SAN' & year > 2000 & age<=4);
w<-aggregate(WSEA~quarter+age,mean,data=w)
round(tapply(w$WSEA,list(w$quarter,w$age),sum),4)

# new, n-s
san.bio<-subset(san.C,WCATCH>0)   # use wcatch as wsea
w.new<-aggregate(WCATCH~quarter+age+species,mean,data=san.bio)
ftable(round(tapply(w.new$WCATCH,list(w.new$species,w.new$quarter,w.new$age),sum),4))

w.new2<-w.new
w.new2[w.new2$quarter==2,'quarter']<-1
w.new2[w.new2$quarter==3,'quarter']<-4
w.new<-rbind(w.new,w.new2)
w.new[w.new$age==0 & w.new$quarter %in% c(1,2),'WCATCH']<-0
w.new[w.new$age==0 & w.new$quarter==3,'WCATCH']<-w.new[w.new$age==0 & w.new$quarter==3,'WCATCH']/2
ftable(round(tapply(w.new$WCATCH,list(w.new$species,w.new$quarter,w.new$age),sum),4))

b<-subset(BIO,species=='SAN' & year>=new.first.year,select=c(year, species, quarter, age, sub_area,PROPMAT,M1,M,WSEA ))
SSA<-b; SSA$species<-'SSA'
NSA<-b; NSA$species<-'NSA'
a<-rbind(NSA,SSA)
a<-merge(a,w.new)
a$WSEA<-a$WCATCH; a$WCATCH<-NULL
a[a$species=='NSA' & a$age==1,'PROPMAT']<-0.05
a[a$species=='NSA' & a$age==2,'PROPMAT']<-0.77
a[a$species=='SSA' & a$age==1,'PROPMAT']<-0.02
a[a$species=='SSA' & a$age==2,'PROPMAT']<-0.83

M<-subset(sa1$BIO,year==2010,select=c(quarter,age,M))
ftable(tapply(M$M,list(M$quarter,M$age),sum))
M$M<-M$M/2
Ma<-M
M[M$quarter==2,'quarter']<-3
M[M$quarter==1,'quarter']<-2
Ma[Ma$quarter==2,'quarter']<-4
M<-rbind(M,Ma)
ftable(tapply(M$M,list(M$quarter,M$age),sum))

a$M<-NULL

a<-merge(a,M)

san.bio<-a
ftable(round(tapply(san.bio$WSEA,list(san.bio$species,san.bio$year,san.bio$quarter,san.bio$age),sum),4))
ftable(round(tapply(san.bio$M,list(san.bio$species,san.bio$year,san.bio$quarter,san.bio$age),sum),2))

ftable(round(tapply(san.bio$PROPMAT,list(san.bio$species,san.bio$year,san.bio$quarter,san.bio$age),sum),2))

###### 
# Sprat 
BIO.CA<-read_catch_bio(data.path=file.path(start,"Industri","Input_data","Brisling_NS"),years=c(1974,2014),quarters=1:4,ages=0:3,noAreas=1,species='SPR',catchMultiplier=1000)
a<-BIO.CA[['CA']]
shiftyear<-function(a) {
  a[a$quarter==1,'q']<-3
  a[a$quarter==2,'q']<-4
  a[a$quarter==3,'q']<-1
  a[a$quarter==4,'q']<-2
  
  a[a$quarter==3,'year']<-a[a$quarter==3,'year']+1
  a[a$quarter==4,'year']<-a[a$quarter==4,'year']+1
  a$quarter<-a$q
  a$q<-NULL
  return(a)
}

BIO.CA[['CA']]<-shiftyear(BIO.CA[['CA']])
BIO.CA[['BIO']]<-shiftyear(BIO.CA[['BIO']])
update.BIO.CA(sp='SPR')
#####
# Norway pout

g<-read.table(file=file.path(start,'Industri','NOP','CANUM.QRT'),header=F) 
g<-reshape(g,direction='long',idvar=c('V1','V2','V3'),varying = list(4:8), v.names = 'C')

g2<-read.table(file=file.path(start,'Industri','NOP','WECA.QRT'),header=F) 
g2<-reshape(g2,direction='long',idvar=c('V1','V2','V3'),varying = list(4:8), v.names = 'W')

g<-merge(g,g2)

g$year<-g$V1
g$quarter<-g$V2
g$age<-g$time-1
g$sub_area<-
g$fleet<-1
g$PROP_CAT<-1
g$cat<-'A'
g$CATCHN<-g$C
g$WCATCH<-g$W
g$species<-'NOP'
g$CATCHN<-g$CATCHN*1000*1000
g$WCATCH<-g$WCATCH/1000
catch.NOP<-subset(g,year>=1974,select=c(  year, age, quarter, sub_area, fleet, cat, species,WCATCH, CATCHN, PROP_CAT))
tapply(catch.NOP$WCATCH*catch.NOP$CATCHN/1000000,list(catch.NOP$year),sum)


g1<-read.table(file=file.path(start,'Industri','NOP','WEST.QRT'),header=F) 
g1<-reshape(g1,direction='long',idvar=c('V1','V2','V3'),varying = list(3:7), v.names = 'WSEA')

g2<-read.table(file=file.path(start,'Industri','NOP','natmor.QRT'),header=F) 
g2<-reshape(g2,direction='long',idvar=c('V1','V2','V3'),varying = list(3:7), v.names = 'M')

g3<-read.table(file=file.path(start,'Industri','NOP','matprop.QRT'),header=F) 
g3<-reshape(g3,direction='long',idvar=c('V1','V2','V3'),varying = list(3:7), v.names = 'PROPMAT')

g<-merge(g1,g2)
g<-merge(g,g3)

g$year<-g$V1
g$quarter<-g$V2
g$age<-g$time-1
g$sub_area<- 1
g$M1<-g$M
g$species<-'NOP'

g$WSEA<-g$WSEA/1000
bio.NOP<-subset(g,year>=1974,select=c(year,age,quarter,sub_area,species,WSEA,PROPMAT,M,M1))
BIO.CA<-list(BIO=bio.NOP,CAA=catch.NOP)
update.BIO.CA(sp='NOP')

#head(BIO.CA[[1]])

#################
# put it all togheter
bio<-rbind(BIO,san.bio) 
bio<-droplevels(subset(bio,species!='SAN'))
unique(bio$species)
head(bio)
head(BIO.old)
BIO.old<-droplevels(subset(BIO.old,species != "SAN"))
a<-merge(x=bio,y=BIO.old,by=c("year","species", "age", "quarter", "sub_area"),all=TRUE) # use the old time series, where new data does not exist
a$WSEA<-a$WSEA.x ; a[is.na(a$WSEA.x),'WSEA']<-a[is.na(a$WSEA.x),'WSEA.y'] 
a$PROPMAT<-a$PROPMAT.x ; a[is.na(a$PROPMAT.x),'PROPMAT']<-a[is.na(a$PROPMAT.x),'PROPMAT.y']
a$M<-a$M.x ; a[is.na(a$M.x),'M']<-a[is.na(a$M.x),'M.y']


a$M1<-NULL



m1<-data.frame(species=c("COD", "WHG", "HAD",  "POK", "HER",  "NSA", "SSA", "NOP",  "SPR",  "PLE", "SOL"),
               M1=     c(0.05,  0.05,  0.05,   0.05,  0.025,   0.05, 0.05,   0.05,   0.05,  0.025, 0.025))
dim(a)
a<-merge(a,m1)
dim(a)

head(a)
bio<-subset(a,year>=1974 & year<=2013,select=c(year,species,age,quarter,sub_area,WSEA,PROPMAT,M,M1))
write.table(bio,file=file.path(root,'data_northSea','SMS-input-2014','VPA_Bi01.in'),row.names=F,sep=',')


#####

catch<-rbind(CA,san.C)
round(tapply(catch$CATCHN*catch$WCATCH/1000,list(catch$year,catch$species),sum))

catch<-droplevels(subset(catch,species!='SAN' ))
unique(catch$species)
catch$PROP_landed<-catch$PROP_CAT; catch$PROP_CAT<-NULL

tail(subset(catch,species=='NSA' & year==2013))
head(CA.old)
CA.old<-droplevels(subset(CA.old,species != "SAN"))
unique(droplevels(CA.old$species))
a<-merge(x=catch,y=CA.old,by=c("year","species", "age", "quarter", "sub_area", "fleet","cat"),all=TRUE) # use the old tim series, where new data does not exist
a$WCATCH<-a$WCATCH.x ; a[is.na(a$WCATCH.x),'WCATCH']<-a[is.na(a$WCATCH.x),'WCATCH.y'] 
a$CATCHN<-a$CATCHN.x ; a[is.na(a$CATCHN.x),'CATCHN']<-a[is.na(a$CATCHN.x),'CATCHN.y']
a[is.na(a$PROP_landed),'PROP_landed']<-a[is.na(a$PROP_landed),'PROP_CAT'] 
catch<-subset(a,year>=1974 & year<=2013,select=c(  year, species, age, quarter, sub_area, fleet, cat, WCATCH, CATCHN, PROP_landed))
head(catch)

round(tapply(catch$CATCHN*catch$WCATCH/1000,list(catch$year,catch$species),sum))
round(tapply(catch$CATCHN*catch$WCATCH/1000,list(catch$year,catch$species),sum,na.rm=T))



CAW<-droplevels(subset(catch,species=='WHG'))
ftable(tapply(CAW$PROP_landed,list(CAW$year,CAW$quarter,CAW$age),sum) )

write.table(catch,file=file.path(root,'data_northSea','SMS-input-2014','VPA_Ca01.IN'),row.names=F,sep=',')

#########################  
## other species
tab.other<-function(x) {
  round(tapply(x$N,list(x$species,x$year),sum)/1000000)
}
tab.other2<-function(x) {
  round(tapply(x$N*x$WSEA,list(x$year,x$species),sum)/4000000)
}
a.other<-read.table(file=file.path(start,"raj_gur_anna.dat"),header=T)   # data from Anna with update of the full time series
a.other<-subset(a.other,species %in% c('RAJ','GUR') & year %in% (1974:2013))
a.other$sub_area<-1
tab.other(a.other)
tab.other2(a.other)
a<-tapply(a.other$N*a.other$WSEA,list(a.other$year,a.other$species),sum)/4000000
apply(a,2,mean)


other<-subset(other,!(species %in% c('RAJ','GUR')))
other$species.n<-NULL
other<-rbind(other,a.other)
tab.other(other)

o.mac<-read.table(file.path(SAS.out,'other_sp.dat'),header=T)
o.mac<-subset(o.mac,species %in% c('W_M','N_M','W_H','N_H'))
tab.other(o.mac)
o.mac$spno<-NULL
o.mac$sub_area<-1

other<-subset(other,!(species %in% c('W_M','N_M','W_H','N_H') ) )
other<-droplevels(rbind(other,o.mac))
tab.other(other)

HKE<-read.table(file.path(start,'SMS_hake_abundance_Cormon_Kempf.dat'),header=T)
HKE<-subset(HKE,select=c(year,quarter, species, age,SMS_area,N, WSEA))
HKE$sub_area<-HKE$SMS_area; HKE$SMS_area<-NULL
other<-rbind(HKE,other)
tab.other(other)
other<-subset(other,year>=new.first.year)
write.table(other,file=file.path(root,'data_northSea','SMS-input-2014','other_sp.dat'),row.names=F,sep=',')
  
##  mean length; 

meanl.new<-read.table(file.path(SAS.out,'mean_l_sas.dat'),header=T)
meanl.new<-subset(meanl.new,species %in% c('NSA','SSA','W_M'))    # W_M added after the the 2014 WGSAM meeting
meanl.new$species.n<-meanl.new$spno;   meanl.new$spno<-NULL
meanl.new$SMS_area<-1

meanl<-subset(meanl,!(species %in% c('SAN','W_M')))            # W_M added after the the 2014 WGSAM meeting
meanl<-rbind(meanl,meanl.new)
meanl$species.n<-NULL

HKE<-read.table(file.path(start,'SMS_hake_abundance_Cormon_Kempf.dat'),header=T)
HKE<-subset(HKE,select=c(year,quarter, species, age,SMS_area,LSEA))
HKE$mean_l<-HKE$LSEA; HKE$LSEA<-NULL
#HKE$sub_area<-HKE$SMS_area; HKE$SMS_area<-NULL
head(meanl)
head(HKE)
meanl<-rbind(meanl,HKE)
meanl<-subset(meanl,year>=new.first.year)
write.table(meanl,file=file.path(root,'data_northSea','SMS-input-2014','mean_l.dat'),row.names=F,sep=',')

## consum

cons$species.n<-NULL
a<-unique(subset(cons,select=c(year, quarter, SMS_area)))
dim(a)
HKEc<-read.table(file.path(start,'saithe_hake_consumption_final.txt'),header=T)
HKEc$CONSUM<-HKEc$consumperquarter
HKEc<-subset(HKEc,select=c(quarter,age, CONSUM ))
HKEc$species<-'HKE'
HKEcc<-merge(a,HKEc)

ftable(round(tapply(HKEcc$CONSUM,list(HKEcc$year,HKEcc$quarter,HKEcc$age),sum),3))

cons<-rbind(cons,HKEcc)
cons<-subset(cons,year>=new.first.year)

# cons of W mac based on cons=a(q)*w^b
a<-c(0.237,0.529,	0.821,0.527)  # from cons_a_b.in file in 2005 run, The application gives cons quite close to the fixed vales used (consum.in)
b<-0.654

wm<-subset(other,species=='W_M',select=-N)
wm$SMS_area<-wm$sub_area;  wm$sub_area<-NULL
wm$CONSUM<-a[wm$quarter]*wm$WSEA^b
wm$WSEA<-NULL
summary(wm)

cons<-subset(cons,species!='W_M')
cons<-rbind(wm,cons)

write.table(cons,file=file.path(root,'data_northSea','SMS-input-2014','consum.dat'),row.names=F,sep=',')



 
 
 ###################
 a<-subset(catch,species=='SPR')
 round(tapply(a$CATCHN*a$WCATCH/1000,list(a$year,a$quarter),sum,na.rm=T))
 
  a<-subset(catch,species=='NSA')
 round(tapply(a$CATCHN*a$WCATCH/1000,list(a$year,a$quarter),sum,na.rm=T))

  a<-subset(catch,species=='SSA')
 round(tapply(a$CATCHN*a$WCATCH/1000,list(a$year,a$quarter),sum,na.rm=T))
