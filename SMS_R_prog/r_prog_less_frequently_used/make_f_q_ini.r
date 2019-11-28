 # read VPA_CAO1 data, on the 4M format  (please note catches are in pure numbers (not thousands )
old<-read.table(file.path(root,"data_northSea","data_incl05","VPA_ca01.IN"),header = T)
fy.old<-min(old$year)
ly.old<-max(old$year)

sps<- c("Fulmar","Guillemot","Her. Gull","Kittiwake","GBB. Gull","Gannet","Puffin","Razorbill","R. radiata",
       "G. gurnards","W_M" ,"N_M" ,"W_H", "N_H" ,"COD" ,"WHG","HAD","POK","HER","SAN","NOP", "PLE" ,"SOL")

mer<-data.frame(year=0,species='dummy', spno=0,age=0,yearGroup=0,ageGroup=0 )

for (sp in (first.VPA:nsp)) {
  species<-sps[sp]
  sp.index<-sp-first.VPA+1
  max.season.age<-length(SMS.control@catch.season.age[[sp.index]])
  ageGroup<-1
  season.age<-SMS.control@catch.season.age[[sp-first.VPA+1]][ageGroup]
  for (age in (SMS.control@species.info[sp,"first-age F>0"]:SMS.control@species.info[sp,"last-age-selec"])) {
    yearGroup<-1
    max.yearGroup<-length(SMS.control@catch.sep.year[[sp.index]])
    for (year in (fy.old:ly.old) )  {
      if (year==SMS.control@catch.sep.year[[sp.index]][min(yearGroup+1,max.yearGroup)]) yearGroup<-min(yearGroup+1,max.yearGroup)
      #print(paste(species,age,ageGroup,year,yearGroup))
      mer<-rbind(mer, data.frame(year=year,species=species, spno=sp,age=age,yearGroup=yearGroup,ageGroup=ageGroup ))
     }
    if (season.age==age) {
       ageGroup<-min(ageGroup+1,max.season.age)
       season.age<-SMS.control@catch.season.age[[sp.index]][ageGroup]
    }
  }
}

a<-merge(old,mer)

b1<-aggregate(a$CATCHN,list(a$spno, a$species,a$ageGroup,a$yearGroup,a$quarter),mean)
names(b1)<-c('spno','species','ageGroup','yearGroup','quarter','C')
head(b1)

b2<-aggregate(b1$C,list(b1$spno,b1$species,b1$ageGroup,b1$yearGroup),sum)
names(b2)<-c('spno','species','ageGroup','yearGroup','sumC')
head(b2)

b3<-merge(b1,b2)
b3$prop<-b3$C/b3$sumC
fq<-subset(b3,spno>0, select=-c(sumC,C))

fq<-reshape(fq,idvar=c('spno','species','ageGroup','yearGroup'),direction='wide',timevar='quarter')

fq$ref4<-ifelse(is.na(fq$prop.2),0.50,0.25)
fq$p1<-fq$prop.1/fq$prop.4*fq$ref4
fq$p2<-fq$prop.2/fq$prop.4*fq$ref4
fq$p3<-fq$prop.3/fq$prop.4*fq$ref4
fq$p4<-fq$prop.4/fq$prop.4*fq$ref4
fq


ofile<-file.path(data.path,'F_q_ini.in')
#ofile<-''
cat("# relative F by quarter 1 - 4 by species, first age in separable season age group and first year in separable year group\n#\n",file=ofile)
aa<-1
yy<-1
sp<-1

for (a in (1:dim(fq)[1]))  {
 if (is.na(fq[a,'p1']))  cat (paste('           ',                          round(fq[a,'p3'],3),"\t#",round(fq[a,'p4'],2),fq[a,'species']),file=ofile,append=T)
 if (!is.na(fq[a,'p1'])) cat (paste(round(fq[a,'p1'],3),round(fq[a,'p2'],3),round(fq[a,'p3'],3),"\t#",round(fq[a,'p4'],2),fq[a,'species']),file=ofile,append=T)
 cat(paste(",  age:", SMS.control@catch.season.age[[sp]][aa]," year:",SMS.control@catch.sep.year[[sp]][yy],"\n"),file=ofile,append=T)
 yy<-yy+1
 if (length(SMS.control@catch.sep.year[[sp]])<yy) {
   yy<-1
   aa<-aa+1;
   if (length(SMS.control@catch.season.age[[sp]])<aa)  {sp<-sp+1; aa<-1; yy<-1; cat('#\n',file=ofile,append=T)}
 }
}

