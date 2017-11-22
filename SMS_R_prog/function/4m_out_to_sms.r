
# change by user

fourMdir<-"NS_4_7_MAC_4m"
sp<- c("COD","HAD","HER","N_M","NOP","POK","SAN","W_M","WHG")
Species.n<-c(3,5,7,2,9,6,8,1,4)

fourMdir<-"Baltic_4m"
sp<- c("COD","HER","SPR")
Species.n<-c(1,2 ,3)


### summary.out

a<-read.table(file.path(root,fourMdir,"compar01.out"),comment.char = "#",header=T)

b<-data.frame(species=sp,Species.n=Species.n)
c<-merge(a,b)

d<-data.frame(Year=c$year,Species.n=c$Species.n,Age=c$age,M2=c$M2,F=c$F,Z=c$Z,
               N=c$N,N.bar=c$N*(1-exp(-c$Z))/c$Z)

write.table(d,file=file.path(root,fourMdir,"summary.out"),row.names=F,quote=F)


#  "summary_table_raw.out"
## mean F
F.age<-SMS.control@avg.F.ages
first.VPA


# change  by user
#d<-subset(c,Species.n>=3&year>=1975 & year<=2000)
d<-c

d<-data.frame(d,first=F.age[d$Species.n-first.VPA+1,1],last=F.age[d$Species.n-first.VPA+1,2])
d<-subset(d,age>=first &age<=last,select=c(Species.n,year,N,Z,F,age))
d<-data.frame(d,Nbar=d$N*(1-exp(-d$Z))/d$Z)
d<-data.frame(d,deadF=d$Nbar*d$F,deadTot=d$Nbar*d$Z)

# annual values Z
Z<-tapply(d$Z,list(d$year,d$Species.n,d$age),sum,na.rm = TRUE)

deadF<-tapply(d$deadF,list(d$year,d$Species.n,d$age),sum,na.rm = TRUE)
deadTot<-tapply(d$deadTot,list(d$year,d$Species.n,d$age),sum,na.rm =TRUE)

fi<-Z*deadF/deadTot
mean.F<-apply(fi,c(1,2),mean,na.rm = TRUE)
mean.F<-arr2dfny(mean.F)
names(mean.F)<-list("Year","Species.n","mean.F")

###
SOP<-tapply(c$YIELD,list(c$year,c$Species.n),sum)/1000
SOP<-arr2dfny(SOP)
names(SOP)<-list("Year","Species.n","SOP")

SOP.hat<-SOP
names(SOP.hat)<-list("Year","Species.n","SOP.hat")

Eaten<-tapply(c$DEADM2,list(c$year,c$Species.n),sum)/1000
Eaten<-arr2dfny(Eaten)
names(Eaten)<-list("Year","Species.n","Eaten")

tot<-merge(SOP,Eaten)
tot<-merge(tot,merge(mean.F,SOP.hat))

###
d<-subset(c,quarter==3 & age==0)

Rec<-tapply(d$N,list(d$year,d$Species.n),sum)/1000
Rec<-arr2dfny(Rec)
names(Rec)<-list("Year","Species.n","Rec")
tot<-merge(tot,Rec)

###
d<-subset(c,quarter==1)

TSB<-tapply(d$BIOMASS,list(d$year,d$Species.n),sum)/1000
TSB<-arr2dfny(TSB)
names(TSB)<-list("Year","Species.n","TSB")

SSB<-tapply(d$SSB,list(d$year,d$Species.n),sum)/1000
SSB<-arr2dfny(SSB)
names(SSB)<-list("Year","Species.n","SSB")

tot<-merge(tot,merge(SSB,TSB))

write.table(tot,file=file.path(root,fourMdir,"summary_table_raw.out"),row.names=F,quote=F)


