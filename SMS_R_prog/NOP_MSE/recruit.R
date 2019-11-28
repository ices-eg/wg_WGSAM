
scen<-'scen_20'
a<-read.table(file=file.path(data.path,scen,'mcout_n.out'),header=T)
head(a)
a<-subset(a,Age==0 & Quarter==4)
a$N<-a$N/1000

# 
tapply(a$N,list(a$Year),mean)
mean(a$N)
tapply(a$N,list(a$Year),median)
median(a$N)
exp(mean(log(a$N))) # GM
hist(subset(a,Year==2024)$N)

# SESAM
load(file='SSB_R.Rdata',verbose=T)
head(SSB)
rec<-SSB$Rec
hist(rec,main='SESAM recruitment')
hist(log(rec),main='SESAM recruitment')
mean(rec)
median(rec)
exp(mean(log(rec))) # GM
