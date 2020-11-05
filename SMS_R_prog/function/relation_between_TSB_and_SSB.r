dat<-Read.summary.data()
unique(dat$Species)
dat<-droplevels(subset(dat,Species %in% c("N. sandeel","S. sandeel","Nor. pout" , "Sprat") & Quarter==1))
bio<-aggregate(list(TSB=dat$BIO/1000,SSB=dat$SSB/1000),list(Year=dat$Year,Species=dat$Species),sum)

b1<-subset(bio,select=c(Year,Species,TSB))

bio$Year<-bio$Year+1

b2<-subset(bio,select=c(Year,Species,SSB))

b<-merge(b1,b2)

b$ratio<-b$TSB/b$SSB
head(b)
aggregate(ratio~Species,data=b,mean)