dat<-Read.summary.data(extend=F,read.init.function=F)
sp<-1
s<-subset(dat,Species.n==sp)
av.F.age<-SMS.control@avg.F.ages[sp-first.VPA+1,]
s1<-subset(s,s$Age>=av.F.age[1] & s$Age<=av.F.age[2])
FI<-tapply(s1$F,list(s1$Year),sum)/(av.F.age[2]-av.F.age[1]+1)
FI<-c(FI,-1)
a<-read.table(file=file.path(data.path,"summary_table_raw.out"),header=TRUE)
a$old.Mean.F<-a$Mean.F
a$mean.F<-FI

write.table(a, file =file.path(data.path,"summary_table_raw.out"), row.names = F)

