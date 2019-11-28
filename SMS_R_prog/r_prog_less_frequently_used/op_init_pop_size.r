dat<-Read.summary.data(extend=T,read.init.function=F)

a<-subset(dat,Year %in% c(2009,2010,2011) & ((Quarter==1 & Age>0) | (Quarter==3 & Age==0)), select=c(Species.n,Year,Age,N,Quarter))
names(a)

ftable(tapply(a$N,list(a$Year,a$Species.n,a$Age),sum))

