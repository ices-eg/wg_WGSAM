
inFile<-file.path(data.path,'Risk_tab_minTAC_noise1_.out')

a<-read.table(file=inFile,header=T)
a$Year<-paste(a$fy,a$ly,sep='-')
a$fy<-a$ly<-NULL
head(a)

b<-reshape(a, direction = "long", varying = 2:4,v.names='value',timevar = "prob" )
b$prob<-paste('Prob',b$prob,sep='')
head(b)

ftable(round(tapply(b$value,list(Option=b$option,prob=b$prob,Years=b$Year),sum),2))

