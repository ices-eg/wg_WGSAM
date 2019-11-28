a<-Read.size.preference()
a<-subset(a,size.model==11,select=c( Species ,size.ratio, size.var))
a$size.ratio<-round(a$size.ratio,2)
a$size.var<-round(a$size.var,2)
names(a)<-c('Species','mean','variance')
a
