
a<-read.table(file.path(data.path,"short-term-input.out"),skip=3,header=F,sep=',')
names(a)<-list("Age","west","weca","PM","FF","Ninp")


Catch<-read.table(file.path(data.path,"mcout_C.out"),header=T)

N<-read.table(file.path(data.path,"mcout_N.out"),header=T)
CN<-merge(Catch,N)

all<-merge(CN,a)
all<-subset(all,select=c(Age, Year, C,N,west,weca,PM),Year<=2012)
all$SSB<-all$N*all$west*all$PM
all$Yield<-all$C*all$weca
all$Ages<-paste("Age",all$Age)
head(all)
write.table(all,file=file.path(data.path,'Yield_SSB_at_age.csv'),row.names=F,col.names=T,sep=',')

cleanup()
 newplot(dev='screen',2,2)
 
by(all,list(all$Year),function(x)
 pie(x$SSB,labels=x$Ages,main=paste('SSB',x[1,'Year']),radius = 1)
)

by(all,list(all$Year),function(x)
 pie(x$Yield,labels=x$Ages,main=paste('Yield',x[1,'Year']),radius = 1)
)
