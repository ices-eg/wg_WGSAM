
a<- Read.summary.data()

a<-subset(a,Year>=1975)
b<-subset(a,Species=='Cod' )
M2<-aggregate(M2~Year+Age,data=b,sum)

a[a$Species %in% c('Grey seal','H. porpoise'),'SSB'] <-a[a$Species %in% c('Grey seal','H. porpoise'),'N']
SSB<-droplevels(subset(a,Species %in% c('Cod','Grey seal','Whiting') & Quarter==1))
COD<-aggregate(SSB~Year+Species,data=SSB,sum)

CODs<-aggregate(list(maxSSB=COD$SSB),list(Species=COD$Species),max)
#COD<-merge(COD,CODs)
#COD$SSB<-COD$SSB/COD$maxSSB
#COD$maxSSB<-NULL

COD<-reshape(COD,v.names='SSB',timevar='Species',idvar='Year',direction="wide")
names(COD)<-c("Year","COD","GSE","WHG")

a<-merge(M2,COD)
a<-merge(a,M2)
a<-droplevels(subset(a,Age<=3))

cleanup()
newplot(dev='screen',nox=3,noy=4)
by(a,list(a$Age),function(x) {
 plot(x$COD,x$M2,type='b', ylab='M2',xlab='Cod SSB')
 title(main=paste('Age',x[1,'Age']))
})
by(a,list(a$Age),function(x) {
 plot(x$WHG,x$M2,type='b', ylab='M2',xlab='WHG SSB')
 title(main=paste('Age',x[1,'Age']))
})
by(a,list(a$Age),function(x) {
 plot(x$GSE,x$M2,type='b', ylab='M2',xlab='GSE SSB')
 title(main=paste('Age',x[1,'Age']))
})

########################   new try

M2<-Read.part.M2.data()
M<-droplevels(subset(M2,Prey=='Cod' & Prey.age<=3,select=c(Predator,Prey,Year,Quarter,Prey.age,Part.M2)))
M<-aggregate(list(Part.M2=M$Part.M2),list(Year=M$Year,Prey.Age=M$Prey.age,Pred=M$Predator),sum)
head(M)

a<- Read.summary.data()
a$SSB<-a$SSB/1000
a$BIO<-a$BIO/1000
#head(subset(a,Species=='G. gurnards')

a<-subset(a,Quarter==1 & Species %in% c('Grey seal','Cod','Whiting','G. gurnards','H. porpoise') )
a[a$Species %in% c('Grey seal','H. porpoise'),'SSB'] <-a[a$Species %in% c('Grey seal','H. porpoise'),'N']
a[a$Species %in% c('Grey seal','H. porpoise'),'BIO'] <-a[a$Species %in% c('Grey seal','H. porpoise'),'N']
a[a$Species %in% c('G. gurnards'),'SSB'] <-a[a$Species %in% c('G. gurnards'),'BIO']

b<-subset(a,select=c(Species,Age,Year, SSB,BIO))  
b<-aggregate(list(SSB=a$SSB,TSB=a$BIO),list(Year=a$Year,Pred=a$Species),sum)
head(b)

ab<-merge(M,b)
#
#  Year        Pred Prey.Age     Part.M2      SSB    TSB
#1 1963         Cod        0 0.379811633 140015.3 319475
#2 1963         Cod        3 0.006765105 140015.3 319475


ab<-droplevels(ab)
xyplot(Part.M2~Year|paste(factor(Prey.Age),Pred),data=ab, #subset=Year>1981,
 scales=list(relation='free') ,
 panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
       }
)

xyplot(Part.M2~SSB|paste(factor(Prey.Age),Pred),data=ab, #subset=Year>1981,
 scales=list(relation='free') ,
 panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
       }
)



by(ab,list(ab$Prey.Age,ab$Pred),function(x) {
 #aa<-glm(Part.M2*100~Pred:SSB,data=x,family=Gamma(link = "inverse"))
 aa<-glm(Part.M2*100~SSB,data=x)
  print(summary(aa))
})

