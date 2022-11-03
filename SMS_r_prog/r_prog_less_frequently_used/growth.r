
dat<-Read.summary.data.areas()
#dat<-Read.summary.data(read.init.function=F) ; dat$Area<-1 ; dat$size<-dat$west; dat$avail.food<-1; npr<-1

#  Species Area Year Quarter Species.n Age   M1          M2                  Z        N    N.bar pred.w prey.w
#  pred.growth.q pred.growth.y prey.growth.q prey.growth.y     DeadM2  DeadAll avail.food.q avail.food.y

g<-subset(dat,Species.n<=npr,
  select=c(Species,Area,Year,Quarter,Species.n,Age,pred.w,pred.growth.q,pred.growth.y,avail.food.q,avail.food.y))
g$cohort<-g$Year-g$Age


g<-subset(g, & Area==1)
head(g)
g$lab<-paste(sp.names[g$Species.n],g$Age)
g$labq<-paste(sp.names[g$Species.n],g$Age,"Q:",g$Quarter)

###


trellis.device()
xyplot(pred.w~Year|lab,data=g,layout = c(2, 3), subset=(Year>1993 & Age>2),
  scales = "free", main='mean weight all quarters',
   panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)

trellis.device()
gg<-subset(g, Year>1993 & cohort>1989 & cohort<2006)
xyplot(pred.w~Year|as.factor(cohort),data=gg,layout = c(4, 4),,
     panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='b')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)

# Ford Walford
trellis.device()
gg<-subset(g, Year>1993 & cohort>1989 & cohort<2006 & pred.growth.y> -10 & Quarter==1 & Age>1)
gg$wtPlus1<-gg$pred.w+gg$pred.growth.y
gg$l<- (gg$pred.w/8.7E-9)^0.333
gg$l1<-(gg$wtPlus1/8.7E-9)^0.333


xyplot(l1~l|as.factor(cohort),data=gg,layout = c(4, 4),xlab="l(t)",ylab="l(t+1)",
    main="Ford Walford by cohort",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    #panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)

xyplot(wtPlus1~pred.w|as.factor(cohort),data=gg,layout = c(4, 4),xlab="w(t)",ylab="w(t+1)",
    main="Ford Walford by cohort",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)


res<-by(gg,list(gg$cohort),function(x) {
   lm(l1~l,data=x)
})

lapply(res,summary)

a<-lapply(res,coef)


res<-by(gg,list(gg$cohort),function(x) {
 co<-coef(lm(l1~l,data=x))
 list(K=-log(co[[2]]),linf=co[[1]]/(1-co[[2]]))
})
res

plot(gg$l,gg$l1,xlab="l(t)",ylab="l(t+1)")
a<-lm(l1~l,data=gg)
summary(a)
co<-coef(a)
K<- -log(co[[2]])
linf<- co[[1]]/(1-co[[2]])

##############
gg<-subset(g,  Age>0 )

a<-tapply(gg$pred.w,list(Year=gg$Year,cohort=gg$cohort),mean)
matplot(a,type='b',ylab="(kg)", main="Mean weights over 4 quarters by cohort")

a<-tapply(gg$pred.w,list(Year=gg$Year,Age=gg$Age),mean)

ftable(round(tapply(gg$pred.w,list(Quarter=gg$Quarter,Year=gg$Year,Age=gg$Age),mean),3))

matplot(a,type='b',ylab="weight (kg)",  main="Mean weights over 4 quarters")
grid()

trellis.device()
xyplot(pred.growth.y~avail.food.y|lab,data=g,layout = c(2, 3), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)


trellis.device()
xyplot(avail.food.q~Year|labq,data=g,layout = c(4, 5), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free", lattice.options=list(as.table=T),
  panel=function(x,y){
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)

trellis.device()
xyplot(pred.growth.y~avail.food.y|labq,data=g,layout = c(4, 5), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)


trellis.device()
xyplot(pred.growth.q~avail.food.q|labq,data=g,layout = c(4, 5), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)


gg<-subset(g,Year>1993 & avail.food.y>1 & Age>2 & Age<8)

res<-by(gg,list(gg$labq),function(x) {
   glm(pred.growth.y~avail.food.y,data=x)
})

lapply(res,summary)
data.frame(lapply(res,coef))


res<-by(gg,list(gg$labq),function(x) {
   glm(pred.growth.q~avail.food.q,data=x)
})

lapply(res,summary)
data.frame(lapply(res,coef))


##########  divide availeble food by stock density (in this case SSB)

d<-Read.summary.data()
ssb<-aggregate(SSB~Species+Year+Quarter,data=d,sum)
gg<-merge(g,ssb)


trellis.device()
xyplot(pred.growth.q~avail.food.q/SSB|labq,data=gg,layout = c(4, 5), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)

## SSB skulle være årlig værdi !
trellis.device()
xyplot(pred.growth.y~avail.food.y/SSB|labq,data=gg,layout = c(4, 5), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)



trellis.device()
xyplot(pred.growth.q~SSB|labq,data=gg,layout = c(4, 5), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)

## SSB skulle være årlig værdi !
trellis.device()
xyplot(pred.growth.y~SSB|labq,data=gg,layout = c(4, 5), subset=(Year>1993 & avail.food.y>1 & Age>2 & Age<8),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)





#####  Ford Walfor & Stefan
dat<-Read.summary.data(read.init.function=F)

dat<-subset(dat,Species.n==1 & Quarter==1)

FordWalfordGrowth<-function(size,dt) {
  Linf<-1.16  # in meter ?
  K<-2.27
  b<-exp(-K*dt)
  a<-Linf*(1-b)
  l<-a+b*size
  return(l)
}


  # Cod l-weight 8.7E-9*len^3.0
  
FordWalfordGrowth(0.50,1)

minAge<-1
maxAge<-10

time<-seq(minAge:maxAge)
length<-time

length[1]<-0.10   # age 1 cod is assumed 10 cm  is is length or weight ?
for (t in ((minAge+1):maxAge) ) length[t]<- FordWalfordGrowth(length[t-1],1)

plot(time,length)

