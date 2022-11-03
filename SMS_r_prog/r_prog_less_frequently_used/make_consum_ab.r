dat<-Read.summary.data(read.init.function=F)

dat0<-droplevels(subset(dat,ration>0 & west>0.0,select=c(Year,Species,Species.n,Quarter,Age,west,ration,BIO)))

mm<-aggregate(Age~Species,data=dat0,min)
names(mm)<-c('Species','minAge')
ma<-aggregate(Age~Species,data=dat0,max)
mm<-merge(mm,ma)
head(mm)
mm$Same<-mm$minAge==mm$Age
mm2<-subset(mm,!Same,select=c(Species))


dat1<-merge(dat0,mm2)

mac<-droplevels(subset(dat1,Species=='N. mackerel'))
mac<-droplevels(subset(dat1,Species=='N.horse mac'))
ftable(tapply(mac$ration,list(mac$Year,mac$Quarter,mac$Age),mean))
ftable(tapply(mac$west,list(mac$Year,mac$Quarter,mac$Age),mean))

  
trellis.device(device="png", filename=file.path(data.path,'consum.png'),width = 1200, height = 1600,units = "px", pointsize = 40)

xyplot(log(ration)~log(west)|paste(Species,", Q:",Quarter,sep=''),
       auto.key = list(points = T, rectangles = F, space = "right"),scales = "free",
       panel = function(x,y) {
         panel.grid(h=-1, v= -1)
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, data=dat1,subset=ration>0 & west>0)

dev.off()


xyplot(log(ration)~log(west)|Species+ Quarter,
       auto.key = list(points = T, rectangles = F, space = "right"),scales = "free",
       panel = function(x,y) {
         panel.grid(h=-1, v= -1)
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, data=dat1)

xyplot(ration~west| paste(Species,", Q:",Quarter,sep=''),
  auto.key = list(points = T, rectangles = F, space = "right"),scales = "free",
         panel = function(x,y) {
           panel.grid(h=-1, v= -1)
           panel.xyplot(x, y)
            panel.lmline(x, y)
       }, data=dat1)



dat1$Year<-NULL
dat1<-unique(dat1)


# not used
a<-by(dat1,list(dat1$Species.n),function(x) {
 #lm(log(ration)~as.factor(Quarter)+as.factor(Quarter)*log(west)-1,data=x)
    lm(log(ration)~as.factor(Quarter)+log(west)-1,data=x)
})


aa<-by(dat1,list(dat1$Species.n,dat1$Quarter),function(x) {
  c(x[1,"Species.n"],x[1,"Quarter"])
})
lapply(aa,print)

aa<-as.data.frame(matrix(unlist(aa),byrow=T,ncol=2))
names(aa)<-c('Species.n','Quarter')

a<-by(dat1,list(dat1$Species.n,dat1$Quarter),function(x) {
  lm(log(ration)~log(west),weights=BIO,data=x)
})
lapply(a,summary)

b<-lapply(a,coefficients)
bb<-as.data.frame(matrix(unlist(b),byrow=T,ncol=2))
names(bb)<-c('a','b')
 dd<-cbind(aa,bb)
dd$a<-exp(dd$a)


mm2<-subset(mm,Same,select=c(Species))
dat1<-merge(dat0,mm2)
dat1<-subset(dat1,Year==2013,select=c(Species.n, Quarter,ration))
names(dat1)<-c('Species.n','Quarter', 'a')
dat1$b<-0

dd<-rbind(dd,dat1)
dd<-dd[order(dd$Species.n,dd$Quarter),]

cat('a in consumption=a*w^b\n')
a<-tapply(dd$a,list(dd$Species,dd$Quarter),sum)
ftable(round(a,3))

cat('b in consumption=a*w^b\n')
b<-tapply(dd$b,list(dd$Species,dd$Quarter),sum)
ftable(round(b,3))

a[is.na(a)]<-0
b[is.na(b)]<-0

outfile<-file.path(data.path,'consum_ab.in')
cat(file=outfile,"# paramter a and b for consumption=a*weight^b\n")

for (s in (1:npr)) {
  cat(file=outfile,"# ",sp.names[s],"\n",append=T)
  for (q in (1:4)) {
    cat(file=outfile,a[s,q],b[s,q],"\t# Quarter",q,"\n",append=T)    
  }
}



