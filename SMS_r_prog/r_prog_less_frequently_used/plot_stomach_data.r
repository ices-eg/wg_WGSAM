## prog ini 1
# read data from ASCII file and do some graphical initialisations
# you have to do this after every new SMS run
#################################################################
rm(stom)
stom<-Read.stomach.data(read.init.function=T)

#stom<-Read.stomach.data()

a<-aggregate(list(Pred.avail=stom$Prey.avail.part),
              list(Area=stom$SMS.area,Year=stom$Year,Quarter=stom$Quarter,Predator=stom$Predator,Predator.length.class=stom$Predator.length.class),sum)
stom<-merge(stom,a)
stom$stomcon.hat.part<-stom$Prey.avail.part/stom$Pred.avail 
stom$Residual.part<-stom$stom.input-stom$stomcon.hat.part
stom<-transform(stom,year.range=ifelse(Year<=1981,'1977-81','1982-93'),year=paste("Y",Year,sep=''))

stomAll<-stom  # all stomach input irrespectiv of actually use 
stom<-subset(stom,stom.used.all==1)  # used stomachs specified for input


write.csv(stom, file = file.path(data.path,'ASCII_stom_data.csv'),row.names = FALSE)



tr<-trellis.par.get( "background")
# old value  tr$col="#909090"
tr$col="white"
trellis.par.set("background", tr)

dev<-"print"
dev<-"screen"
nox<-4
noy<-3
#################################################################


names(stom)
 

################################################################

stom$ratio<-stom$Predator.size/stom$Prey.size
a<-subset(stom,Prey.no!=0)
a<-a[order(a$ratio),]
head(a)
###

a<-subset(stom,Stom.var>0,select=c(Predator,Quarter,Year,Predator.length.mean,Prey,Prey.length.mean,N.samples,stomcon,stan.residual))
a<-a[order(abs(a$stan.residual),decreasing =T),]
head(a,20)
#####

if (F) { # test of output stom proportion=1 (or a serious error some where)
  a<-aggregate(list(expec=stom$stomcon.hat),list(Year=stom$Year,Quarter=stom$Quarter,Predator.no=stom$Predator.no,Predator=stom$Predator,Predator.size=stom$Predator.length),sum)
  a$expec<-round(a$expec,2)
  a<-a[order(paste(a$Year,a$Quarter,a$Predator.no,a$Predator,a$Predator.size)),]
  a
}
###############################################################
#Number of stomachs;
ns<-unique(subset(stom,select=c(Predator,Predator.no,Predator.length,Predator.length.class,Year,Quarter, N.samples)))

#ns<-droplevels(subset(ns,Predator %in% c("W. mackerel","N. mackerel")))

a<-ftable(tapply(ns$N.sample,list(ns$Predator,ns$Year,ns$Quarter,ns$Predator.length),sum,na.rm=T))
tapply(ns$N.sample,list(ns$Predator,ns$Predator.length),sum,na.rm=T)
tapply(ns$N.sample,list(ns$Predator),sum,na.rm=T)

##############################################################
## prog rel 1
# Observed relativ stomach contents by predator, year and quarter
 unique(stom$Predator)
 
cleanup()

dev<-"print"
#dev<-"screen"
dev<-"png"
nox<-2
noy<-3

i<-0
b<- tapply(stom$stomcon,list(stom$Prey.no),sum)
all.prey.col<-sort(as.numeric(dimnames(b)[[1]]),decreasing = TRUE)
all.names<-rep('aaa',length(all.prey.col))
for (s in (1:length(all.prey.col))) all.names[s]<-sp.other.names[all.prey.col[s]+1]
#stom<-droplevels(subset(stom,Predator=='H. porpoise'))
#stom<-droplevels(subset(stom,Predator %in% c('G. gurnards','R. radiata','Hake','W.horse mac','N.horse mac')))
#stom<-droplevels(subset(stom,Predator %in% c('W.horse mac','N.horse mac')))

#stom<-droplevels(subset(stom,Predator %in% c("W. mackerel","N. mackerel")))

stom<-subset(stom,stom.used.like==1)  # just used stomachs
 
a<-by(stom,list(stom$Quarter,stom$Year,stom$Predator.no),function(x) {
   if (dim(x)[[1]]>0) {
    b<- tapply(x$stomcon,list(x$Prey.no,x$Predator.length),sum) 
    b[is.na(b)]<-0
    prey.names<-as.numeric(dimnames(b)[[1]])
    length.names<-dimnames(b)[[2]]
    #if (x[1,]$Quarter=="Q1") {
    if ((i %% (nox*noy-1))==0) {
      newplot(dev,nox,noy,Portrait=F,filename=paste(x[1,]$Year,x[1,]$Quarter,x[1,]$Predator,sep='-'));
      par(mar=c(3,5,3,2)) 
      plot.new(); legend(x=0,y=1,all.names,fill=all.prey.col,cex=1.0)
    }    
    i<<-i+1
    barplot(b,names=length.names,col=prey.names)
    title(main=paste(x[1,]$Year,x[1,]$Quarter," Pred:",x[1,]$Predator))
   }
})
if (dev=='png') cleanup()




by(stom,list(stom$Predator),function(x) {
  ftable(round(tapply(x$stomcon,list(x$Year,x$Quarter,x$Predator.length,x$Prey),sum)*1000,0))
})


by(stom,list(stom$Predator,stom$Quarter),function(x) {
  ftable(round(tapply(x$stomcon,list(x$Year,x$Predator.length,x$Prey),sum)*1000,0))
})


##############################################################
## prog rel 3
# Observed length distibution of preys by predator, size, quarter and year

dev<-"print"
dev<-"screen"
nox<-2
noy<-3

# select subset
stom2<-subset(stom,Year==1978 & Quarter=="Q1" & Prey.no!=0)

cleanup()
i<-0

by(stom2,list(stom2$Year,stom2$Quarter,stom2$Predator.no),function(x) {

     b<- tapply(x$stomcon,list(x$Predator.length,x$Prey.length.mean,x$Prey),sum)

     b[is.na(b)]<-0
     for (l in (1:dim(b)[1])) {
       if ((i %% (nox*noy))==0) {
         newplot(dev,nox,noy,Portrait=TRUE);
         par(mar=c(3,5,3,2))
       }
       i<<-i+1
       bb<-b[l,,]
       print(b)
       barplot(bb,beside = TRUE,col=2)
       title(main=paste(x[1,]$Year,x[1,]$Quarter,x[1,]$Predator,dimnames(b)[[1]][l]))
     }
})




dev<-"print"
dev<-"screen"
nox<-2
noy<-3


tapply(stom$Prey.length.mean,list(stom$Predator,stom$Prey),min)


# select subset, ONE prey only
stom2<-subset(stom,Quarter=="Q3" & Prey=='Sandeel'    & Predator=='Whiting')
stom2<-subset(stom,Quarter=="Q3" & Prey=='Herring')


    b<- tapply(stom2$Prey.length.mean,list(stom2$Predator,stom2$Prey),sum)
cleanup()
i<-0

by(stom2,list(stom2$Year,stom2$Quarter,stom2$Predator.no),function(x) {

    # b<- tapply(x$stomcon/x$Prey.weight,list(x$Predator.length,x$Prey.length.mean),sum)
     b<- tapply(x$stomcon,list(x$Predator.length,x$Prey.length.mean),sum)

     b[is.na(b)]<-0
     for (l in (1:dim(b)[1])) {
       if ((i %% (nox*noy))==0) {
         newplot(dev,nox,noy,Portrait=TRUE);
         par(mar=c(3,5,3,2))
       }
       i<<-i+1
       bb<-b[l,]
       print(b)
       barplot(bb,beside = TRUE,col=2)
       title(main=paste(x[1,]$Year,x[1,]$Quarter,x[1,]$Predator,dimnames(b)[[1]][l]))
     }
})

##################################################################################
## prog rel 4
# Obsereved and predicted relativ stomach content by predator quarter and year

cleanup()

dev<-"print"
dev<-"screen"
#dev<-"wmf"
nox<-3
noy<-3

i<-0
b<- tapply(stom$stomcon,list(stom$Prey.no),sum)
all.prey.col<-sort(as.numeric(dimnames(b)[[1]]),decreasing = TRUE)
all.names<-rep('aaa',length(all.prey.col))
for (s in (1:length(all.prey.col))) all.names[s]<-sp.other.names[all.prey.col[s]+1]

by(stom,list(stom$Quarter,stom$Year,stom$Predator.no),function(x) {
    b<- tapply(x$stomcon,list(x$Prey.no,x$Predator.length),sum)
    b[is.na(b)]<-0

    c<- tapply(x$stomcon.hat,list(x$Prey.no,x$Predator.length),sum)
    c[is.na(c)]<-0

    b<-rbind(b,c)
    prey.no<-as.numeric(dimnames(b)[[1]])
    prey.names<-rep('aaa',length(prey.no))
    for (s in (1:length(prey.names))) prey.names[s]<-sp.other.names[prey.no[s]+1]
    length.names<-dimnames(b)[[2]]
    #if (x[1,]$Quarter=="Q1") {
    if ((i %% (nox*noy-1))==0) {
      newplot(dev,nox,noy,Portrait=F,filename=paste('stom',i));
       par(mar=c(3,5,3,2))
      if (dev=="wmf" ) par(mar=c(2,4,2,2))
      
     # text(x=0.0,y=0.07,"lower: observed",pos=4)
      plot.new();
      title(main="upper: expected\nlower: observed")
      legend("center",all.names,fill=all.prey.col,cex=1.2)

    }
    i<<-i+1
    barplot(b,names=length.names,col=prey.no)
    title(main=paste(x[1,]$Year,x[1,]$Quarter," Pred:",x[1,]$Predator))
    #title(main=paste(x[1,]$Year,x[1,]$Quarter))

    abline(h=1,lwd=2)
})

##############################################################

# Histogram of observations with and without prey size
stom2<-subset(stom, Prey!='Other' & Prey.length.mean >0 & stomcon>1E-5)
cleanup()
nox<-3; noy<-2;
i<-0
minStom<-0.2

by(stom2,list(stom2$Predator.no),function(x) {
       xx<-x
       x$stomcon[x$stomcon>minStom]<-minStom
       xx<-with(xx, aggregate(stomcon,list(Predator=Predator,Prey=Prey,Quarter=Quarter,Year=Year,Length=Predator.length),sum))
       xx$x[xx$x>minStom]<-minStom
      print(xx) 
      if ((i %% (nox*noy))==0) {
         newplot(dev,nox,noy,Portrait=TRUE);
         par(mar=c(3,5,3,2))
       }
       i<<-i+1
       hist(x$stomcon,freq=F,breaks=50,main=paste(x[1,]$Predator,dim(x)[1],round(sum(x$stomcon))))       
       hist(xx$x,freq=F,breaks=50,main=paste(xx[1,]$Predator,dim(xx)[1],'cond',round(sum(xx$x))))
})



##############################################################
## prog size 1
#size at length by species

cleanup()
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

by(stom,list(stom$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$Predator.length.mean,x$Predator.size,xlab='length',ylab='size')
   title(main=paste('pred:',x[1,]$Predator) )
   i<<-i+1
})

by(stom,list(stom$Prey),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   x<-subset(x,Prey.size>0)
   if (dim(x)[1]>0) {
     plot(x$Prey.length.mean,x$Prey.size,xlab='length',ylab='size')
     title(main=paste('prey:',x[1,]$Prey) )
     i<<-i+1
   }
})



##############################################################
#length distribution in the sea
## prog length 1

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0 

a<-subset(stom,Prey.no !=0 & L.N.bar>0,select=c("Year","Quarter","Quarter.no","Prey.no","Prey","Prey.length.class","Prey.length.mean","L.N.bar"))
a<-unique(a)
a<-subset(a,Prey=='Cod')
by(a,list(a$Quarter,a$Year,a$Prey.no),function(x) {
    if (x[1,]$Quarter=="Q1") {
      newplot(dev,nox,noy);
    }    
    plot(x$Prey.length.mean,x$L.N.bar,col=x$Quarter.no)
    title(main=paste(x[1,]$Year,x[1,]$Quarter," Prey:",x[1,]$Prey))
})



##############################################################
#length distribution in the sea
## prog length 2

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

a<-subset(stom,Prey.no !=0,select=c("Year","Quarter","Quarter.no","Prey.no","Prey","Prey.length.class","Prey.length.mean","L.N.bar"))
a<-unique(a)
by(a,list(a$Year,a$Prey.no),function(x) {
    if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
    xmin<-min(x$Prey.length.mean)
    xmax<-max(x$Prey.length.mean)
    ymin<-min(x$L.N.bar)
    ymax<-max(x$L.N.bar)
    s<-order(x$Quarter.no,x$Prey.length.class)
    x<-x[s,]
    plot(x$Prey.length.mean[x$Quarter.no==1],x$L.N.bar[x$Quarter.no==1],col=x$Quarter.no,type='b',
          xlim=c(xmin,xmax),ylim=c(ymin,ymax),xlab='length',ylab='Nbar')
    title(main=paste(x[1,]$Year," Prey:",x[1,]$Prey))
    for (q in seq(2,4)) {
      lines(x$Prey.length.mean[x$Quarter.no==q],x$L.N.bar[x$Quarter.no==q],col=q)
    }
    i<<-i+1
})

 
##############################################################
#length distribution in the sea
## prog length 3

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

a<-subset(stom,Prey.no !=0 & Prey.length.mean>0, select=c("Year","Quarter","Quarter.no","Prey.no","Prey","Prey.length.class","Prey.length.mean","Prey.weight","L.N.bar"))
a<-unique(a)
by(a,list(a$Prey.no),function(x) {
    newplot(dev,nox,noy)
    xmin<-min(x$Prey.length.mean)
    xmax<-max(x$Prey.length.mean)
    ymin<-min(x$L.N.bar*x$Prey.weight)
    ymax<-max(x$L.N.bar*x$Prey.weight)
    s<-order(x$Quarter.no,x$Prey.length.class,x$Prey.length.mean)
    x<-x[s,]
    d<-as.data.frame(x)
    print(xyplot(L.N.bar~Prey.length.mean|Prey+as.factor(Year),groups=Quarter,type='b',data=d,ylab='Nbar'),
    layout = c(2, 2))
})
 
 ##############################################################
#length distribution in the sea
## prog length 3

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

a<-subset(stom,Prey.no !=0 & Prey.length.mean>0, select=c("Year","Quarter","Quarter.no","Prey.no","Prey","Prey.length.class","Prey.length.mean","Prey.weight","L.N.bar"))
a<-unique(a)
by(a,list(a$Prey.no),function(x) {
    newplot(dev,nox,noy)
    xmin<-min(x$Prey.length.mean)
    xmax<-max(x$Prey.length.mean)
    ymin<-min(x$L.N.bar*x$Prey.weight)
    ymax<-max(x$L.N.bar*x$Prey.weight)
    s<-order(x$Quarter.no,x$Prey.length.class,x$Prey.length.mean)
    x<-x[s,]
    d<-as.data.frame(x)
    print(xyplot(L.N.bar*Prey.weight~Prey.length.mean|Prey+as.factor(Year),groups=Quarter,type='b',data=d,ylab='Nbar*mean weight'),
    layout = c(2, 2))
})


##############################################################
#length distribution in the sea
## prog length 4

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

a<-subset(stom,Prey.no !=0 & Prey.length.mean>0, select=c("Year","Quarter","Quarter.no","Prey.no","Prey","Prey.length.class","Prey.length.mean","Prey.weight","L.N.bar"))
a<-unique(a)

 barchart(stom~as.factor(trunc(PredPrey*2)/2)|Predator, data=a, 
    xlab='log(predator weight/prey weight)',ylab='proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.8, lines=2),
    layout = c(1, 3) ,col='grey')

 



 ##############################################################
#Dirichlet 
a<-subset(stom,stom.used.like==1,select=c(Predator,	Quarter,	Year,	Predator.length.class,	Prey,	Quarter.no,	SMS.area,	Predator.no,	Diri.p,	Diri.sum.p,	Diri.like	,Prey.no,	Prey.length.class, N.haul))
a<-unique(a)

a<-a[order(a$Diri.like),]
head(a,20)
tail(a,20)
## 

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

a<-subset(stom,Quarter.no>0)
a<-unique(a)
xyplot(Diri.sum.p~N.samples,group=Predator,
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)

xyplot(Diri.like~N.samples,group=Predator,
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)

 
xyplot(Diri.like~N.samples|Predator,group=Predator.length.class,
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)


a<-subset(stom,Quarter.no>0 )

xyplot(stomcon.hat*(1-stomcon.hat)/(Diri.sum.p+1)*N.haul ~stomcon.hat,group=Predator, ylab="VAR(STOM)",
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)

xyplot(stomcon.hat*(1-stomcon.hat)/(Diri.sum.p+1)*N.haul ~stomcon.hat|Predator, ylab="VAR(STOM)",
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)


###
library(MCMCpack)
rdirichlet<-function(n, alpha)  # stolen from library MCMC-pack
{
    l <- length(alpha)
    x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
    sm <- x %*% rep(1, l)
    return(x/as.vector(sm))
}
apply(rdirichlet(20, c(1,1,1) ),2,mean)
apply(rdirichlet(20, c(10,10,10) ),2,mean)
apply(rdirichlet(20, c(1,1,1) ),2,sd)
apply(rdirichlet(20, c(10,10,10) ),2,sd)

s<-subset(stom,Quarter.no>0 & Diri.sum.p>0 & Predator=='Cod' & Year==1981)
a<-tapply(s$stomcon.hat,list(s$Year,s$Quarter.no,s$Predator.no,s$Predator.length.class,s$Prey.no,s$Prey.length.class),sum,na.rm=T)
a.in<-tapply(s$stom.input,list(s$Year,s$Quarter.no,s$Predator.no,s$Predator.length.class,s$Prey.no,s$Prey.length.class),sum,na.rm=T)
avail<-tapply(s$stom.used.avail,list(s$Year,s$Quarter.no,s$Predator.no,s$Predator.length.class,s$Prey.no,s$Prey.length.class),sum,na.rm=T)

ss<-unique(droplevels(subset(s,select=c(Diri.sum.p,Year,Quarter.no,Predator.no,Predator,Predator.length.class))))
xyplot(Diri.sum.p~Predator.length.class|Predator, ylab="p sum",
      auto.key = list(space = "right", points = T, lines = TRUE),data=ss)


p_sum<-tapply(ss$Diri.sum.p,list(ss$Year,ss$Quarter.no,ss$Predator.no,ss$Predator.length.class),mean,na.rm=T)
#ftable(round(p_sum,1))

nrep<-200
dd<-dimnames(a)
sumStom<-SMS.dat@sum.stom.like
min.stom.cont<-SMS.dat@min.stom.cont   
ddd<-NULL
for (y in dd[[1]]) {for (q in dd[[2]]) {
    cat('y:',y,' q:',q,'\n')
    for (pred in dd[[3]]) {
      for (predl in dd[[4]]) {
      obs<-t(a[y,q,pred,predl,,])
      obs<-obs[!is.na(obs)]
      obs<-obs[obs>0]
      if (length(obs)>1) 
         bb<-obs*p_sum[y,q,pred,predl]
         dirbb<-rdirichlet(nrep,bb)
         for (i in (1:dim(dirbb)[2])) {
            d<-density(dirbb[,i])
            ddd<-rbind(ddd,data.frame(no=i,year=y,q=q,pred=pred,predl=predl,obs=obs[i],x=d$x,y=d$y))
          }
      }
}}}


ddd$labs<-paste(ddd$no," mean=",round(ddd$obs,4),sep='')
ddd$labs2<-paste(ddd$year,ddd$q,ddd$pred,ddd$predl,sep=',')

b<-droplevels(subset(ddd, pred=='17' &predl=='17' & year=='1981' & q=='1'))

xyplot(y~x| labs, ylab="density",data=b,type='l',scales=list(y=list(relation="free")))


b<-droplevels(subset(ddd, pred=='17' & year=='1981' & q=='1'))
xyplot(y~x|labs2, groups=no,ylab="density",data=b,type='l',scales=list(y=list(relation="free")))



##############################################################


# QQplot of Residuals by Predator and prey
## prog qq 1


cleanup() 
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

by(stom,list(stom$Prey,stom$Predator),function(x) {
if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
  qqnorm(x$Residual,main=' ')
  qqline(x$Residual)
  title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
  i<<-i+1
})


##############################################################
#SPECIAL CASE COD  observed and predicted stomach contents by Predator and prey
# prog stom 1 cod

cleanup() 
nox<-1; noy<-2;
newplot(dev,nox,noy);
i<-0
stom2<-subset(stom,Prey=='Cod')
 head(stom2)
by(stom2,list(stom2$Prey,stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-log(x$stomcon)
   bb<-log(x$stomcon.hat)
   max.val<-max(aa,bb)
   min.val<-min(aa,bb)
   plot(bb,aa,xlab='Expected stomach content',ylab='observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val),pch=x$Predator.length.class,
   col=x$Predator.no)
   if (var(bb)>0) abline(lm(aa~bb), lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})


##############################################################
#observed and predicted stomach contents by Predator and prey
# prog stom 1

cleanup() 
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

by(stom,list(stom$Prey,stom$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-(x$stomcon)
   bb<-(x$stomcon.hat)
   max.val<-max(aa,bb)
   min.val<-min(aa,bb)
   plot(bb,aa,xlab='Expected stomach content',ylab='observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val))
   if (var(bb)>0) abline(lm(aa~bb), lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})

##############################################################
#log observed and predicted stomach contents by Predator and prey
## prog stom 2

cleanup() 
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

by(stom,list(stom$Prey,stom$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-log(x$stomcon)
   bb<-log(x$stomcon.hat)
   max.val<-max(aa,bb)
   min.val<-min(aa,bb)
   plot(bb,aa,xlab='log Expected stomach content',ylab='log observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val))
   if (var(bb)>0) abline(lm(aa~bb), lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})

##############################################################
#observed and predicted stomach contents by  prey
## prog stom 3

cleanup() 
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

by(stom,list(stom$Prey),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-(x$stomcon)
   bb<-(x$stomcon.hat)
   max.val<-max(aa,bb)
   min.val<-min(aa,bb)
   plot(bb,aa,xlab='Expected stomach content',ylab='observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val))
   r2<-''
   if (var(bb)>0) {
       cc<-lm(aa~bb)
       abline(cc, lty=3) 
       dd<-summary(cc)
       r2<-paste(" r^2=",round(dd[["r.squared"]],2),sep='')
   }
   title(main=paste("Prey:",x[1,]$Prey,r2,sep=""))
   i<<-i+1
})

 
##############################################################
#PAPER version. observed and predicted stomach contents by  prey
## prog stom 3

cleanup() 
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0
  
by(stom,list(stom$Prey),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
  # x<-subset(x,stomcon<0.2 &stomcon.hat<0.2)
  x<-subset(x,stomcon>0.0 )
   aa<-(x$stomcon)
   bb<-(x$stomcon.hat)
   max.val<-max(aa,bb)
   min.val<-min(aa,bb)
   plot(bb,aa,xlab='Expected stomach content',ylab='Observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val),cex=.8)
   r2<-''
   if (var(bb)>0) {
       #cc<-lm(aa~bb)
       cc<-lm(aa~bb,weights=(sqrt(x$N.samples)))

       abline(cc, lty=3) 
       dd<-summary(cc)
      # r2<-round(dd[["r.squared"]],2)
        r2<-formatC(dd[["r.squared"]],digits = 2, format = "f")
       sp<- x[1,]$Prey
       #r2<-paste(" r^2=",r2,sep='')
          }
 
 title(main=paste("        ",sp),adj=0)    
 title(bquote(r^2 == .(r2)),adj=1)
 
  #text(x=0,y=max.val*0.9,labels=bquote(r^2 == .(r2)),pos=4,adj=1,cex=1.5)
   i<<-i+1
})

 #theta <- 1.23 ; title(bquote(hat(theta) == .(theta)))
 # substitute(rho == . , list( . = r2))


#############################################################
# log observed and predicted stomach contents by  prey
## prog stom 4

cleanup() 
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

by(stom,list(stom$Prey),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
     x<-subset(x,stomcon>0.0 )

   aa<-log(x$stomcon)
   bb<-log(x$stomcon.hat)
   max.val<-max(aa,bb)
   min.val<-min(aa,bb)
   plot(bb,aa,xlab='log Expected stomach content',ylab='log observed ',
       ylim=c(min.val,max.val),xlim=c(min.val,max.val),col=1)
   r2<-''
   if (var(bb)>0) {
       #cc<-lm(aa~bb,weights=1/x$Stom.var)
       cc<-lm(aa~bb)
       abline(cc, lty=3) 
       dd<-summary(cc)
       r2<-paste(" r^2=",formatC(dd[["r.squared"]],2,format='f'),sep='')
   }
   title(main=paste("Prey:",x[1,]$Prey,r2,sep=""))
   i<<-i+1
})



##############################################################
# PAPER Residuals by Predator, and expected stom content
## prog resid 1

cleanup() 
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Predator=='Cod' & Prey=="Sandeel")
#stom2<-subset(stom,Predator=='Cod' )

by(stom2,list(stom2$Prey),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$stomcon.hat,x$Residual,xlab='Expected relative stomach content',ylab='Residual')
   abline(h=0, lty=3) 
#   title(main=paste(" Prey:",x[1,]$Prey,sep=""))
#   title(main=paste("Prey:",x[1,]$Prey))

   #abline(lm(x$Residual~x$stomcon.hat),lty=2)
   #if (length(x$Prey)>5 )  lines(smooth.spline(x$stomcon.hat,x$Residual),lty=1)
   i<<-i+1
})


##############################################################
#Residuals by Predator, and expected stom content
## prog resid 1

cleanup() 
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

by(stom,list(stom$Prey,stom$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$stomcon.hat,x$Residual,xlab='Expected relative stomach content',ylab='residuals ')
   abline(h=0, lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
#   title(main=paste("Prey:",x[1,]$Prey))

   #abline(lm(x$Residual~x$stomcon.hat),lty=2)
   #if (length(x$Prey)>5 )  lines(smooth.spline(x$stomcon.hat,x$Residual),lty=1)
   i<<-i+1
})


##############################################################
#Residuals by Predator and prey, and prey/pred size
## prog resid 2

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Prey.no,stom2$Predator),function(x) {
if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Predator.size/x$Prey.size),x$Residual,xlab='log(Pred size/Prey size)',ylab='residuals ')
   abline(h=0, lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   if (x$Prey.no !=0 & length(x$Prey)>5 )  lines(smooth.spline(log(x$Predator.size/x$Prey.size),x$Residual),lty=1)
   i<<-i+1
}}
)}

##############################################################
#Residuals and prey, and prey size. Label=quarter
## prog resid 3

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {

by(stom2,list(stom2$Prey.no),function(x) {
if (x$Prey.no[1] != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Prey.size),x$Residual,xlab='log(Prey size)',ylab='residuals', pch=x$Quarter.no,col=x$Quarter.no)
   abline(h=0, lty=3) 
   title(main=paste("Prey:",x[1,]$Prey,sep=""))
   #if (x$Prey.no[1] !=0 & length(x$Prey)>5 )  lines(smooth.spline(log(x$Prey.size),x$Residual),lty=1)
   i<<-i+1
}}
)}


##############################################################
#Residuals and prey, and prey size. Label=year
## prog resid 4

cleanup()
nox<-3; noy<-1;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {

by(stom2,list(stom2$Prey.no),function(x) {
if (x$Prey.no[1] != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Prey.size),x$Residual,xlab='log(Prey size)',ylab='residuals', pch=x$Year-1977,col=x$Year-1977)
   abline(h=0, lty=3) 
   title(main=paste("Prey:",x[1,]$Prey,sep=""))
   if (x$Prey.no[1] !=0 & length(x$Prey)>5 )  lines(smooth.spline(log(x$Prey.size),x$Residual),lty=1)
   i<<-i+1
}}
)}

##############################################################
#Residuals and prey, and prey size. Label=predator
## prog resid 5

cleanup()
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {

by(stom2,list(stom2$Prey.no),function(x) {
if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Prey.size),x$Residual,xlab='log(Prey size)',ylab='residuals', pch=x$Predator.no,col=x$Predator.no)
   abline(h=0, lty=3) 
   title(main=paste("Prey:",x[1,]$Prey,sep=""))
   if (x$Prey.no !=0 & length(x$Prey)>5 )  lines(smooth.spline(log(x$Prey.size),x$Residual),lty=1)
   i<<-i+1
}})}


##############################################################
#Residuals and prey, and prey size and quarter
## prog resid 6

cleanup()
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Quarter,stom2$Prey.no),function(x) {
if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Prey.size),x$Residual,xlab='log(Prey size)',ylab='residuals ', pch=x$Predator.no,col=x$Predator.no)
   abline(h=0, lty=3) 
   title(main=paste(x[1,]$Quarter," Prey:",x[1,]$Prey,sep=""))
   if (x$Prey.no !=0 & length(x$Prey)>5 )  lines(smooth.spline(log(x$Prey.size),x$Residual),lty=1)
   i<<-i+1
}})}





##############################################################
#Residuals and prey, and prey size and quarter
## prog resid 6b

cleanup()
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Quarter,stom2$Prey.no),function(x) {
if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$Prey.length.class,x$Residual,xlab='prey length class',ylab='residuals ', pch=x$Predator.no,col=x$Predator.no)
   abline(h=0, lty=3)
   title(main=paste(x[1,]$Quarter," Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
}})}

##############################################################
#Residuals by Predator and prey, and prey size
## prog resid 7

nox<-3; noy<-3;
cleanup() 
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Predator,stom2$Prey.no),function(x) {
if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Prey.size),x$Residual,xlab='log(Prey size)',ylab='residuals ')
   abline(h=0, lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   if (x$Prey.no !=0 & length(x$Prey)>5 )  lines(smooth.spline(log(x$Prey.size),x$Residual),lty=1)
   i<<-i+1
}})}

##############################################################
#Residuals by Predator and prey, and prey size and quarter
## prog resid 8


nox<-2; noy<-1;
cleanup() 
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Quarter,stom2$Prey.no,stom2$Predator),function(x) {
if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Prey.size),x$Residual,xlab='log(Prey size)',ylab='residuals ')
   abline(h=0, lty=3) 
   title(main=paste(x[1,]$Quarter," Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
}})}


##############################################################
#Residuals by Prey and predator, and prey size and quarter
## prog resid 9


nox<-2; noy<-3;
cleanup() 
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Quarter,stom2$Predator,stom2$Prey.no),function(x) {
if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Prey.size),x$Residual,xlab='log(Prey size)',ylab='residuals ')
   abline(h=0, lty=3) 
   title(main=paste(x[1,]$Quarter,", Prey:",x[1,]$Prey," Pred:",x[1,]$Predator,sep=""))
   i<<-i+1
}})}



##############################################################
# Box-plot of residuals by predator, prey and quarter
# prog box 1

nox<-2; noy<-2;
cleanup() 
newplot(dev,nox,noy);
i<-0
res.ylim<-c(-0.1,0.1)     # Dirichlet
#res.ylim<-c(-5,5)  # log normal

my.pred<-c("Cod","Whiting","Saithe","Haddock","W Mackerel","NS Mackerel")
my.pred<-c("Cod")

#my.pred<-c("Cod")
stom3<-subset(stom,Predator %in% my.pred,drop=TRUE)
#if (length(my.pred)==1) a<-glm(Residual~-1+Quarter:Prey,data=stom3,weight=stomcon.hat)
if (length(my.pred)==1) a<-glm(Residual~-1+Quarter:Prey,data=stom3,weight=N.haul*stomcon.hat)
if (length(my.pred)>1) a<-glm(Residual~-1+Quarter:Predator:Prey,data=stom3,weight=stomcon.hat)
summary(a)

by(stom3,list(stom3$Predator,stom3$Prey),function(x) {
#if (x$Prey.no != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
    boxplot(Residual~Quarter,data=x,ylim=res.ylim)

   abline(h=0,col=3)
   title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
   i<<-i+1
#}
})


##############################################################
# Box-plot of residuals by predator and quarter
# prog box 2

nox<-3; noy<-2;
cleanup() 
newplot(dev,nox,noy);
i<-0

# note only other food selected
stom3<-subset(stom,Prey=="Other" & stom$Predator %in% my.pred,drop=TRUE)

by(stom3,list(stom3$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   boxplot(Residual~Quarter,data=x,ylim=c(-1,1))
   abline(h=0,col=3)

   title(main=paste(x[1,]$Predator,sep=""))
   i<<-i+1
})


##############################################################
# Box-plot of residuals by prey and quarter
# prog box 2

nox<-3; noy<-2;
cleanup() 
newplot(dev,nox,noy);
i<-0

stom3<-subset(stom, stom$Predator %in% my.pred,drop=TRUE)

by(stom3,list(stom3$Prey),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   boxplot(Residual~Quarter,data=x,ylim=c(-0.1,0.1))
   abline(h=0,col=3)

   title(main=paste(x[1,]$Prey,sep=""))
   i<<-i+1
})

 
##############################################################
# Box-plot of residuals by prey and year and quarter
# prog box 3

nox<-4; noy<-4;
cleanup() 
newplot(dev,nox,noy);
i<-0

stom3<-subset(stom, stom$Predator %in% my.pred,drop=TRUE)

by(stom3,list(stom3$year,stom3$Prey),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   boxplot(Residual~Quarter,data=x,ylim=c(-1,1))
   abline(h=0,col=3)

   title(main=paste(x[1,]$year,x[1,]$Prey,sep=" "))
   i<<-i+1
})

##############################################################
#Frequency of observation by predator-prey size ratio
## prog size 1

cleanup()
nox<-4; noy<-4;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model==1)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Prey.no,stom2$Predator),function(x) {
if (x$Prey.no[1] != 0) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }  
   hist(log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(pred-size / prey-size) ')
   abline(h=0, lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
}}
)}

 
cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & stom$Prey.no != 0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
by(stom2,list(stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }  
   hist(log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(pred-size / prey-size) ')
   abline(h=0, lty=3) 
   title(main=paste("Pred:",x[1,]$Predator,sep=""))
   i<<-i+1
}
)}

 ##################################
 
 
stom3<-subset(stom,Size.model>0 & Prey.no>0 & N.haul>=10 & Predator !="NS Mackerel" & Predator !="W Mackerel" )
#stom3<-subset(stom,Size.model>0 & Prey.no>0 & N.haul>10  )
#stom3<-subset(stom3, !((Predator=='Cod' & Predator.length.mean<400)| (Predator=='Whiting' & Predator.length.mean<200)  | (Predator=='Saithe' & Predator.length.mean<400)))

# start with a high fac for calc of mean and var
fac<-50  
a<-aggregate(list(stom=stom3$stomcon),list(Predator=stom3$Predator,PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*50)/50),sum)

by(a,list(a$Predator),function(x) weighted.mean(x$PredPrey, x$stom))


 barchart(stom~as.factor(trunc(PredPrey*2)/2)|Predator, data=a, 
    xlab='log(predator weight/prey weight)',ylab='proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.8, lines=2),
    layout = c(1, 3) ,col='grey')

 

####################################
# observation by predator-prey size ratio
## prog size 1b

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0




stom3<-subset(stom,  (Size.model==1 | Size.model==11) &  Prey.no>0 & !( 
                     (Predator=='Cod' & Predator.length.mean<400) | 
                     (Predator=='Whiting' & Predator.length.mean<200)  |
                     (Predator=='Saithe' & Predator.length.mean<400)))

stom3<-subset(stom,  (Size.model==1 | Size.model==11) &  Prey.no>0 & Predator %in% c('H. porpoise','Grey seal'))
                     
stom3<-subset(stom,  (Size.model==1 | Size.model==11) &  Prey.no>0 )


# just checking
a<-aggregate(list(stom=stom3$stomcon,stom.input=stom3$stom.input,stom.hat=stom3$stomcon.hat,stom.hat.part=stom3$stomcon.hat.part),
             list(Year=stom3$Year,Quarter=stom3$Quarter,Predator=stom3$Predator,Pred_l=stom3$Predator.length),sum)

# use the individual data points (not the liklihood data) when size mode is 11                       
stom3[stom3$Size.model==11,]$stomcon<-    stom3[stom3$Size.model==11,]$stom.input
stom3[stom3$Size.model==11,]$stomcon.hat<-stom3[stom3$Size.model==11,]$stomcon.hat.part

stom3<-droplevels(stom3)

# start with a high fac for calc of mean and var
fac<-50  
a<-aggregate(list(stom=stom3$stomcon*5000,stom.hat=stom3$stomcon.hat*5000)
             ,list(Predator=stom3$Predator,PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*fac)/fac),sum)

# by(a,list(a$Predator),function(x) weighted.mean(x$PredPrey, x$stom))

bb<-data.frame(size=rep(a$PredPrey,trunc(a$stom)),Predator=rep(a$Predator,trunc(a$stom)))
k1<-aggregate(list(mean=bb$size),list(Predator=bb$Predator),mean)
k2<-aggregate(list(var=bb$size),list(Predator=bb$Predator),var)
stat<-merge(k1,k2)

bb<-data.frame(size=rep(a$PredPrey,trunc(a$stom.hat)),Predator=rep(a$Predator,trunc(a$stom.hat)))
k1<-aggregate(list(mean.hat=bb$size),list(Predator=bb$Predator),mean)
k2<-aggregate(list(var.hat=bb$size),list(Predator=bb$Predator),var)
stat.hat<-merge(k1,k2)

stat<-merge(stat,stat.hat) 
stat

# decrease fac for a  nice plot
fac<-2
a<-aggregate(list(stom=stom3$stomcon, stomNo=stom3$stomcon/stom3$Prey.weight,stom.hat=stom3$stomcon.hat),list(Predator=stom3$Predator,
                                      PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*fac)/fac),sum)                                     
b<-aggregate(list(sumstom=a$stom, sumstomNo=a$stomNo, sumstom.hat=a$stom.hat),list(Predator=a$Predator),sum)
a<-merge(a,b)
a<-merge(a,stat)
a$stom<-a$stom/a$sumstom
a$stomNo<-a$stomNo/a$sumstomNo
a$stom.hat<-a$stom.hat/a$sumstom.hat

## observed
a$headt<-paste(a$Predator,", mean=",formatC(a$mean,digits=2,format='f')," variance=",formatC(a$var,digits=2,format='f'),sep='')

cleanup() 
trellis.device(device = "windows", 
               color = T, width=9, height=17,pointsize = 12,
               new = TRUE, retain = FALSE)
cleanup()
trellis.device(device = "windows",
               color = T, width=18, height=18,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|headt, data=a,
    groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.5, lines=2),
    scales = list(x = list(rot = 90, cex=0.5), y= list(alternating = 1,cex=0.5)),
    layout = c(3,4) ,col='grey')

if (F) {
 # for the paper
 barchart(stom~as.factor(PredPrey)|headt, data=a, 
    groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(1,1) ,col='grey')
 }

b1<-a
b1$type<-paste(a$headt,'\nObserved',sep='');

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')

trellis.device(device = "windows",
               color = T, width=18, height=22,pointsize = 12,
               new = TRUE, retain = FALSE)


  barchart(stom.hat~as.factor(PredPrey)|headt, data=a,
    groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.5, lines=2),
    scales = list(x = list(rot = 90, cex=0.5), y= list(alternating = 1,cex=0.5)),
    layout = c(3,4) ,col='grey')


if (F) {

trellis.device(device = "windows", 
               color = T, width=9, height=17,pointsize = 2,
               new = TRUE, retain = FALSE)

 # for the paper
 barchart(stom.hat~as.factor(PredPrey)|headt, data=a, 
    groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.8, lines=2),
    scales = list(x = list(rot = 45, cex=0.8), y= list(alternating = 1,cex=0.8)),
    layout = c(1, 3) ,col='grey')
}

b2<-a

b2$type<-paste(a$headt,'\nPredicted',sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)

trellis.device(device = "windows", 
               color = T, width=17, height=17,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|type, data=aa, 
     groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis

    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(2, 3) ,col='grey')

 
#####################
# same as above, but with contributions from prey species

fac<-2
a<-aggregate(list(stom=stom3$stomcon, stomNo=stom3$stomcon/stom3$Prey.weight,stom.hat=stom3$stomcon.hat),list(Predator=stom3$Predator,
                                      Prey=stom3$Prey,
                                      PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*fac)/fac),sum)
b<-aggregate(list(sumstom=a$stom, sumstomNo=a$stomNo, sumstom.hat=a$stom.hat),list(Predator=a$Predator),sum)
a<-merge(a,b)
a<-merge(a,stat)
a$stom<-a$stom/a$sumstom
a$stomNo<-a$stomNo/a$sumstomNo
a$stom.hat<-a$stom.hat/a$sumstom.hat

## observed
a$headt<-paste(a$Predator,", mean=",formatC(a$mean,digits=2,format='f')," variance=",formatC(a$var,digits=2,format='f'),sep='')

#cleanup()
trellis.device(device = "windows",
               color = T, width=17, height=17,pointsize = 12,
               new = TRUE, retain = FALSE )

b<- tapply(stom3$stomcon,list(stom3$Prey),sum)
col<-1:6
lab<-dimnames(b)[[1]]

#cleanup()
trellis.device(device = "windows",
               color = T, width=7, height=9,pointsize = 2,
               new = TRUE, retain = FALSE)


 barchart(stom~as.factor(PredPrey)|headt, data=a,
 groups = Prey, stack = TRUE,  col=1:6,
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:5),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(1, 1) )


b1<-a
b1$type<-paste(a$headt,'\nObserved',sep='');

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')
trellis.device(device = "windows",
               color = T, width=7, height=9,pointsize = 2,
               new = TRUE, retain = FALSE)
               
barchart(stom.hat~as.factor(PredPrey)|headt, data=a,
 groups = Prey, stack = TRUE,  col=1:6,
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:6),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(1, 3) )

 
b2<-a

b2$type<-paste(a$headt,'\nPredicted',sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)
aa<-droplevels(aa)

trellis.device(device = "windows", 
               color = T, width=17, height=17,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|type, data=aa, 
     groups = Prey, stack = TRUE,  col=1:6,    # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:6),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(2, 3) )

 

# BALTIC both on the same plot b
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)

trellis.device(device = "windows",
               color = T, width=8, height=4,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|type, data=aa,
     groups = Prey, stack = TRUE,  col=1:3,    # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:3),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(2, 1) )


###################

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0


by(a,list(a$Predator),function(x) {
    tit<-paste(x[1,]$Predator,' mean=',formatC(x[1,]$mean,digits=2,format='f'), ' var=',formatC(x[1,]$var,digits=2,format='f'),sep='') 
    b<- tapply(x$stom,x$PredPrey,sum)
    print(b)
    if (i==noxy) {newplot(dev,nox,noy); i<<-0 } 
    barplot(b) 
    title(main=tit)
})

 
 ##############################################################
#observation of prey size against predator size by predator
## prog size 2aa

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Prey.size))
max.ratio<-log(max(stom2$Prey.size))

by(stom2,list(stom2$Prey.no,stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Predator.size),log(x$Prey.size),main=NULL,xlab='log(predator size)',ylab="log(prey.size)",
             ylim=c(min.ratio,max.ratio),col=x$Quarter.no)
   abline(h=0, lty=3)
   aa<-lm(log(Prey.size)~log(Predator.size) ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3)
   title(main=paste(x[1,]$Predator," eating ",x[1,]$Prey,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   i<<-i+1
})}


##############################################################
#observation of predator-prey size ratio against predator size by predator
## prog size 2a

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Prey.no,stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$Predator.size,log(x$Predator.size/x$Prey.size),main=NULL,xlab='predator size',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio),col=x$Quarter.no)
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~Predator.size ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3)
   title(main=paste(x[1,]$Predator," eating ",x[1,]$Prey,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   i<<-i+1
})}


##############################################################
#observation of predator-prey size ratio against predator size by predator
## prog size 2b

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0 &stomcon>1E-4)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Prey.no,stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Predator.size),log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(predator size)',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio))
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~log(Predator.size) ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3,lwd=2)
   
   #quantile regression
   for (tau in c(0.025,0.975)) {
     ru<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=tau)
     abline(ru,col='blue',lty=2)
   }
   # weighted regression
   #aa<-lm(log(Predator.size/Prey.size)~log(Predator.size),weights=stomcon ,data=x)
   #ac<-aa[["coefficients"]]
   #abline(aa, lty=2,col=2)

   
   title(main=paste(x[1,]$Predator," eating ",x[1,]$Prey,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   i<<-i+1
})}


##############################################################
#observation of predator-prey size ratio against predator size by predator
## prog size 2b paper

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0 &stomcon>1E-4 & Predator=='Cod' & Prey=='Haddock')
#stom2<-subset(stom,Size.model>0 & Prey.no !=0 &stomcon>1E-4 & Predator=='Cod' & Prey=='Cod')
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Prey.no,stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Predator.size),log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(predator weight)',ylab="log(predator weight / prey weight)")
    
   #quantile regression
   for (tau in c(0.025,0.975)) {
     ru<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=tau)
     abline(ru,col=1,lty=2)
   }
   
   i<<-i+1
})}


##############################################################
#observation of predator-prey size ratio against predator size by prey
## prog size 3

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Predator,stom2$Prey.no),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$Predator.size,log(x$Predator.size/x$Prey.size),main=NULL,xlab='predator size',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio),col=x$Quarter.no)
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~Predator.size ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3)
   title(main=paste(x[1,]$Predator," eating ",x[1,]$Prey,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   i<<-i+1
})}



##############################################################
#observation of predator-prey size ratio against predator
## prog size 4

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$Predator.size,log(x$Predator.size/x$Prey.size),main=NULL,xlab='predator size',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio),col=x$Prey.no)
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~Predator.size ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3)
   title(main=paste(x[1,]$Predator,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   i<<-i+1
})}


##############################################################
#observation of predator-prey size ratio against predator
## prog size 5

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Predator.size),log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(predator size)',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio))
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~log(Predator.size) ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3)
      #quantile regression
   rl<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=0.025)
   ru<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=0.975)
   abline(rl,col='blue',lty=2)
   abline(ru,col='blue',lty=2)

   title(main=paste(x[1,]$Predator,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   i<<-i+1
})}


######################################################################
#observation of predator-prey size ratio against predator
## prog size 6

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(log(x$Predator.size),log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(predator size)',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio),col=x$Prey.no)
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~log(Predator.size) ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3,lwd=3)
   title(main=paste(x[1,]$Predator,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   xx<-x
   by(xx,list(xx$Prey.no),function(x) {
        abline(lm(log(Predator.size/Prey.size)~log(Predator.size),data=x))
   })
   i<<-i+1
})}



##############################################################
# Other food observed and estimated by year
## prog other 1

nox<-4; noy<-4;
cleanup() 
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Prey.no==0)

by(stom2,list(stom2$Year,stom2$Quarter,stom2$Predator.no),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   max.y<-max(x$stomcon.hat,x$stomcon)
   plot(x$Predator.length.class,x$stomcon,xlab='pred. length class',ylab='', pch=1,col=1,type='b',ylim=c(0,max.y))
   lines(x$Predator.length.class,x$stomcon.hat,pch=2,col=2,type='p')
   title(main=paste(x[1,]$Predator,x[1,]$Year,x[1,]$Quarter))
   i<<-i+1
})




##############################################################
# Other food observed and estimated by year
## prog other 1

nox<-2; noy<-2;
cleanup() 
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Prey=='Other')

xyplot(stomcon.hat~Predator.length.class|Quarter*Predator*year, data=stom2,
groups=stomcon,
panel=function(x,y,subscripts,groups) {
 panel.xyplot(x,y,col=1 ,pch=1)
 panel.xyplot(x,groups[subscripts],col=2 ,pch=4)
})

##############################################################
# Other food, suitatability
## prog other 2

xyplot(Suit~Predator.length.class|Quarter*Predator*year, data=stom,subset=(Prey=='Other'),col=2)


##############################################################
#Other food residuals
## prog other 3
xyplot(Residual~Predator.length.class|Quarter*Predator, data=stom,subset=(Prey=='Other'),
panel=function(x,y) {
 panel.xyplot(x,y,col=1 ,pch=1)
 panel.loess(x,y, span=1)
})
##############################################################


##############################################################
#Other food residuals
## prog other 4
xyplot(Residual~log(Predator.size)|Quarter*Predator, data=stom,subset=(Prey=='Other'),
panel=function(x,y) {
 panel.xyplot(x,y,col=1 ,pch=1)
 panel.loess(x,y, span=1)
})
##############################################################


##############################################################

#Total Available food
## prog avail 2
#
s<-tapply(stom$Prey.avail,list(stom$Year,stom$Quarter,stom$Predator,stom$Predator.length.class),sum)
s1<-arr2df(s)
names(s1)<-c('Year','Quarter','Predator','Predator.length','Prey.avail')
s1<-s1[!is.na(s1$Prey.avail),]

xyplot(Prey.avail~Predator.length|Quarter*Predator,col=2, data=s1)

##############################################################

