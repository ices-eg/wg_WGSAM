## prog ini 1
# read data from ASCII file and do some graphical initialisations
# you have to do this after every new SMS run
#################################################################

stom<-Read.stomach.data()
stom$Area<-stom$SMS.area
stom$SMS.area<-NULL
a<-aggregate(list(Pred.avail=stom$Prey.avail.part),
              list(Year=stom$Year,Quarter=stom$Quarter,Predator=stom$Predator,Predator.length.class=stom$Predator.length.class),sum)
stom<-merge(stom,a)
stom$stomcon.hat.part<-stom$Prey.avail.part/stom$Pred.avail
stom$Residual.part<-stom$stom.input-stom$stomcon.hat.part
stom<-transform(stom,year.range=ifelse(Year<=1981,'1977-81','1982-93'),year=paste("Y",Year,sep=''))

write.csv(stom, file = file.path(data.path,'ASCII_stom_data.csv'),row.names = FALSE)
stomAll<-stom  # all stomach input irrespectiv of actually use
stom<-subset(stom,stom.used.all==1)  # used stomachs specified for input


tr<-trellis.par.get( "background")
# old value  tr$col="#909090"
tr$col="white"
trellis.par.set("background", tr)

dev<-"print"
dev<-"screen"

nox<-4
noy<-3

no.areas<-SMS.control@no.areas
if (no.areas==4) no.areas<-3
area.names<-Read.area.names()
################################################################


stom<-subset(stom, Stom.var>0 & stom.used.like==1)

# Peter Lewy's Dirichlet residuals
stom$pl.theta<-stom$N.samples/stom$Diri.sum.p
stom$pl.E<-stom$N.samples*stom$Diri.p/stom$Diri.sum.p
stom$pl.Var<-stom$Diri.p*stom$pl.theta^2
stom$pl.sd<-sqrt(stom$pl.Var)
stom$pl.res<-((stom$N.samples*stom$stomcon)-stom$pl.E)/stom$pl.sd
stom$pl.y<-pgamma(stom$N.samples*stom$stomcon,shape=stom$Diri.p,scale=stom$pl.theta)

dim(stom)

stom<-subset(stom,stom$pl.y<1)
dim(stom)

cleanup()
newplot(dev="screen",nox=2,noy=1,w8=5,w11=8);      # Peter Lewy residual
hist(stom$pl.res,main='Stomach residuals',xlab="residuals")
qqnorm(stom$pl.res)
qqline(stom$pl.res)


cleanup()
newplot(dev="screen",nox=2,noy=2,w8=8,w11=8);
hist(stom$pl.res,main='Stomach residuals. Dirichlet trans',xlab="residuals")  # Peter Lewy residual
qqnorm(stom$pl.res)
qqline(stom$pl.res)

hist(stom$stan.res,main='Stomach residuals standardised',xlab="residuals")
qqnorm(stom$stan.res)
qqline(stom$stan.res)


a<-subset(stom,Stom.var>0 & stom.used.like==1 ,select=c(Area,Predator,Quarter,Year,Predator.length.class ,Predator.length.mean,Prey,Prey.length.mean,N.samples,stomcon,stomcon.hat,Residual,stan.residual))
a<-a[order(abs(a$stan.residual),decreasing =T),]
head(a,20)


a<-subset(stom,Stom.var>0 & stom.used.like==1 ,select=c(Area,Predator,Quarter,Year,Predator.length.class ,Predator.length.mean,Prey,Prey.length.mean,N.samples,stomcon,stomcon.hat,pl.res,stan.residual))
a<-a[order(abs(a$pl.res),decreasing =T),]
head(a,20)
#tail(a,20)


###
a<-a[order(abs(a$Residual),decreasing =T),]
head(a,20)


a<-unique(subset(stom,Stom.var>0 & stom.used.like==1 ,select=c(Predator,Quarter,Year,Predator.length.class ,N.samples,Diri.like)))
#a<-a[order(abs(a$Diri.like),decreasing =T),]
a<-a[order(a$Diri.like,decreasing =T),]
head(a,20)

tail(a,20)
###############################################################
#Number of stomachs;
ns<-unique(subset(stom,select=c(Predator,Predator.no,Predator.length,Predator.length.class,Year,Quarter, N.samples)))
ns<-ns[order(ns$N.sample),]
head(ns)

ftable(tapply(ns$N.sample,list(ns$Predator,ns$Year,ns$Quarter,ns$Predator.length),sum,na.rm=T))
tapply(ns$N.sample,list(ns$Predator,ns$Predator.length),sum,na.rm=T)
tapply(ns$N.sample,list(ns$Predator),sum,na.rm=T)
X11()
hist(ns$N.sample, freq = T,nclass=50,xlab='number of stomachs per sample',main='')

###############################################################
# Buble residual plot
#
stomBuble<-function(my.pred='Cod',my.area=1,my.dev=my.dev.used,filename='a',time.order="quarter-year",pred.order="pred-size",pred.prey.order="prey-pred",scale=2,Res.type=1,use.likelihood=F,make.table=F,...) {
    # res type
    # 1= observed-predicted
    # 2= standardised
    # 3= Peter Lewys suggestion
    
    area.names<-Read.area.names()
    stom3<-(subset(stom,Predator %in% my.pred & Size.model %in% c(0,1,4,11) & Area==my.area,
       select=c(Predator,Quarter,Year,Predator.length.class ,Predator.length,Prey,Prey.no,N.samples,Diri.like,stan.residual,Residual,pl.res)))

    if (use.likelihood) {
      stom3<-unique(subset(stom3,select=c(-stan.residual,-Residual,-Prey,-Prey.no)))
      stom3$Residual<-stom3$Diri.like
    } else if (Res.type==2) stom3$Residual<-stom3$stan.residual else if (Res.type==3) stom3$Residual<-stom3$pl.res
    

    if (time.order== "quarter-year") stom3$time<-paste(stom3$Quarter,stom3$Year,sep=' ')
    if (time.order== "year-quarter") stom3$time<-paste(stom3$Year,stom3$Quarter,sep=' ')
    if (use.likelihood) stom3$plPrey<-'' else stom3$plPrey<-paste(stom3$Prey.no,") ",stom3$Prey,sep='')
    if (pred.order=="pred-size") stom3$plPred<-paste(stom3$Predator,stom3$Predator.length/10,sep='-')
    if (pred.order=="size-pred") stom3$plPred<-paste(stom3$Predator.length/10,stom3$Predator,sep='-')

    s<-droplevels(subset(stom3,select=c(time,plPrey,plPred,Predator,Predator.length,Residual)))
    if (pred.prey.order=="pred-prey") a<-tapply(s$Residual,list(s$plPred,s$plPrey,s$time),sum,na.rm=T)
    if (pred.prey.order=="prey-pred") a<-tapply(s$Residual,list(s$plPrey,s$plPred,s$time),sum,na.rm=T)

    if (make.table) ftable(round(a,3))
    bb<-data.frame(ftable(a))
    names(bb)<-c('Prey','Pred','Period','Value')
    ylabs<-sort(unique(paste(bb$Prey,bb$Pred)))
    xlabs<-sort(unique(bb$Period))
    b<-as.matrix(ftable(a))
    b<-arr2df(b)
    names(b)<-c('y','x','v')
    b<-subset(b,!is.na(v))

    bp3<-function(x,y,v, scale=3,xlabs,ylabs,...){
      plot(x,y,cex=sqrt(abs(v))*scale, col='seagreen3', pch=ifelse(v<0,16,1), axes=F,ylab='',xlab='',main=area.names[my.area])
      axis(2,at=1:length(ylabs),labels=ylabs,las=1,cex.axis=0.7)
      axis(1,at=1:length(xlabs),labels=xlabs,las=2,cex.axis=0.7)
      box()
      points(x,y,cex=sqrt(abs(v))*scale, col='red', pch=1)
    }

    newplot(dev=my.dev,nox=1,noy=1,filename=filename,...);
    par(mar=c(4,6,1,2)) #c(bottom, left, top, right)
    bp3(x=as.numeric(b$x),y=b$y,v=b$v, scale=scale,xlabs=xlabs,ylabs=ylabs)
}



cleanup()

my.dev.used='png'
#stomBuble(my.area=1,my.pred='Cod',time.order="quarter-year",pred.order="pred-size",scale=1,Res.type=2)
for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',time.order="quarter-year",pred.order="pred-size",scale=2,Res.type=1,filename='stom_res1_quarter-year')
for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',time.order="quarter-year",pred.order="pred-size",scale=1.3,Res.type=2,filename='stom_res2_quarter-year')
for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',time.order="quarter-year",pred.order="pred-size",scale=1.3,Res.type=3,filename='stom_res3_quarter-year')

for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',time.order="year-quarter",pred.order="pred-size",scale=2,Res.type=1,filename='stom_res1_year-quarter')
for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',time.order="year-quarter",pred.order="pred-size",scale=1.3,Res.type=2,filename='stom_res2_year-quarter')
for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',time.order="year-quarter",pred.order="pred-size",scale=1.3,Res.type=3,filename='stom_res3_year-quarter')

for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',my.dev='png',filename=paste('stom_likelihood1',are,sep='_'),time.order="year-quarter",pred.order="pred-size",scale=1,use.likelihood=T)
for (are in (1:no.areas)) stomBuble(my.area=are,my.pred='Cod',my.dev='png',filename=paste('stom_likelihood2',are,sep='_'),time.order="quarter-year",pred.order="pred-size",scale=1,use.likelihood=T)

cleanup()

my.pred<-"Cod"; my.area<-1
stom3<-(subset(stom,Predator %in% my.pred & Size.model %in% c(0,4,11) & Area==my.area,
     select=c(Predator,Quarter,Year,Predator.length.class ,Predator.length,Prey,Prey.no,N.samples,Diri.like,stan.residual,Residual,pl.res)))
other<-subset(stom3,Prey=='Other',select=c( Quarter, Year, Predator.length.class , Prey, N.samples ))
head(other)
cod<-subset(stom3,Prey=='Cod',select=c( Quarter, Year, Predator.length.class ))
head(cod)
cod$present<-1
a<-merge(x=cod,y=other,all.y=T)
head(a)
a[is.na(a$present),'present']<-0
a$period<-'1977-1981'
a[a$Year>1981,'period']<-'1982-1992'
ftable(round(tapply(a$present,list(a$Year,a$Predator.length.class),mean),2))
ftable(round(tapply(a$present,list(a$period,a$Predator.length.class),mean),2))

##############################################################
# Box-plot of residuals by predator, prey and quarter
# prog box 1
Res.type<-3

nox<-3; noy<-4;
#cleanup()
#newplot(dev="screen",filename='stom_box',nox,noy,Portrait=F);
newplot(dev="png",filename='stom_box',nox,noy,Portrait=F);
 par(mar=c(3,2,3,2)) #c(bottom, left, top, right)
i<-0

if (Res.type==2) stom3$Residual<-stom3$stan.residual else if (Res.type==3) stom3$Residual<-stom3$pl.res

res.ylim<-c(-0.05,0.05)
res.ylim<-c(-0.05,0.05)
 res.ylim<-c(-0.1,0.1)

my.pred<-c("Cod")
stom3<-subset(stom,Predator %in% my.pred,drop=TRUE)
stom3$Length<-stom3$Predator.length/10
stom3$yq<-paste(stom3$Year,stom3$Quarter)
my.col<-"red"
by(stom3,list(stom3$Predator,stom3$Prey,stom3$Area),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 ;  par(mar=c(3,2,3,2)) }#c(bottom, left, top, right)
   if (x[1,"Prey"]=="Other" ) fac<-1 else fac<-1

   boxplot(Residual~yq,data=x,ylim=6*fac*res.ylim)
   abline(h=0,col=my.col)
 #  title(main=paste(area.names[x[1,'Area']],x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
   title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
   i<<-i+1

   boxplot(Residual~Year,data=x,ylim=6*fac*res.ylim)
   abline(h=0,col=my.col)
   title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
   i<<-i+1

   boxplot(Residual~Quarter,data=x,ylim=3*fac*res.ylim)
   abline(h=0,col=my.col)
   title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
   i<<-i+1

   boxplot(Residual~Length,data=x,ylim=3*fac*res.ylim)
   abline(h=0,col=my.col)
   title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
   i<<-i+1

})


cleanup()



##############################################################
# Box-plot of residuals by predator, prey and quarter
# prog box 1
Res.type<-3

nox<-no.areas; noy<-1;
cleanup()
newplot(dev="screen",filename='stom_box',nox,noy,Portrait=F);
#newplot(dev="png",filename='stom_box',nox,noy,Portrait=F);
 par(mar=c(3,2,3,2)) #c(bottom, left, top, right)
i<-0

if (Res.type==2) stom3$Residual<-stom3$stan.residual else if (Res.type==3) stom3$Residual<-stom3$pl.res

if (Res.type==2) res.ylim<-c(-3,3)
if (Res.type==3)res.ylim<-c(-0.04,0.04)

my.pred<-c("Cod")
stom3<-subset(stom,Predator %in% my.pred,drop=TRUE)
stom3$Length<-stom3$Predator.length/10
stom3$yq<-paste(stom3$Year,stom3$Quarter)
my.col<-"red"
by(stom3,list(stom3$Area,stom3$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   if (x[1,"Prey"]=="Other" ) fac<-5 else fac<-1

   boxplot(Residual~Prey,data=x,main=area.names[x[1,'Area']])
   abline(h=0,col=my.col)
   title(main='')
   i<<-i+1
})



 ##############################################################
#Dirichlet
##

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0

a<-subset(stom,Quarter.no>0)
a<-unique(a)
xyplot(Diri.sum.p~N.samples,
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)

xyplot(Diri.like~N.samples,
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)


xyplot(Diri.like~N.samples|Predator,group=Predator.length.class,
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)

xyplot(stomcon.hat*(1-stomcon.hat)/(Diri.sum.p+1)*N.haul ~stomcon.hat,group=Predator, ylab="VAR(STOM)",
      auto.key = list(space = "right", points = T, lines = TRUE),data=a)


##############################################################
#observed and predicted stomach contents by Predator and prey
# prog stom 1

cleanup()
nox<-2; noy<-2;
newplot(dev="screen",filename='Stom_obs_predicted',nox,noy,Portrait=F);
newplot(dev="png",filename='Stom_obs_predicted',nox,noy,Portrait=F);

i<-0
stom2<-subset(stom,stomcon>0)
by(stom2,list(stom2$Prey,stom2$Predator),function(x) {
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

##########################################################
stom3<-droplevels(subset(stom,  (Size.model==1 | Size.model==11) &  Prey.no>0) )

# use the individual data points (not the liklihood data) when size mode is 11
stom3[stom3$Size.model==11,]$stomcon<-    stom3[stom3$Size.model==11,]$stom.input
stom3[stom3$Size.model==11,]$stomcon.hat<-stom3[stom3$Size.model==11,]$stomcon.hat.part

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
               color = T, width=18, height=22,pointsize = 12,
               new = TRUE, retain = FALSE)


b1<-a
b1$type<-paste("a) ", a$headt,'\nObserved',sep='');

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')
b2<-a

b2$type<-paste("b) ", a$headt,'\nPredicted',sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)

trellis.device(device = "windows",
               color = T, width=7, height=7,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|type, data=aa,
     groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis

    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1.5, lines=2),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(1, 1) ,col='grey')


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
col<-1:3
lab<-dimnames(b)[[1]]

#cleanup()
trellis.device(device = "windows",
               color = T, width=7, height=9,pointsize = 2,
               new = TRUE, retain = FALSE)


b1<-a
b1$type<-paste("a) ",a$headt,'\nObserved',sep='');
b2<-a

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')
b2<-a
b2$type<-paste("b) ",a$headt,'\nPredicted',sep='');

# BALTIC both on the same plot b
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)

trellis.device(device = "windows",
               color = T, width=10, height=7,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|type, data=aa,
      groups = Prey, stack = TRUE,  col=1:3,    # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:3),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(2, 1) )
