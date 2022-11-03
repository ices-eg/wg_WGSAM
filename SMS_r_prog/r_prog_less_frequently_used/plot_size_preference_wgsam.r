
stom<-Read.stomach.data()

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

################################################


####################################
# observation by predator-prey size ratio
## prog size 1b

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0



stom3<-subset(stom,  (Size.model==1 | Size.model==11) &  Prey.no>0 & Predator %in% c('H. porpoise','Grey seal'))

stom3<-subset(stom,  (Size.model==1 | Size.model==11) &  Prey.no>0 )


# just checking
a<-aggregate(list(stom=stom3$stomcon,stom.input=stom3$stom.input,stom.hat=stom3$stomcon.hat,stom.hat.part=stom3$stomcon.hat.part),
             list(Year=stom3$Year,Quarter=stom3$Quarter,Predator=stom3$Predator,Pred_l=stom3$Predator.length),sum)

# use the individual data points (not the liklihood data) when size mode is 11
stom3[stom3$Size.model==11,]$stomcon<-    stom3[stom3$Size.model==11,]$stom.input
stom3[stom3$Size.model==11,]$stomcon.hat<-stom3[stom3$Size.model==11,]$stomcon.hat.part


# to get rit of "Other"
write.csv(stom3, file = file.path(data.path,'junk.csv'),row.names = FALSE)
stom3<-read.csv(file = file.path(data.path,'junk.csv'))

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
a$headt<-paste(a$Predator,", mean=",formatC(a$mean,digits=2,format='f')," var=",formatC(a$var,digits=2,format='f'),sep='')

cleanup()
trellis.device(device = "windows",
               color = T, width=9, height=17,pointsize = 12,
               new = TRUE, retain = FALSE)
cleanup()
trellis.device(device = "windows",
               color = T, width=18, height=22,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|headt, data=a,
    groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.6, lines=2),
    scales = list(x = list(rot = 90, cex=0.5), y= list(alternating = 1,cex=0.5)),
    layout = c(3,4) ,col='grey')


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
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.6, lines=2),
    scales = list(x = list(rot = 90, cex=0.5), y= list(alternating = 1,cex=0.5)),
    layout = c(3,4) ,col='grey')




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
a$headt<-paste(a$Predator,", mean=",formatC(a$mean,digits=2,format='f')," var=",formatC(a$var,digits=2,format='f'),sep='')

#cleanup()
trellis.device(device = "windows",
               color = T, width=17, height=17,pointsize = 12,
               new = TRUE, retain = FALSE )

b<- tapply(stom3$stomcon,list(stom3$Prey),sum)
dim(b)

col<-1:dim(b)
lab<-dimnames(b)[[1]]


 barchart(stom~as.factor(PredPrey)|headt, data=a,
 groups = Prey, stack = TRUE,  col=1:dim(b),
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.75, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:dim(b)),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(2, 2) )


b1<-a
b1$type<-paste(a$headt,'\nObserved',sep='');

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')
trellis.device(device = "windows",
               color = T, width=17, height=17,pointsize = 12,
               new = TRUE, retain = FALSE )


 barchart(stom.hat~as.factor(PredPrey)|headt, data=a,
 groups = Prey, stack = TRUE,  col=1:dim(b),
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.75, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:dim(b)),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(2, 2) )

b2<-a

b2$type<-paste(a$headt,'\nPredicted',sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)

trellis.device(device = "windows",
               color = T, width=17, height=17,pointsize = 12,
               new = TRUE, retain = FALSE)

 barchart(stom~as.factor(PredPrey)|type, data=aa,
     groups = Prey, stack = TRUE,  col=1:7,    # this line is not needed, but it gives a nicer offset on the Y-axis
    xlab='log(predator weight/prey weight)',ylab='Proportion',
    strip = strip.custom( bg='white'),par.strip.text=list(cex=0.75, lines=2),
    key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=1:7),
    scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
    layout = c(2, 3) )
