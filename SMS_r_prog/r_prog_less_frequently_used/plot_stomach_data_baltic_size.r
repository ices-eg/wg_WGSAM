plot_all_figs<-FALSE


stom2<-subset(stom,Size.model>0 & stom$Prey.no != 0)

if (plot_all_figs) {
  nox<-4; noy<-4;
  #cleanup()
  dev<-'screen'
  newplot(dev="screen",filename='stom_box',nox,noy,Portrait=F);
  #newplot(dev="png",filename='stom_box',nox,noy,Portrait=F);
  par(mar=c(3,2,3,2)) #c(bottom, left, top, right)
  i<-0
  
  if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
    by(stom2,list(stom2$Predator),function(x) {
      if (i==noxy) {newplot(dev,nox,noy); i<<-0 }  
      hist(log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(pred-size / prey-size) ')
      abline(h=0, lty=3) 
      title(main=paste("Pred:",x[1,]$Predator,sep=""))
      i<<-i+1
    }
    )}
}
##################################


stom3<-subset(stom,Size.model>0 & Prey.no>0 & N.haul>=10 & Predator !="NS Mackerel" & Predator !="W Mackerel" )
#stom3<-subset(stom,Size.model>0 & Prey.no>0 & N.haul>10  )
#stom3<-subset(stom3, !((Predator=='Cod' & Predator.length.mean<400)| (Predator=='Whiting' & Predator.length.mean<200)  | (Predator=='Saithe' & Predator.length.mean<400)))

# start with a high fac for calc of mean and var
fac<-50  
a<-aggregate(list(stom=stom3$stomcon),list(Predator=stom3$Predator,PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*50)/50),sum)

by(a,list(a$Predator),function(x) weighted.mean(x$PredPrey, x$stom))


if (plot_all_figs) barchart(stom~as.factor(trunc(PredPrey*2)/2)|Predator, data=a, 
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
if (plot_all_figs) {
  cleanup()
  trellis.device(device = "windows",
                 color = T, width=18, height=18,pointsize = 12,
                 new = TRUE, retain = FALSE)
  
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
if (plot_all_figs) {
  trellis.device(device = "windows", 
                 color = T, width=9, height=9,pointsize = 22,
                 new = TRUE, retain = FALSE)
  
  # for the paper
  barchart(stom.hat~as.factor(PredPrey)|headt, data=a, 
           groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
           xlab='log(predator weight/prey weight)',ylab='proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=0.8, lines=2),
           scales = list(x = list(rot = 45, cex=0.8), y= list(alternating = 1,cex=0.8)),
           layout = c(1, 1) ,col='grey')
}

b2<-a

b2$type<-paste(a$headt,'\nPredicted',sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)

if (plot_all_figs) {
  trellis.device(device = "windows", 
                 color = T, width=17, height=16,pointsize = 12,
                 new = TRUE, retain = FALSE)
  
  barchart(stom~as.factor(PredPrey)|type, data=aa, 
           groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
           
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(2, 1) ,col='grey')
}

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
if (plot_all_figs) {
  trellis.device(device = "windows",
                 color = T, width=7, height=7,pointsize = 2,
                 new = TRUE, retain = FALSE)
  
  
  barchart(stom~as.factor(PredPrey)|headt, data=a,
           groups = Prey, stack = TRUE,  col=2:3,
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=2:3),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(1, 1) )
}

b1<-a
#b1$type<-paste(a$headt,'\nObserved',sep='');
b1$type<-paste('Observed\n',a$head,sep='');

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')
if (plot_all_figs) {
  trellis.device(device = "windows",
                 color = T, width=7, height=7,pointsize = 2,
                 new = TRUE, retain = FALSE)
  
  barchart(stom.hat~as.factor(PredPrey)|headt, data=a,
           groups = Prey, stack = TRUE,  col=2:3,
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=2:3),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(1, 1) )
}

b2<-a

b2$type<-paste('Predicted\n',a$headt,sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)
aa<-droplevels(aa)

if (plot_all_figs) {
  trellis.device(device = "windows", 
                 color = T, width=9, height=7,pointsize = 12,
                 new = TRUE, retain = FALSE)
  
  
  barchart(stom~as.factor(PredPrey)|type, data=aa, 
           groups = Prey, stack = TRUE,  col=2:3,    # this line is not needed, but it gives a nicer offset on the Y-axis
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=2:3),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(2, 1) )
  
  
}

ggplot(aa, aes(x=PredPrey, y=stom, fill=Prey)) +
  geom_bar(stat="identity")+theme_minimal()+
  facet_wrap(~type,  ncol=1)+xlab('log(predator weight/prey weight)')+ylab('Proportion')
newplot(dev='png',nox=1,noy=1,Portrait=F,filename=paste('size_all','Cod',sep='-'),dir=stomach.dir,doGgplot=TRUE);



aa$type2<-if_else(grepl('Observed',aa$type),'Observed','Predicted')
ggplot(aa, aes(x=PredPrey, y=stom, fill=Prey)) +
  geom_bar(stat="identity")+theme_minimal()+
  facet_grid(rows=vars(type2),cols=vars(Prey))+xlab('log(predator weight/prey weight)')+ylab('Proportion')
newplot(dev='png',nox=1,noy=1,Portrait=F,filename=paste('size_prey','Cod',sep='-'),dir=stomach.dir,doGgplot=TRUE);

cleanup()

