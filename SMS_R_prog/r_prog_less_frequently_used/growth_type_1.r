write.growth.out<-T

dat<-Read.summary.data(read.init.function=F)

#clupeids<-c(2,3)      # Herring and sprat
clupeids<-c(3)      # sprat only
N<-subset(dat, Species.n %in% clupeids  & Quarter==1 )

clu.N<-aggregate(N~Year,data=N,sum)


g<-subset(dat,select=c(-M1,-M2,-M,-F,-Z,-N,-C.hat,-C.obs,-Yield,-CWsum,-propmat,-ration,-Lsea,-BIO,-SSB,-N.bar))
g$cohort<-g$Year-g$Age
head(g)


# Species Year Quarter Species.n Age     N.bar  west   weca
#1     Cod 1974       1         1   1 394432.00 0.065 0.0060  27920.4     0.00

g<-g[order(g$Species.n,g$cohort,g$Year,g$Quarter),]
ll<-dim(g)[1]
west1<-c(g$west[2:(ll)],0)
west2<-c(g$west[3:(ll)],0,0)
west3<-c(g$west[4:(ll)],0,0,0)
west4<-c(g$west[5:(ll)],0,0,0,0)
g$cohort4<-c(g$cohort[5:(ll)],0,0,0,0)

g<-data.frame(g,west1=west1,west2=west2,west3=west3,west4=west4)
g[g$Age==0,'west4']<-g[g$Age==0,'west2']
g[g$Age==0,'west3']<-g[g$Age==0,'west2']

g$growth1<-g$west1-g$west
g$growth2<-g$west2-g$west
g$growth3<-g$west3-g$west
g$growth4<-g$west4-g$west
head(g,20)

head(subset(g,Age<2 & Species.n==3 ),20)

g<-subset(g,cohort==cohort4 & west4>0 )

head(subset(g,Age<2 & Species.n==3 ),20)

g<-merge(clu.N,g)
g<-g[order(g$Species.n,g$cohort,g$Year,g$Quarter),]
g$age<-g$Age+(g$Quarter-1)*0.25

head(g,20)
if (F) {

  cleanup()
  a<-by(g,g$Species.n,function(g){
  trellis.device(height=10,width=10,pointsize=8)
  tit<-paste(g[1,'Species'],', mean weight by cohort',sep='')
  print(xyplot(west*1000~age| paste(cohort),data=g,layout = c(6, 6),subset=(cohort>=1972 & cohort<=2006 ) ,
    main=tit, ylab='Weight in the stock (gram)',
     panel=function(x,y) {
      panel.xyplot(x,y,col=1 ,pch=1,type='p')
      panel.loess(x,y, span=1)
      panel.lmline(x,y,col=2)
    }
  ))
  })


   cleanup()
  a<-by(g,g$Species.n,function(g){
  trellis.device(height=10,width=10,pointsize=8)
  tit<-paste(g[1,'Species'],', mean weight by cohort, Q1',sep='')
  print(xyplot(west*1000~age| paste(cohort),data=g,layout = c(6, 6),subset=(cohort>=1972 & cohort<=2006 & Quarter==1) ,
    main=tit, ylab='Weight in the stock (gram)',
     panel=function(x,y) {
      panel.xyplot(x,y,col=1 ,pch=1,type='p')
      panel.loess(x,y, span=1)
      panel.lmline(x,y,col=2)
    }
  ))
  })


   cleanup()
  a<-by(g,g$Species.n,function(g){
  trellis.device(height=10,width=10,pointsize=8)
  tit<-paste(g[1,'Species'],', Annual Growth by cohort, Q1',sep='')
  print(xyplot(growth4*1000~Age| paste(cohort),data=g,layout = c(6, 6),subset=(cohort>=1972 & cohort<=2006 & Quarter==1) ,
    main=tit, ylab='Growth (gram)',
     panel=function(x,y) {
      panel.xyplot(x,y,col=1 ,pch=1,type='p')
      panel.loess(x,y, span=1)
      panel.lmline(x,y,col=2)
    }
  ))
  })


   cleanup()
  a<-by(g,g$Species.n,function(g){
  trellis.device(height=10,width=10,pointsize=8)
  tit<-paste(g[1,'Species'],', Growth by cohort',sep='')
  print(xyplot(growth4*1000~age| paste(cohort),data=g,layout = c(6, 6),subset=(cohort>=1972 & cohort<=2006) ,
    main=tit, ylab='Weight in the stock (gram)',
     panel=function(x,y) {
      panel.xyplot(x,y,col=1 ,pch=1,type='p')
      panel.loess(x,y, span=1)
      panel.lmline(x,y,col=2)
    }
  ))
  })


  cleanup()
  a<-by(g,g$Species.n,function(g){
  trellis.device(height=10,width=10,pointsize=8)
  tit<-paste(g[1,'Species'],', Growth versus clupeid abundance, Q1',sep='')
  print(xyplot(growth4*1000~N/1000000 |paste( Species,Age),data=g,layout = c(3,3),subset=( Quarter==1) ,
    main=tit, ylab='Annual growth (gram)',xlab='Clupeid abundance (10^9)',
     panel=function(x,y) {
      panel.xyplot(x,y,col=1 ,pch=1,type='p')
      panel.loess(x,y, span=1)
      panel.lmline(x,y,col=2)
    }
  ))
  })


  cleanup()
  a<-by(g,g$Species.n,function(g){
  trellis.device(height=10,width=10,pointsize=8)
  tit<-paste(g[1,'Species'],', Growth versus clupeid abundance, Q1',sep='')
  print(xyplot(growth4*1000~N/1000000 |paste( Species,Age),data=g,layout = c(3,3),subset=( Quarter==1 | (Age==0 & Quarter==3)) ,
    main=tit, ylab='Annual growth,(half yearly for 0-groups (gram)',xlab='Clupeid abundance (10^9)',
     panel=function(x,y) {
      panel.xyplot(x,y,col=1 ,pch=1,type='p')
      panel.loess(x,y, span=1)
      panel.lmline(x,y,col=2)
    }
  ))
  })
}

gg<-subset(g,Quarter==1 | (Quarter==3 & Age==0))

#dat<-subset(gg,Species.n==3 & Age==4)
cleanup()
aa<-by(gg,list(gg$Age,gg$Species.n),function(dat) {


  m<-lm(west4~west+N,data=dat)
  newplot(nox=2,noy=2,dev='screen')
  plot(m,main=paste("sp:",dat[1,'Species.n']," Age:",dat[1,'Age']))
  
  newplot(nox=2,noy=2,dev='screen')
  plot(dat$N,dat$west4,xlab='Clupeid abundance',ylab="W(t+1)",main=paste("sp:",dat[1,'Species.n']," Age:",dat[1,'Age']))
  plot(dat$N,dat$growth4,xlab='Clupeid abundance',ylab="growth, W(t+1)-w(t)")
  plot(dat$west,dat$west4,xlab='W(t)',ylab="W(t+1)")
  abline(a=0,b=1,col='red')

  pred.w.plim <- predict(m, dat, interval="prediction")
  pred.w.clim <- predict(m, dat, interval="confidence")
  matplot(dat$Year,cbind(pred.w.clim, pred.w.plim[,-1]),
          lty=c(1,2,2,3,3), type="l", ylab="predicted",xlab='')
  points(dat$Year,dat$west4)

  return(list(sp=dat[1,'Species.n'],Age=dat[1,'Age'],mod=m))

})


#subset(g,Species.n==3 & Age==1 & Quarter==1)

mm<-lapply(aa,function(x)coef(x$mod))
mm<-matrix(unlist(mm),ncol=3,byrow=T)
mm<-data.frame(mm)
names(mm)<-c('Intercept','Wt','Clup_N')
ll<-dim(mm)[1]

sp<-unlist(lapply(aa,function(x)x$sp)); sp<-sp[1:ll]
age<-unlist( lapply(aa,function(x)x$Age));age<-age[1:ll]
mm$sp<-sp
mm$Age<-age
mm

a<-lapply(aa,function(x){print(paste(sp.names[x$sp],"Age:",x$Age)); print(summary(x$mod))})



round(summary(unique(g$N))/1000000)

clup<-summary(unique(g$N))
clup<-clup[c(1,2,3,5,6)]
a<-data.frame(clup)
a$abun<-paste(1:5,rownames(a))

g1<-subset(dat,Quarter==1 | (Quarter==3 & Age==0))
g1<-aggregate(west~Species.n+Age,mean,data=g1)
g1<-merge(a,g1)

w<-tapply(g1$west,list(g1$abun,g1$Species.n,g1$Age),mean)
dimnames(w)[3]<-list(paste('Age',0:8,sep='-'))
dimnames(w)[2]<- list(sp.names)
ftable(w)
w[,,2:8]<-0
ftable(w)


mm

inter<-tapply(mm$Intercept,list(Sp=mm$sp,Age=mm$Age),sum)
Wt<-tapply(mm$Wt,list(Sp=mm$sp,Age=mm$Age),sum)
Clup_N<-tapply(mm$Clup_N,list(Sp=mm$sp,Age=mm$Age),sum)

for (abun in (1:5)) for (sp in (1:3)) for ( age in (0:7))  {
 a<-age+1  # age 0 has index 1
 w[abun,sp,a+1]<-inter[sp,a]+Wt[sp,a]*w[abun,sp,a]+ Clup_N[sp,a]*clup[abun]
}

ftable(round(w,5))

cleanup()
for (sp in (1:3)) {
 X11()
 matplot(x=0:8,y=t(w[,sp,]),type='b',lwd=3,main=sp.names[sp],xlab='Age')
 obs<-subset(dat,Species.n==sp & Quarter==1,select=c(Year,Age,west))
 points(obs$Age,obs$west)
}
w


cleanup()
for (sp in (1:3)) {
  X11()
  obs<-subset(dat,Species.n==sp & Quarter==1,select=c(Year,Age,west))
  boxplot(west~Age,data=obs,ylim=c(0,max(obs$west)),main=sp.names[sp])
 for (i in (1:5)) points(0:8,w[i,sp,],col=i,type='b',lwd=3)
}

#######

dat<-Read.summary.data(read.init.function=F)
g1<-subset(dat,select=c(-M1,-M2,-M,-F,-Z,-C.hat,-C.obs,-Yield,-CWsum,-propmat,-ration,-Lsea,-BIO,-SSB,-N.bar))
g1$cohort<-g1$Year-g1$Age
g1<-subset(g1,Quarter==1 | (Quarter==3 & Age==0))
obs<-tapply(g1$west,list(g1$Species.n,g1$cohort,g1$Age),sum)
ftable(round(obs,4))
pred<-obs
cl<-aggregate(N~Year,data=g1,FUN=sum,subset=(Species.n %in% clupeids))
cl<-tapply(cl$N,cl$Year,sum)

for (s in (1:3)) for (y in as.character(1974:2009)) for (a in (1:8)){
  cohort<-as.character(as.numeric(y)-a)
  #print(paste(y,cohort,a))
  pred[s,cohort,as.character(a)]<-inter[s,as.character(a-1)]+Wt[s,as.character(a-1)]*obs[s,cohort,as.character(a-1)]+Clup_N[s,as.character(a-1)]*cl[y]
}

pred<-arr2df(pred)
names(pred)<-c('Species.n','cohort','Age','w')
pred$type='predicted'
obs<-arr2df(obs)
names(obs)<-c('Species.n','cohort','Age','w')
obs$type='Observed'

a<-rbind(pred,obs)
a<-subset(a,!is.na(w) & paste(cohort)>="1974" & paste(cohort)<="2006")

cleanup()

b<-by(a,a$Species.n,function(g){
trellis.device(height=8,width=10,pointsize=8)
tit<-paste(sp.names[g[1,'Species.n']],', mean weight Quarter 1 by cohort',sep='')
print(xyplot(w*1000~Age| paste(cohort),group=type, data=g,layout = c(6, 6) ,
  main=tit, ylab='Weight in the stock (gram)' ,type='b'
))
})


###########
# changes from one quarter to the next
g1<-subset(g,Quarter==1 | (Quarter==3 & Age==0) )
q<-aggregate(list(ratio2=g1$west1/g1$west,ratio3=g1$west2/g1$west,ratio4=g1$west3/g1$west),
          list(Species.n=g1$Species.n,Quarter=g1$Quarter,Age=g1$Age),mean)
q$ratio1<-1
q<-subset(q,select=c(Species.n,Age,ratio1,ratio2,ratio3,ratio4))

q
a1<-subset(q,Species.n==1 & Age==7);a1$Age<-8
a2<-subset(q,Species.n==2 & Age==7);a2$Age<-8
a3<-subset(q,Species.n==3 & Age==6);a3$Age<-7
a4<-subset(q,Species.n==3 & Age==6);a4$Age<-8

g03<-aggregate(west~Species.n+Age,data=dat,subset=(Quarter==3 & Age==0),FUN=mean)
g04<-aggregate(west~Species.n+Age,data=dat,subset=(Quarter==4 & Age==0),FUN=mean)
a5<-data.frame(Species.n=g03$Species.n,Age=g03$Age,ratio1=1,ratio2=g04$west/g03$west,ratio3=1,ratio4=1)


q<-rbind(q,a1,a2,a3,a4)
q<-q[order(q$Species.n,q$Age),]
q<-subset(q,select=c(Species.n,Age,ratio1,ratio2,ratio3,ratio4))

#q.gem<-rbind(q,a5)
q.gem<-q
q.gem<-q.gem[order(q.gem$Species.n,q.gem$Age),]

q<-subset(q,select=c(ratio1,ratio2,ratio3,ratio4))

q<-as.matrix(q)


a<-seq(0,8.75,0.25)
sp<-2; i<-3;
t(w[i,sp,]*q[(1+(sp-1)*9):(9+(sp-1)*9),])
length(t(w[i,sp,]*q[(1+(sp-1)*9):(9+(sp-1)*9),]))
length(a)

cleanup()
for (sp in (1:3)) {
  X11()
  obs<-subset(dat,Species.n==sp & (Quarter==1 | (Quarter==0 & Age==3)),select=c(Year,Age,west))
  boxplot(west~Age,data=obs,ylim=c(0,max(obs$west)),main=sp.names[sp],xlab='Age',ylab='Mean weight (g)')
 for (i in (1:5)) points(a,t(w[i,sp,]*q[(1+(sp-1)*9):(9+(sp-1)*9),]),col=i,type='b',lwd=3)
}

# ratio between weca and west
obs<-subset(dat,weca>0 & west>0 & Year>2000)
obs$ratio<-obs$weca/obs$west
a<-tapply(obs$ratio,list(obs$Species.n,obs$Quarter,obs$Age),mean)
ftable(round(a,1))
ratio<-a

if (write.growth.out) {
  outfile<-file.path(data.path,'growth_type1.in')
  cat("# Density dependent Growth parameter for type 1 growth where W(t+1)=intercept+ a*W(t) + b* clupeid total N\n",
      "#  W(t+1) is for Quarter 1 \n",
      "#\n#  Intercept   a            b",file=outfile)
  mm.out<-mm
  mm.out$Age<-mm.out$Age+1

  dat<-Read.summary.data(read.init.function=F)

  obs<-subset(dat,west>0 & (Quarter==3 & Age==0) )
  a<-aggregate(west~Species.n+Age,mean,data=obs)
  names(a)<-c("sp","Age","Intercept")
  a$Wt<-a$Clup_N<-0
  a<-subset(a,select=c(Intercept, Wt, Clup_N, sp, Age))

  #all<-rbind(mm.out,a, data.frame(Intercept=0,Wt=0,Clup_N=0,sp=3,Age=8))
  all<-rbind(mm.out,a,data.frame(Intercept=0,Wt=0,Clup_N=0,sp=3,Age=8))

  all<-all[order(all$sp,all$Age),]
  for (s in (1:3)) {
    cat("#", sp.names[s],"\n",file=outfile,append=T)
    for (a in (0:8)){
      cat(all[all$sp==s & all$Age==a,"Intercept"],all[all$sp==s & all$Age==a,"Wt"],all[all$sp==s & all$Age==a,"Clup_N"]," #",sp.names[s]," Age",a,"\n",file=outfile,append=T)
  }}
  
  cat("#############################################################\n",
      "#  Ratio between weight in the first quarter and Q1 , Q2, Q3 and Q4\n",file=outfile,append=T)
  b<-q.gem
  for (s in (1:3)) {
    cat("#", sp.names[s],"\n",file=outfile,append=T)
    for (a in (0:8)){
      cat(unlist(b[b$Species.n==s & b$Age==a,3:6])," #",sp.names[s]," Age",a,"\n",file=outfile,append=T)
  }}

   cat("#############################################################\n",
      "#  Ratio between weca and west\n",file=outfile,append=T)
  ratio[is.na(ratio)]<-0
  ratio[ratio>2]<-2
  ratio[1,3,1]<-ratio[1,4,1]<-1
  write.table(ftable(round(ratio,2)),file=outfile,append=T,col.names=F,row.names=F)
  

  dat<-subset(Read.summary.data(read.init.function=F),Quarter==1 | (Quarter=3 & Age==0))
  minW<-aggregate(west~Species.n+Age,data=dat,FUN=quantile,p=0.1)
  names(minW)<-c("Species.n","Age","minW")
  maxW<-aggregate(west~Species.n+Age,data=dat,FUN=quantile,p=0.9)
  w<-merge(minW,maxW)

  minw<-tapply(w$minW,list(w$Species,w$Age),sum)
  minw[is.na(minw)]<-0
  cat("#############################################################\n",
      "#  Minimum Weight, Q1\n",file=outfile,append=T)
  write.table(round(minw,6),file=outfile,append=T,col.names=F,row.names=F)

  maxw<-tapply(w$west,list(w$Species,w$Age),sum)
  maxw[is.na(maxw)]<-0
  cat("#############################################################\n",
      "#  Maximum Weight, Q1\n",file=outfile,append=T)
  write.table(round(maxw,6),file=outfile,append=T,col.names=F,row.names=F)

  cleanup()
  aa<-by(w,list(w$Species.n),function(x) {
    X11()
    obs<-subset(dat,Species.n==x[1,"Species.n"]& Quarter==1)
   plot(x$Age,x$west,ylim=c(0,max(obs$west)),main=sp.names[x[1,"Species.n"]],type='b',col='red',lwd=3,xlab='Age',ylab='mean weight')
   points(x$Age,x$minW,type='b',col='red',lwd=3)
    points(obs$Age,obs$west,type='p',col='blue')
  })

  
}


dat<-subset(Read.summary.data(read.init.function=F),Quarter==1)
minW<-aggregate(west~Species.n+Age,data=dat,subset=(Quarter==1),FUN=quantile,p=0.1)
names(minW)<-c("Species.n","Age","minW")
maxW<-aggregate(west~Species.n+Age,data=dat,subset=(Quarter==1),FUN=quantile,p=0.9)
w<-merge(minW,maxW)

minw<-tapply(w$minW,list(w$Species,w$Age),sum)
minw[is.na(minw)]<-0

maxw<-tapply(w$west,list(w$Species,w$Age),sum)
maxw[is.na(maxw)]<-0

#cleanup()
aa<-by(w,list(w$Species.n),function(x) {
  X11()
  obs<-subset(dat,Species.n==x[1,"Species.n"]& Quarter==1)
 plot(x$Age,x$west*1000,ylim=c(0,max(obs$west*1000)),main=sp.names[x[1,"Species.n"]],type='b',col='red',lwd=3,xlab='Age',ylab='mean weight')
 points(x$Age,x$minW*1000,type='b',col='red',lwd=3)
  points(obs$Age,obs$west*1000,type='p',col='blue')
})

