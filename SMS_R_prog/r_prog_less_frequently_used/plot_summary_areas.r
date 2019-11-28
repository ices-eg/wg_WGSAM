
paper<-F                   # output on paper (=TRUE) or screen (=FALSE)

Portrait<-F                 # graphical output orientation
include.terminal.year <- T          # plot terminal year (last assessment year +1) as well?
include.last.assess.year.recruit <- T          # plot recruits terminal year as well?

first.year<- 1974                #first year on plot, negative value means value defined by data
last.year<- 2011                 #last year on plot

##########################################################################

cleanup()
 
if (paper) dev<-"wmf" else dev<-"screen"

Init.function()

dat<-Read.summary.data.areas(extend=include.terminal.year,read.init.function=F)
dat<-subset(dat,Year<=last.year )
if (first.year>0) dat<-subset(dat,Year>=first.year )

# Dead in numbers caused by predation
nox<-2; noy<-3;
noxy<-nox*noy

for (sp in (first.VPA:nsp)){
    sp.name<-sp.names[sp]

    newplot(dev,nox,noy,filename=paste(file.name,'_',sp.name,sep=''),Portrait=Portrait)
    par(mar=c(3,4,3,2))

    s<-subset(dat,Species.n==sp)
    for (a in (SMS.control@first.age :min(SMS.control@species.info[sp,'last-age'],5))){
       s1<-subset(s,Age==a )
       if (sum(s1$DeadM2)>0) {
         deadM2<-tapply(s1$DeadM2,list(s1$Area,s1$Year),sum)/1000
         cat("\nsp:",sp," Age:",a,"\n"); print(ftable(deadM2))
         barplot(deadM2,space=1,xlab='',ylab='millions',main=paste(sp.name,' age:',a,sep=''),ylim=c(0,max(apply(deadM2,2,sum))))
       }
    }
}
    
##############################
# M2 by quarter
cleanup()
nox<-2; noy<-3;
noxy<-nox*noy

dat0<-Read.summary.data(extend=include.terminal.year,read.init.function=F)
dat0<-subset(dat0,Year>=min(dat$Year) & Year<=max(dat$Year),select=c(Species,Year,Quarter,Species.n,Age,M2))
dat0$Area<-0


for (sp in (first.VPA:nsp)){
    sp.name<-sp.names[sp]
    s <-subset(dat, Species.n==sp,select=c(Area,Species,Year,Quarter,Species.n,Age,M2))
    s0<-subset(dat0,Species.n==sp,select=c(Area,Species,Year,Quarter,Species.n,Age,M2))
    for (q in (1:SMS.control@last.season)) {
       s1<-subset(s,Quarter==q )
       newplot(dev,nox,noy,filename=paste(file.name,'_',sp.name,sep=''),Portrait=Portrait)
       par(mar=c(3,4,3,2))

       for (a in (SMS.control@first.age :min(SMS.control@species.info[sp,'last-age'],5))){
           s2<-subset(s1,Age==a )
           s01<-subset(s0,Age==a & Quarter==q)
           s2<-rbind(s01,s2)
           
           b<-tapply(s2$M2,list(s2$Year,s2$Area),sum)
           if (sum(b,na.rm=T)>=0.01) {
            #cat("\nsp:",sp," Age:",a,"\n"); print(ftable(b))

             y<-as.numeric(dimnames(b)[[1]])
             plot(y,b[,1],main=paste(sp.name," Q:",q," Age:",a),xlab="",ylab='local M2',type='b',pch='0',lwd=1.5,ylim=c(0,max(b,na.rm=T)))
             for (aa in (2:(dim(b)[2]))) if(max(b[,aa],na.rm=T)>0.001) lines(y,b[,aa],col=aa,lwd=1.5,type='b',pch=as.character(aa-1))
          }
       }
    }
}

##############################
# sum of quarterly M2
cleanup()
nox<-2; noy<-2;
noxy<-nox*noy

dat0<-Read.summary.data(extend=include.terminal.year,read.init.function=F)
dat0<-subset(dat0,Year>=min(dat$Year) & Year<=max(dat$Year),select=c(Species,Year,Quarter,Species.n,Age,M2))
dat0$Area<-0
cleanup()
palette(rainbow(4))

for (sp in (first.VPA:nsp)){
  sp.name<-sp.names[sp]
  s <-subset(dat, Species.n==sp,select=c(Area,Species,Year,Quarter,Species.n,Age,M2))
  s0<-subset(dat0,Species.n==sp,select=c(Area,Species,Year,Quarter,Species.n,Age,M2))
  newplot(dev,nox,noy,filename=paste(file.name,'_',sp.name,sep=''),Portrait=Portrait)
  par(mar=c(3,4,3,2))

  for (a in (SMS.control@first.age :min(SMS.control@species.info[sp,'last-age'],3))){
    s2<-rbind(subset(s,Age==a ),subset(s0,Age==a))

    b<-tapply(s2$M2,list(s2$Year,s2$Area),sum)
    if (sum(b,na.rm=T)>=0.01) {
         y<-as.numeric(dimnames(b)[[1]])
         plot(y,b[,1],main=paste(sp.name," Age:",a),xlab="",ylab='local M2',type='b',pch='0',lwd=2.5,ylim=c(0,max(b,na.rm=T)))
        # for (aa in (2:(dim(b)[2]))) if(max(b[,aa],na.rm=T)>0.001) lines(y,b[,aa],col=aa,lwd=1.5,type='b',pch=as.character(aa-1))
        pchs<-c('5','6','8')
        for (aa in (2:(dim(b)[2]))) if(max(b[,aa],na.rm=T)>0.001) lines(y,b[,aa],col=c('grey','red','blue','green')[aa],lwd=2.0,type='b',pch=pchs[aa-1])

    }
  }
}
