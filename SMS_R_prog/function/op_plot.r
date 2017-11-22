OP_plot<-function(dat,incl.M2.plot=T) {


cleanup()

dev<-"screen"

if (incl.M2.plot) { nox<-2; noy<-3;} else { nox<-2; noy<-2;}
noxy<-nox*noy

dat<-res.all
  dat$Species<-sp.names[dat$Species.n]
  if (no.areas >1) {
    dat$N.bar<-dat$N*(1-exp(-dat$Z))/dat$Z
    dat$DM<-dat$M*dat$N.bar
    dat$DM1<-dat$M2*dat$N.bar
    dat$DM2<-dat$M2*dat$N.bar
    dat$DF<-dat$F*dat$N.bar
    dat$DZ<-dat$Z*dat$N.bar
    dat$Nwest<-dat$N*dat$west
    dat$Cweca<-dat$C*dat$weca

    dat<-aggregate(cbind(DM,DM1,DM2,DF,DZ,N,C,Nwest,Cweca,Yield,CWsum,BIO,SSB)~Species+Year+Quarter+Species.n+Age,sum,na.rm=T,data=dat)
    dat$Z<- -log((dat$N-dat$DZ)/dat$N)
    dat$M<-dat$DM/dat$DZ*dat$Z
    dat$M1<-dat$DM1/dat$DZ*dat$Z
    dat$M2<-dat$DM2/dat$DZ*dat$Z
    dat$F<-dat$DF/dat$DZ*dat$Z

    dat$weca<-dat$Cweca/dat$C
    dat[is.na(dat$weca),'weca']<-0
    dat$west<-dat$Nwest/dat$N
   }

  dat$N.bar<-dat$N*(1-exp(-dat$Z))/dat$Z
  dat$C<-NULL
  dat$N_dist<-NULL
  dat$Area<-NULL
  dat <-subset(dat, select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,BIO,SSB))

if (incl.M2.plot) {
  dat<-data.frame(dat,deadM1=dat$M1*dat$N.bar*dat$west,deadM2=dat$M2*dat$N.bar*dat$west,deadM=dat$M*dat$N.bar*dat$west)
 }

for (sp in (first.VPA:nsp)){
    sp.name<-sp.names[sp]

    X11(width=6, height=8)
    par(mar=c(3,4,3,2))
    par(mfcol=c(noy,nox))

    s<-subset(dat,Species.n==sp)

    s1<-subset(s,s$Age>=av.F.age[sp,1] & s$Age<=av.F.age[sp,2])
    FI<-tapply(s1$F,list(s1$Year),sum)/(av.F.age[sp,2]-av.F.age[sp,1]+1)

    s1<-subset(s,weca>=0 )

    Yield<-tapply(s1$Yield,list(s1$Year),sum)/1000
    SOP<-tapply(s1$CWsum,list(s1$Year),sum)/1000
    catch<-Yield
    s1<-subset(s,Quarter==1)
    ssb<-tapply(s1$SSB,list(s1$Year),sum)/1000
    s2<-subset(s,Age==fa & Quarter==rec.season)
    rec<-tapply(s2$N,list(s2$Year),sum)/1000000
    year<-unlist(dimnames(ssb))
    year.ssb<-year

    barplot(catch,space=1,xlab='',ylab='1000 tonnes',main=paste(sp.name,',  Catch',sep=''),ylim=c(0,max(SOP)))

    #plot recruits
    #plot(year,rec,type='h',lwd=5,xlab='',ylab='billions',main=paste('Recruitment age',fa),ylim=c(0,max(rec)))
    barplot(rec,space=1,xlab='',ylab='billions',main=paste('Recruitment age',fa),ylim=c(0,max(rec)))

    plot(year,FI,type='b',lwd=3,xlab='',ylab='',main="Fishing mortality",ylim=(c(0,max(FI))))
   grid()

       SSB.max<-max(ssb)
    plot(year.ssb,ssb,type='b',lwd=3,xlab='',ylab='1000 tonnes',main='SSB',ylim=c(0,SSB.max))
     grid()
    if (incl.M2.plot) {
      deadM1<-tapply(s$deadM1,list(s$Year),sum)/1000
      deadM2<-tapply(s$deadM2,list(s$Year),sum)/1000
      deadM1<-deadM1[1:length(SOP)]
      deadM2<-deadM2[1:length(SOP)]
      barplot(rbind(SOP,deadM1,deadM2),space=1,main='Biomass removed\ndue to F, M1 and M2',ylab='1000 tonnes')

      b<-tapply(s$M2,list(s$Year,s$Age),sum)
      b<-b[1:length(SOP),]
      if (sum(b,na.rm=T)>=0.01) {
        y<-as.numeric(dimnames(b)[[1]])
        plot(y,b[,1],main=paste("M2 at age"),xlab="",ylab='M2',
                type='l',lwd=1.5,ylim=c(0,max(b,na.rm=T)))
        for (a in (2:(dim(b)[2]))) if(max(b[,a],na.rm=T)>0.001) lines(y,b[,a],lty=a,col=a,lwd=2)
      }
     }
}
}