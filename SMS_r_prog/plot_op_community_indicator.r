

Portrait<-F                 # graphical output orientation

first.year<- 1963                #first year on plot, negative value means value defined by data
last.year<- 2100               #last year on plot


HindcastModel<-T; # plot values from the hindcast
ForecastModel<-F; # plot values from the scenarios

redefine.scenario.manually<-T  # Define the scenario dir used explecitely in this script or do it externally

op.dir<-data.path
if (ForecastModel==T & redefine.scenario.manually)  {
   scenario<-"Opti-2_HCR_4_stoch_penBlim_atAgeW__"; 
   output.dir<-data.path 
   op.dir<-file.path(data.path,scenario)
} else if (ForecastModel==T ) {
   output.dir<-scenario.dir 
   op.dir<-scenario.dir
} 


##########################################################################


my.dev<-'png'   # output device:  'screen', 'wmf', 'png', 'pdf'

#my.dev<-'screen'
cleanup()
file.name<-'summary'


nox<-3; noy<-1;
noxy<-nox*noy

ref<-Read.reference.points()

Init.function()

pel.bio<<-NULL
dem.bio<-NULL
deadM<-NULL
deadM1<-NULL
deadM2<-NULL
CWsum<-NULL
Expectancy<-NULL
LFI<-NULL


if (HindcastModel) {
  dat1<-Read.summary.data(dir=data.path)
   
  dat1<-subset(dat1,Year<=last.year )
  if (first.year>0) dat1<-subset(dat1,Year>=first.year )
  bio<-subset(dat1,select=c(Species.n,Year,Age,BIO),Quarter==1)
  
  round(tapply(bio$BIO,list(bio$Year,bio$Species.n),sum)/1000)
  
  MSFD<-read.FLOP.MSFD.control(file=file.path(op.dir,"op_msfd.dat"),n.VPA=nsp-first.VPA+1,n.other.pred=first.VPA-1)
  
  pl<-strmacro(opt,out,n1=1,
       expr={
        incl<-NULL
        for (s in (n1:nsp)) { 
          fa.i<-MSFD@opt['first.age',s]
          la.i<-MSFD@opt['last.age',s] 
          if (la.i>=fa.i) incl<-rbind(incl,data.frame(Species.n=s,Age=seq(fa.i,la.i)))
        }
        out<-merge(incl,bio)
        out<-tapply(out$BIO,list(out$Year),sum)
      } )
    
  pl(community.biomass.pelagic.ages,pel.bio)    # creates pel.bio
  pl(community.biomass.demersal.ages,dem.bio)   # creates dem.bio
  pl(community.biomass.forage.ages,forage.bio)   # creates forage.bio
  
  bio2<-subset(bio,Species.n>=first.VPA)
  tot.bio<-tapply(bio2$BIO,list(bio2$Year),sum)
  

  #community M and F
  bio<-subset(dat1,select=c(Species.n,Year,Quarter,Age,BIO,M,M1,M2,CWsum,west, N.bar ))
  incl<-data.frame(Species.n=first.VPA:nsp,incl=c(MSFD@community.M.sp))
  a<-merge(incl,bio)
  a<-subset(a,incl==1)
  a[a$M1==-1,'M1']<-a[a$M1==-1,'M']
  a$deadM1<-a$N.bar*a$west*a$M1
  a$deadM2<-a$N.bar*a$west*a$M2
  bio2<-subset(a,Quarter==1); 
  bio2<-tapply(bio2$BIO,list(bio2$Year),sum) 
  deadM1<-tapply(a$deadM1,list(a$Year),sum)/bio2
  deadM2<-tapply(a$deadM2,list(a$Year),sum)/bio2

  incl<-data.frame(Species.n=first.VPA:nsp,incl=c(MSFD@community.F.sp))
  a<-merge(incl,bio)
  a<-subset(a,incl==1)
  bio2<-subset(a,Quarter==1); 
  bio2<-tapply(bio2$BIO,list(bio2$Year),sum) 
  CWsum<-tapply(a$CWsum,list(a$Year),sum)/bio2
 

  # life expectancy
  bio<-subset(dat1,select=c(Species.n,Year,Quarter,Age,Z))
  incl<-data.frame(Species.n=first.VPA:nsp,incl=c(MSFD@community.life.expectancy.options['first.age',]))
  incl<-NULL
  for (s in (first.VPA:nsp)) { 
          fa.i<-MSFD@community.life.expectancy.options['first.age',s-first.VPA+1]
          la.i<-SMS.control@species.info[s,'last-age']
          if (la.i>=fa.i) incl<-rbind(incl,data.frame(Species.n=s,Age=seq(fa.i,la.i)))
   }
  bio<-merge(bio,incl)
  a<-data.frame(Species.n=first.VPA:nsp,w=c(MSFD@community.life.expectancy.options['weighting',]))
  bio<-merge(bio,a)
   
  bio<-bio[order(bio$Species.n,bio$Year,bio$Age,bio$Quarter),]
  min.Year<-min(bio$Year)
  bio$first<- !duplicated(paste(bio$Species.n,bio$Year))
  
  a<-unique(subset(bio,first,select=c(Species.n,Age,Quarter)))
  a$ini.age<-a$Age+(a$Quarter-1)*0.25
  a<-subset(a,select=c(Species.n,ini.age))
  
  bio<-merge(bio,a)
  bio$sumZ<-bio$Z
  bio<-bio[order(bio$Species.n,bio$Year,bio$Age,bio$Quarter),]
  
  
  for (x in (1:dim(bio)[1])) {
   if (!bio[x,'first']) bio[x,'sumZ']<-bio[x,'sumZ']+(bio[x-1,'sumZ'])
  }
  bio$p<-exp(-bio$sumZ)
  
  a<-aggregate(p~Year+Species.n+ini.age+w,sum,na.rm=T,data=bio)
  a$p<-a$p/4 + a$ini.age+0.25/2  # to expected life in years (from quarters)
  a$pw<-a$p*a$w
  
  a<-aggregate(cbind(pw, w)~Year,sum,na.rm=T,data=a)
  a$Expectancy<-a$pw/a$w
  Expectancy<-tapply(a$Expectancy,list(a$Year),sum)
  
  # LFI
  bio<-subset(dat1,Quarter==1,select=c(Species.n,Year,Age,BIO))
  incl<-data.frame(Species.n=1:nsp,incl=MSFD@LFI.sp['include',])
  bio<-merge(bio,incl)
  bio<-subset(bio,incl==1,select=-incl)
  NW.all<-tapply(bio$BIO,list(bio$Year),sum)
  
  incl<-NULL
  for (s in (1:nsp)) { 
          fa.i<-MSFD@LFI.age['first.age',s]
          la.i<-SMS.control@species.info[s,'last-age']
          if (la.i>=fa.i) incl<-rbind(incl,data.frame(Species.n=s,Age=seq(fa.i,la.i)))
   }
  bio<-merge(bio,incl)
  NW.LFI<-tapply(bio$BIO,list(bio$Year),sum)
  
  LFI<-NW.LFI/NW.all

}


if (ForecastModel) {
 dat<-Read.OP.community.indicator(dir=op.dir)
} else  dat<-NULL

add.set<-strmacro(in1,in2,out,
   expr={
    if (HindcastModel & ForecastModel) out<-c(in1,in2) else if (HindcastModel) out<-c(in1)  else if (ForecastModel) out<-c(in2)
  } )

# biomass plot
b.factor<-1000
if (ForecastModel) {
   pel.b<-tapply(dat$bio.pelag,list(dat$Year),mean)
   dem.b<-tapply(dat$bio.demer,list(dat$Year),mean)
}
add.set(in1=pel.bio,in2=pel.b,pel.b)
add.set(in1=dem.bio,in2=dem.b,dem.b)
add.set(in1=forage.bio,in2=forage.b,forage.b)
add.set(in1=tot.bio,in2=tot.b,tot.b)
tot.b<-tot.b/b.factor
pel.b<-pel.b/b.factor
dem.b<-dem.b/b.factor
forage.b<-forage.b/b.factor
max.bio<-max(c(pel.b,dem.b,forage.b))
min.bio<-min(c(pel.b,dem.b,forage.b))
ratio<-pel.b/dem.b
year<-as.numeric(unlist(names(ratio)))



 
cleanup()
plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=8, height=12, pointsize=12)
  if (dev=='wmf') win.metafile(filename = paste(out,'.wmf',sep=''), width=8, height=10, pointsize=12)
  if (dev=='png') png(filename =paste(out,'.png',sep=''), width = 1000, height = 1400,units = "px", pointsize = 30, bg = "white")
  if (dev=='pdf') pdf(file =paste(out,'.pdf',sep=''), width = 8, height = 10,pointsize = 12,onefile=FALSE)
}


plotfile(dev=my.dev,out=file.path(op.dir,'community_indicator_year'));
par(mfcol=c(nox,noy))
par(mar=c(3,4,3,2))  # bottom, left, top, right
par(mar=c(2,4,3,5)+.1)  #bottom, left, top, right

plot(year,dem.b, ylim=c(min.bio,max.bio),type='b',xlab='Year',ylab='1000 t',lty=1,pch='d',lwd=2,main='Biomass: Pelagic (p), Demersal (d) and p:d ratio(r)')
lines(year,pel.b,lty=2,pch='p',lwd=2,type='b',col=2)
#lines(year,forage.b,lty=2,pch='f',lwd=2,type='b',col=3)


#legend('topright',
#     c('Pelagic','Demersal','Ratio'),
#     pch="pdr",lty=c(0,0,0),col=c(1,2,3),lwd=rep(2,3))
     
par(new=T)
plot(year,ratio,lty=3,pch='r',lwd=2,ylab=' ',xlab=' ',type='b',axes=F,col=4)  
axis(side=4)
mtext(side=4,line=3.0,"Ratio Pelagic:Demersal",cex=0.65)
par(xaxs="r")

# community F and M2


if (ForecastModel) {
  FF<-tapply(dat$comm.Fall,list(dat$Year),mean)
  M<-tapply(dat$comm.M,list(dat$Year),mean)
  M2<-tapply(dat$comm.M2,list(dat$Year),mean)
}
add.set(in1=CWsum,in2=FF,FF)
add.set(in1=deadM1,in2=M,M1)
add.set(in1=deadM2,in2=M2,M2)


max.D<-max(c(FF,M1,M2))
min.D<-min(c(FF,M1,M2))
year<-as.numeric(unlist(names(M1)))



plot(year,FF,ylim=c(min.D,max.D),lty=1,pch='F',lwd=2,ylab='',xlab=' ',type='b',axes=T,col=1,main='Removed biomass relative to TSB 1. January')  

legend('topright',
     c('Fishing','M2 predation','M1 residual mort.'),
     pch="F21",lty=c(0,0,0),col=c(1,2,4),lwd=rep(2,3))

lines(year,M1,lty=2,pch='1',lwd=2,type='b',col=4)
lines(year,M2,lty=3,pch='2',lwd=2,type='b',col=2)




#  community.life.expect
if (ForecastModel) { 
  ex<-tapply(dat$community.life.expect,list(dat$Year),mean)
}
add.set(in1=Expectancy,in2=ex,ex)
year<-as.numeric(unlist(names(ex)))
plot(year,ex,type='b',xlab=' ',pch='e',ylab='Life expectancy (year)',lty=1,lwd=2,main='Life expectancy (e) and LFI (l)')


#  LFI
if (ForecastModel) {
  LFI2<-tapply(dat$LFI,list(dat$Year),mean)
}
add.set(in1=LFI,in2=LFI2,LFI)
LFI<-LFI*100
year<-as.numeric(unlist(names(LFI)))

     
par(new=T)
plot(year,LFI,lty=3,pch='l',lwd=2,ylab=' ',xlab=' ',type='b',axes=F,col=4)  
axis(side=4)
mtext(side=4,line=3.0,"LFI (%)",cex=0.65)
par(xaxs="r")


if (my.dev %in% c('png','wmf','pdf')) dev.off()  
