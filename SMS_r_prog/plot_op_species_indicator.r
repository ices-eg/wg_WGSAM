

Portrait<-F                 # graphical output orientation

first.year<- 1963                #first year on plot, negative value means value defined by data
last.year<- 2100               #last year on plot


HindcastModel<-T; # plot values from the hindcast
ForecastModel<-F; # plot values from the scenarios

redefine.scenario.manually<-F  # Define the scenario dir used explecitely in this script or do it externally

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


MSFD<-read.FLOP.MSFD.control(file=file.path(op.dir,"op_msfd.dat"),n.VPA=nsp-first.VPA+1,n.other.pred=first.VPA-1)

if (HindcastModel) {
  dat1<-subset(Read.summary.data(dir=data.path),Year<=last.year &Species.n>=first.VPA)
  if (first.year>0) dat1<-subset(dat1,Year>=first.year  )
  
  # mean weight in the sea and in tha catch
  w<-subset(dat1,select=c(Species.n,Year,Quarter,Age,weca,west, N,C.obs ))
  incl<-data.frame(Species.n=first.VPA:nsp,incl=c(MSFD@mean.weight.C.sp))
  w<-subset(merge(incl,w),incl==1) 
  w[w$weca<-0,'weca']<-0
  w$N1<-w$N
  w[w$Age==0,'N1']<-0
  w$WC<-w$C.obs*w$weca
  w$WN<-w$N*w$west
  w$WN1<-w$N1*w$west
    
  w<-aggregate(cbind(WC,WN,WN1,N,N1,C.obs)~Species.n+Year,data=w,sum,na.rm=T)
  w$west<-w$WN/w$N
  w$west1<-w$WN1/w$N1
  w$weca<-w$WC/w$C.obs
  w<-subset(w,select=c( Species.n, Year, west,west1,weca))

  #Species average M2  and F
  mf<-subset(dat1,select=c(Species.n,Year,Quarter,Age, N.bar,M2,C.hat ))
  mf$deadM2<-mf$N.bar*mf$M2
  inclM<-data.frame(Species.n=first.VPA:nsp,faM=c(MSFD@M2.bar.ages[1,]),laM=c(MSFD@M2.bar.ages[2,]) )
  inclF<-data.frame(Species.n=first.VPA:nsp,faC=c(SMS.control@avg.F.ages[,1]),laC=c(SMS.control@avg.F.ages[,2]) )
  incl<-merge(inclM,inclF)
  mf<-merge(incl,mf)
  mf$inclF<-F
  mf$inclM<-F
  mf[mf$faM>=0 & mf$laM>=0 & mf$Age>=mf$faM & mf$Age<=mf$laM,'inclM']<-T
  mf[mf$faC>=0 & mf$laC>=0 & mf$Age>=mf$faC & mf$Age<=mf$laC,'inclF']<-T
  
  m<-droplevels(subset(mf,inclM==T))
  m$deadM2<-m$N.bar*m$M2
  m<-aggregate(deadM2~Species.n+Year+Age,sum,data=m)
  mn<-subset(dat1,(Age>0 & Quarter==1) | (Age==0 & Quarter==3),select=c(Species.n,Year,Age,N))
  mz<-aggregate(Z~Species.n+Year+Age,sum,data=dat1)
  mm<-merge(mn,mz)
  mm$N.bar<-mm$N*(1-exp(-mm$Z))/mm$Z
  m<-merge(mm,m)
  m$M2<- m$deadM2/m$N.bar
  m<-aggregate(M2~Species.n+Year,mean,data=m)
   
  f<-droplevels(subset(mf,inclF==T))
  f<-aggregate(C.hat~Species.n+Year+Age,sum,data=f)
  fn<-subset(dat1,(Age>0 & Quarter==1) | (Age==0 & Quarter==3),select=c(Species.n,Year,Age,N))
  fz<-aggregate(Z~Species.n+Year+Age,sum,data=dat1)
  ff<-merge(fn,fz)
  ff$N.bar<-ff$N*(1-exp(-ff$Z))/ff$Z
  f<-merge(ff,f)
  f$F<- f$C.hat/f$N.bar
  f<-aggregate(F~Species.n+Year,mean,data=f)
 
 fm<-merge(f,m,all=T)
 
  life.expectancy<-function(fa,la) {
    # life expectancy
    bio<-subset(dat1,select=c(Species.n,Year,Quarter,Age,Z))
    incl<-data.frame(Species.n=first.VPA:nsp,incl=c(MSFD@community.life.expectancy.options['first.age',]))
    incl<-NULL
    for (s in (first.VPA:nsp)) { 
            fa.i<-fa[s-first.VPA+1]
            la.i<-la[s]
            if (la.i>=fa.i) incl<-rbind(incl,data.frame(Species.n=s,Age=seq(fa.i,la.i)))
     }
    bio<-merge(bio,incl)
  #  a<-data.frame(Species.n=first.VPA:nsp,w=c(MSFD@community.life.expectancy.options['weighting',]))
   # bio<-merge(bio,a)
     
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
    
    a<-aggregate(p~Year+Species.n+ini.age,sum,na.rm=T,data=bio)
    a$Expectancy<-a$p/4 + a$ini.age+0.25/2  # to expected life in years (from quarters)
    a$p<-NULL
    a$ini.age<-NULL
    return(a)
  }
  a<-life.expectancy(fa=MSFD@community.life.expectancy.options['first.age',],la=SMS.control@species.info[,'last-age'])
  aa<-MSFD@community.life.expectancy.options['first.age',]
  aa[]<-1
  a1<-life.expectancy(fa=aa,la=SMS.control@species.info[,'last-age'])
  a1$Expectancy1<-a1$Expectancy
  a1$Expectancy<-NULL
  a<-merge(a,a1)
}


add.set<-strmacro(in1,in2,out,
   expr={
    if (HindcastModel & ForecastModel) out<-c(in1,in2) else if (HindcastModel) out<-c(in1)  else if (ForecastModel) out<-c(in2)
  } )

 ##  HER SKAL HISTORISKE DATA SAMLES MED SCENARIO DATA


a<-merge(a,w,all = T)
a<-merge(a,fm,all=T)
# a includes all data

a$Species<-sp.names[a$Species.n]

my.sp<-sort(unique(a$Species.n))

cleanup()
plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=8, height=12, pointsize=12)
  if (dev=='wmf') win.metafile(filename = paste(out,'.wmf',sep=''), width=8, height=10, pointsize=12)
  if (dev=='png') png(filename =paste(out,'.png',sep=''), width = 1000, height = 1400,units = "px", pointsize = 30, bg = "white")
  if (dev=='pdf') pdf(file =paste(out,'.pdf',sep=''), width = 8, height = 10,pointsize = 12,onefile=FALSE)
}
 
  
for (sp in my.sp) {
  #sp<-18
  Species<-sp.names[sp] 
  b<-droplevels(subset(a,Species.n==sp))
   
  plotfile(dev=my.dev,out=file.path(op.dir,paste('Species_indicator',sp,sep='-')));
  par(mfcol=c(nox,noy))
  par(mar=c(3,4,3,2))  # bottom, left, top, right
  par(mar=c(2,4,3,5)+.1)  #bottom, left, top, right
  

  # F and M2  
  min.d<-min(min(b$M2,na.rm=T),min(b$F,na.rm=T),na.rm=T)
  max.d<-max(max(b$M2,na.rm=T),max(b$F,na.rm=T),na.rm=T)
  
   plot(b$Year,b$F, ylim=c(min.d,max.d),type='b',xlab='Year',ylab='',lty=1,pch='F',lwd=2,main=paste(Species,' Mean F and M2',sep=':'))
  lines(b$Year,b$M2,lty=2,pch='M',lwd=2,type='b',col=2)
 
   
   #  life.expect
     min.d<-min(min(b$Expectancy,na.rm=T),min(b$Expectancy1,na.rm=T),na.rm=T)
  max.d<-max(max(b$Expectancy,na.rm=T),max(b$Expectancy1,na.rm=T),na.rm=T)

  plot(b$Year,b$Expectancy,lty=1,pch='0',lwd=2,ylab='year',xlab=' ',type='b',col=1,ylim=c(min.d,max.d),main="Life expectancy for age 0 (third quarter) and age 1")  
  lines(b$Year,b$Expectancy1,lty=2,pch='1',lwd=2,type='b',col=2)  

  
    
  
  min.d<-min(min(b$west,na.rm=T),min(b$weca,na.rm=T),na.rm=T)
  max.d<-max(max(b$west,na.rm=T),max(b$weca,na.rm=T),na.rm=T)
  
  plot(b$Year,b$weca, ylim=c(min.d,max.d),type='b',xlab='Year',col='blue',ylab='kg',lty=1,pch='c',lwd=2,main=paste('Mean weight in the sea (for age>=0 and age>=1) and in the catch (c)',sep=':'))
  lines(b$Year,b$west,lty=2,pch='0',lwd=2,type='b',col=1)
   lines(b$Year,b$west1,lty=2,pch='1',lwd=2,type='b',col=2)
      
 
  
  if (my.dev %in% c('png','wmf','pdf')) dev.off()  
}

