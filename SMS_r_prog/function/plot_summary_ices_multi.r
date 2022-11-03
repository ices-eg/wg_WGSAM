plot_summary_ices_multi<-function(
  Portrait=T,                 # graphical output orientation
  include.terminal.year= FALSE,          # plot terminal year (last assessment year +1) as well?
  include.last.assess.year.recruit=FALSE,          # plot recruits terminal year as well?
   
  first.year= -1974,                #first year on plot, negative value means value defined by data
  last.year= 2300,             #last year on plot
  incl.M2.plot=TRUE,
  incl.reference.points=TRUE,
  incl.TSB=FALSE,
  splitLine=FALSE,
  OperatingModel=FALSE,
  scenario.dir=data.path,
  redefine.scenario.manually=FALSE,
  output.dir=data.path,
  op.dir=data.path,
  my.dev=c('screen','wmf', 'png', 'pdf')[3])
 {

  cleanup()
  
  if (OperatingModel==T & redefine.scenario.manually==T)  {
     scenario<-"test"; 
     #op.dir<-file.path(data.path,"HCR_1_deter_adjust_test_01_HCR1_0_Rec0__2030")
     op.dir<-data.path
  } else if (OperatingModel==T & redefine.scenario.manually==FALSE) {
     op.dir<-scenario.dir
  } 
  

  ##########################################################################
  
  
 
  palette("default")
  #cleanup()
  file.name<-'plot_summary'
  
  
  #dev<-"dummy"
  if (incl.M2.plot) { nox<-3; noy<-2;} else { nox<-2; noy<-2;}
  noxy<-nox*noy
  
  ref<-Read.reference.points()
  
  Init.function()
  
  if (!OperatingModel) dat<-Read.summary.data(extend=include.terminal.year,read.init.function=F)
  if (OperatingModel) {
    dat1<-Read.summary.data(extend=F,read.init.function=F)
  
    dat<-Read.summary.data(dir=op.dir,infile="op_summary.out",read.init.function=F)
    
     # x<-subset(dat,Species.n==2  & Age>=3 & Age <=6) ;  tapply(x$F/4,list(x$Year),sum)
    
    if (SMS.control@no.areas >1) {
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
    dat1<-subset(dat1,select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,Yield.core,CWsum.core,BIO,SSB))
    dat <-subset(dat, select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,Yield.core,CWsum.core,BIO,SSB))
  
    dat<-rbind(dat1,dat)
  }
  #tapply(dat$Yield,list(dat$Year,dat$Species),sum)
  
  
  dat<-subset(dat,Year<=last.year )
  #(subset(dat,Year==2011 & Species=='Cod'))
  if (first.year>0) dat<-subset(dat,Year>=first.year )
  if (incl.M2.plot) {
    dat<-data.frame(dat,deadM1=dat$M1*dat$N.bar*dat$west,deadM2=dat$M2*dat$N.bar*dat$west,deadM=dat$M*dat$N.bar*dat$west)
   }
  
  if (FALSE) {
    a<-subset(dat,Species=='Sprat' & Age %in% c(1,2) & Year>2015)
    subset(a,Year==2016,select=c(Quarter,Age,F,M2,Z))
    aggregate(F/2~Year,sum,data=a) # mean F (sum of F)
  } 
  
    
  plotfile<-function(dev='screen',out) {
    if (dev=='screen') X11(width=11, height=8, pointsize=12)
    if (dev=='wmf') win.metafile(filename = file.path(output.dir,paste(out,'.wmf',sep='')), width=8, height=10, pointsize=12)
    if (dev=='png') png(filename =file.path(output.dir,paste(out,'.png',sep='')), width = 1400, height = 1000,units = "px", pointsize = 25, bg = "white")
    if (dev=='pdf') pdf(file =file.path(output.dir,paste(out,'.pdf',sep='')), width = 8, height = 10,pointsize = 12,onefile=FALSE)
  }
  
  for (sp in (first.VPA:nsp)){
      sp.name<-sp.names[sp]
      discard<-SMS.control@discard[sp-first.VPA+1]==1
      
      plotfile(dev=my.dev,out=paste(file.name,'_',sp.name,sep=''));
      par(mfcol=c(nox,noy))
  
      
      par(mar=c(3,4,3,2))
  
      core_area<-FALSE
      s<-subset(dat,Species.n==sp)
      av.F.age<-SMS.control@avg.F.ages[sp-first.VPA+1,]
      s1<-subset(s,s$Age>=av.F.age[1] & s$Age<=av.F.age[2])
      FI<-tapply(s1$F,list(s1$Year),sum)/(av.F.age[2]-av.F.age[1]+1)
      
      if (F) { 
        print(sp.name)
        print(max(FI[as.character(2050:2070)]))
      }
  
      s1<-subset(s,weca>=0 )

      Yield<-tapply(s1$Yield,list(s1$Year),sum)/1000
      SOP<-tapply(s1$CWsum,list(s1$Year),sum)/1000
      Yield.core<-tapply(s1$Yield.core,list(s1$Year),sum)/1000
      SOP.core<-tapply(s1$CWsum.core,list(s1$Year),sum)/1000
      if (discard)  catch<-rbind(SOP-Yield,Yield) else {
         catch<-rbind(Yield.core,Yield-Yield.core)
         if (sum(Yield-Yield.core) > 1) core_area<-TRUE
      }
      s1<-subset(s,Quarter==1)
      ssb<-tapply(s1$SSB,list(s1$Year),sum)/1000
      tsb<-tapply(s1$BIO,list(s1$Year),sum)/1000
      s2<-subset(s,Age==fa & Quarter==SMS.control@rec.season)
      rec<-tapply(s2$N,list(s2$Year),sum)/1000000
      year<-as.numeric(unlist(dimnames(ssb)))
      year.ssb<-year
      
      if(include.terminal.year){
       #Truncate the final year from key parameters
       year<- year[-length(year)]        
       FI <- FI[-length(FI)]
      } 
      if(!include.last.assess.year.recruit) rec <- rec[-length(rec)]
      
      bp<-barplot(catch,space=1,xlab='',ylab='1000 tonnes',main=paste(sp.name,ifelse(discard,',  Yield and discard', ifelse(core_area,' Catch (model area and other)',' Catch')),sep=''),ylim=c(0,max(SOP)))
      if (splitLine) {
         v<-which(names(catch)==as.character(SMS.control@last.year.model))
         abline(v = bp[v]+0.5,col="red",lty=2)
      } 
       
      #plot recruits
      #plot(year,rec,type='h',lwd=5,xlab='',ylab='billions',main=paste('Recruitment age',fa),ylim=c(0,max(rec)))
      
      bp<-barplot(rec,space=1,xlab='',ylab='billions',main=paste('Recruitment age',fa),ylim=c(0,max(rec)))
      if (splitLine) {
        v<-which(names(rec)==as.character(SMS.control@last.year.model))
        abline(v = bp[v]+0.5,col="red",lty=2)
      } 
      
      F.max<-max(FI,ref[sp,"Flim"])
  
       plot(year,FI,type='b',lwd=3,xlab='',ylab='',main="Fishing mortality",ylim=c(0,F.max))
      if (splitLine) abline(v=SMS.control@last.year.model+0.5,lty=2, col='red')
     # tmp<-paste('(',av.F.age[1],'-',av.F.age[2],')',sep='')
     # plot(year,FI,type='l',lwd=3,xlab='',ylab='',main=expression(bar(F)),ylim=c(0,F.max))
  
      if (incl.reference.points) if (ref[sp,"Flim"]>0) abline(h=ref[sp,"Flim"],lty=2,lwd=2)
      if (incl.reference.points) if (ref[sp,"Fpa"]>0) abline(h=ref[sp,"Fpa"],lty=3,lwd=2)
      grid()
      
      Blim<-ref[sp,"Blim"]/1000; Bpa<-ref[sp,"Bpa"]/1000
      SSB.max<-max(ssb,Bpa)
      if (incl.TSB) SSB.max<-max(tsb,SSB.max)
      plot(year.ssb,ssb,type='b',lwd=3,xlab='',ylab='1000 tonnes',main='SSB',ylim=c(0,SSB.max))
      if (incl.TSB)  lines(year.ssb,tsb,lwd=3,col='blue')
      if (incl.reference.points) if (Blim>0) abline(h=Blim,lty=2,lwd=2)
      if (incl.reference.points) if (Bpa>0) abline(h=Bpa,lty=3,lwd=2)
      if (splitLine) abline(v=SMS.control@last.year.model+0.5,lty=2, col='red')
     
      grid()
      if (incl.M2.plot) {
        deadM1<-tapply(s$deadM1,list(s$Year),sum)/1000
        deadM2<-tapply(s$deadM2,list(s$Year),sum)/1000
        deadM1<-deadM1[1:length(SOP)]
        deadM2<-deadM2[1:length(SOP)]
        bp<-barplot(rbind(SOP,deadM1,deadM2),space=1,main='Biomass removed\ndue to F, M1 and M2',ylab='1000 tonnes')
        if (splitLine) {
          v<-which(names(SOP)==as.character(SMS.control@last.year.model))
          abline(v = bp[v]+0.5,col="red",lty=2)
        } 
        
      
        b<-tapply(s$M2,list(s$Year,s$Age),sum)
        b<-b[1:length(SOP),]
        if (sum(b,na.rm=T)>=0.01) {
          y<-as.numeric(dimnames(b)[[1]])
          plot(y,b[,1],main=paste("M2 at age"),xlab="",ylab='M2',
                  type='l',lwd=1.5,ylim=c(0,max(b,na.rm=T)))
          for (a in (2:(dim(b)[2]))) if(max(b[,a],na.rm=T)>0.001) lines(y,b[,a],lty=a,col=a,lwd=2)
          if (splitLine) abline(v=SMS.control@last.year.model+0.5,lty=2, col='red')
        }
       }
       if (my.dev %in% c('png','wmf','pdf')) cleanup()
  }
}