
##########################################################################
plot.OP.HCR<-function(file.name="HCR-OP", HCR=1,my.device='png',first.year.on.plot=2012,last.year.on.plot=2050,scenario.dir=data.path,tit='',used.penalty='no',portrait=TRUE) {
  incl.yield<-T
  incl.mean.values<-F
  cat('plotting results in dir: ',scenario.dir,'\n')
  file.name<-paste(file.name,ifelse((portrait),'P','L'),sep='_')
  plotfile<-function(dev='screen',out) {
    cleanup()
    if (portrait) {
      if (dev=='screen') X11(width=8, height=8, pointsize=12)
      if (dev=='wmf') win.metafile(filename = file.path(scenario.dir,paste(out,'.wmf',sep='')), width=8, height=8, pointsize=12)
      if (dev=='png') png(filename =file.path(scenario.dir,paste(out,'.png',sep='')), width = 1200, height = 1600,units = "px", pointsize = 25, bg="white")
      if (dev=='pdf') pdf(file =file.path(scenario.dir,paste(out,'.pdf',sep='')),width = 12, height = 16,pointsize = 12)
      par(mfcol=c(5,2))
      par(mar=c(4,4,4,6))  # c(bottom, left, top, right)
      par(oma=c(0,0,3,0))
    }
    if (!portrait) {
      if (dev=='screen') X11(width=8, height=8, pointsize=12)
      if (dev=='wmf') win.metafile(filename = file.path(scenario.dir,paste(out,'.wmf',sep='')), width=8, height=8, pointsize=12)
      if (dev=='png') png(filename =file.path(scenario.dir,paste(out,'.png',sep='')), width = 1700, height = 1200,units = "px", pointsize = 25, bg="white")
      if (dev=='pdf') pdf(file =file.path(scenario.dir,paste(out,'.pdf',sep='')),width = 18, height = 12,pointsize = 12)
      par(mfcol=c(3,3))
      par(mar=c(4,4,4,6))  # c(bottom, left, top, right)
      par(oma=c(0,0,3,0))
    }
  }
  
  a<-Read.OP.par(dir=scenario.dir)
  b<-Read.OP.condensed(dir=scenario.dir)
  b<-subset(b,Year>=first.year.on.plot & Year<=last.year.on.plot)
  plot.no<-0
  
  ref<-Read.reference.points() 
  Blim<-as.vector(ref[,'Blim'])
  Bpa<-as.vector(ref[,'Bpa'])
  
  plotfile(dev=my.device,out=file.name);
  #for (sp in (first.VPA:nsp)){
  for (sp in (first.VPA:26)){
    sp.name<-sp.names[sp]
  
    bb<-droplevels(subset(b,Species.n==sp))
    bb$SSB<-bb$SSB/1000
    bb$TSB<-bb$TSB/1000
    bb$yield<-bb$yield/1000
    aa<-subset(a,Species.n==sp)
    
    if (aa[1,'HCR']==22) bb$SSB<-bb$TSB
    maxSSB<-max(bb$SSB)
    maxF<-max(bb$Fbar)
    T1<-aa[1,'T1']/1000
    T2<-aa[1,'T2']/1000
    Ftarget<-aa[1,'Ftarget']

    if (HCR != 1) stat<-paste("median F=",round(median(bb$Fbar),2)) else stat<-'Median'
    stat<-paste(stat," Yield:",round(median(bb$yield),0),"kt ",ifelse(aa[1,'HCR']==22,'TSB:','SSB:'),round(median(bb$SSB),0),'kt\n')
    if (incl.mean.values)    stat<-paste(stat,"mean F=",round(mean(bb$Fbar),2)," Yield:",round(mean(bb$yield),0),"kt SSB:",round(mean(bb$SSB),0),'kt')
    stat<-paste(stat,'Target F:',formatC(Ftarget,format='f',digits=2),sep='')
    if (F) {  
      if (plot.no%%noxy==0){
        newplot(dev,nox,noy,filename=paste(file.name,'_',sp.name,sep=''),Portrait=Portrait)
        par(mar=c(4,4,4,6))  # c(bottom, left, top, right)
        plot.no<-0
      }
    }
    if (aa[1,'HCR']==22)  x.lab<-"TSB (1000 t)" else  x.lab<-"SSB (1000 t)"
    dim(bb)[[1]]
    plot(bb$SSB,bb$Fbar,type='p',cex=2,xlab=x.lab,ylab="Fbar",xlim=c(0,max(maxSSB,c(T2*1.1))),ylim=c(0,Ftarget*1.2),main=list(paste(sp.name,': ',stat,sep=''),cex=1.25))
    points(bb[dim(bb)[[1]],'SSB'],bb[dim(bb)[[1]],'Fbar'],type='p',cex=3,pch=19,col='black')
    #if (aa[1,'penaltyUse']==1) abline(v=aa[1,'penaltySSB']/1000,col='green',lty=2)
    if (aa[1,'penaltyUse']==1) {
      if (used.penalty=='Blim')  abline(v=Blim[sp]/1000,col='green',lty=2)
      if (used.penalty=='Bpa')  abline(v=Bpa[sp]/1000,col='green',lty=2)
    }
    if (sp==first.VPA) {
       mtext(tit,line=0, adj=0.5, cex=1.0, col="red", outer=TRUE)
    }
    plot.no<-plot.no+1
   
    # 1: Target F
    #      F=Ftarget
    # 2: Target F, and known trigger T1 and T2
    #      F=0                        for     SSB<T1
    #      F=Ftarget*(SSB-T1)/(T2-T1) for  T1<SSB<T2
    #      F=Ftarget                  for  T2<SSB
    # 3: Target F and Fslope, and known trigger T1 and T2
    #      F=0                          for     SSB<T1
    #      F=Ftarget*(SSB-T1)/(T2-T1)   for  T1<SSB<T2
    #      F=Ftarget+(SSB-T2)/T2*Fslope for  T2<SSB
    #  4: Logistic curve
    #      F=Ftarget / (1+exp(S1 - S1/SSB50%*SSB)
    #          Ftarget, S1 and SSB50% (SSB for which F is 50% of Ftarget)
  
    if (aa[1,'HCR']==1) { #constant F
      abline(h=aa[1,"Ftarget"],lwd=2,col='red')
  
    }
    else if (aa[1,'HCR']==2 | aa[1,'HCR']==22) {
      x<-c(0,aa[1,'T1']/1000)
      y<-c(0,0)
      lines(x,y,col='red',lwd=2)
      x<-c(aa[1,'T1']/1000,aa[1,'T2']/1000)
      y<-c(0,aa[1,'Ftarget'])
      lines(x,y,col='red',lwd=2)
      x<-c(aa[1,'T2']/1000,max(maxSSB,aa[1,'T2']/1000*1.1))
      y<-c(aa[1,'Ftarget'],aa[1,'Ftarget'])
      lines(x,y,col='red',lwd=2)
      abline(v=T1,lty=2)
      abline(v=T2,lty=2)
    }
    else if (aa[1,'HCR']==3) {
      x<-c(0,aa[1,'T1']/1000)
      y<-c(0,0)
      lines(x,y,col='red',lwd=2)
      x<-c(aa[1,'T1']/1000,aa[1,'T2']/1000)
      y<-c(0,aa[1,'Ftarget'])
      lines(x,y,col='red',lwd=2)
      x<-c(aa[1,'T2']/1000,maxSSB)
      y<-c(aa[1,'Ftarget'],aa[1,'Ftarget']+((maxSSB*1000-aa[1,'T2'])/aa[1,'T2']*aa[1,'Fslope']))
      lines(x,y,col='red',lwd=2)
      abline(v=T1,lty=2)
      abline(v=T2,lty=2)
    }
    else if (aa[1,'HCR']==4) {
      x<-seq(0,maxSSB,1)
      y<-aa[1,'Ftarget'] / (1+exp(aa[1,'S1'] - aa[1,'S1']/aa[1,'SSB50']*x))
      lines(x,y,col='red',lwd=2)
      abline(v=aa[1,'SSB50'],lty=2)
    }
    
    
    if (incl.yield) {
      par(new=T)
  
      plot( bb$SSB,bb$yield,axes=F,xlab=x.lab, ylab=' ',col='blue',pch=2,ylim=c(0,max(bb$yield)),xlim=c(0,max(maxSSB,c(T2*1.1))),type='p')
        points(bb[dim(bb)[[1]],'SSB'],bb[dim(bb)[[1]],'yield'],type='p',cex=3,pch=17,col='blue')
  
      axis(side=4)
      mtext(side=4,line=3.0,"Yield (1000 t)",cex=0.75,col='blue')
      par(xaxs="r")
  
    }
  
  }
  if (my.device %in% c('png','wmf','pdf')) dev.off()  
}
