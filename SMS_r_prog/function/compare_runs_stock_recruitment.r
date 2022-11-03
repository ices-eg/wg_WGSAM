compare_runs_stock_rec<-function(dirs,labels,first.year.on.plot=1975,last.year.on.plot=2020,incl.sp="all",
                                 include.CV=TRUE,include.CV2=TRUE,include.mean=TRUE,
                                 palette="R3", makeAllGraphs=FALSE,nox=2, noy=2, w8=8,w11=11,
                                 include.year.labels=TRUE,incl_not_used=TRUE,run.ID='SBB_rec', 
                                 paper=FALSE,facSSB=1000,facRec=1000000,
                                 compare.dir=data.path) {
                                    
   if (paper) dev<-"png" else dev<-"screen"
    
   palette(palette)
   for (dir in dirs) {
     if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
   } 
   
   Init.function() # get SMS.contol object  including sp.names
   
   SSB.R.year.first<-SMS.control@SSB.R.year.first
   SSB.R.year.last <-SMS.control@SSB.R.year.last
   SSB.R.year.first[SSB.R.year.first==-1]<-SMS.control@first.year.model
   SSB.R.year.last[SSB.R.year.last==-1]<-SMS.control@last.year.model
   
   SSB.R.year<-rbind(SSB.R.year.first,SSB.R.year.last)
   colnames(SSB.R.year)<-sp.names[first.VPA:nsp]
   
   
   model.name<-c('Ricker','Bev. & Holt','Geom. mean','Hockey stick')
   
   
   for (dir in dirs) {
      a<-Read.summary.table(dir=file.path(root,dir),read.init.function=T)
      a$label<-labels[ which(dirs==dir)]
      a<-subset(a,Year>=first.year.on.plot & Year<=last.year.on.plot,select=c(label,Year, Species, Species.n, SSB,Rec))
      

      
      # read recruitment years used
      b<-read_recruit_years(dir=file.path(root,dir),read.init.function=TRUE) 
      a<-merge(a,b,all.x=TRUE)    
      
      # read SSB/Rec model
      p<-Read.SSB.Rec.data(dir=file.path(root,dir))
      a<-merge(a,p,all.x=TRUE)
      
      if (dir==dirs[1]) all<-a else all<-rbind(all,a)
   }
   
   all$SSB<-all$SSB/facSSB
   all$Rec<-all$Rec/facRec
   
   SSB_R<-function(SSB,model,alfa,beta,delta=0.5,info1,info2,std) {
      x<-SSB
      y<-SSB  # copy structure
      #print(c(alfa,beta,exp(alfa+beta)))
      if (model==1) y<-alfa*x*exp(-beta*x)
      else if (model==51 | model==52) y<-alfa*x*exp(-beta*x+info1*info2)
      else if (model==2) y<-alfa*x/(1+x*beta)
      else if (model==3) y<-rep(exp(alfa),length(x))
      else if (model==4) {for (ii in (1:length(x))) y[i]<-exp(alfa)*min(x[i],exp(beta))}
      else if (model==5) {for (ii in (1:length(x))) {
         if (x[ii]<=(exp(beta)*(1-delta))) y[ii]<-exp(alfa)*x[ii]
         else if (x[ii]< exp(beta)*(1+delta)) y[ii]=exp(alfa)*(x[ii]-(((x[ii]-exp(beta)*(1+delta))^2)/(4*delta*exp(beta))))
         else if (x[ii]>=exp(beta)*(1+delta)) y[ii]<=exp(alfa+beta)
         else x[i]<-NA
      }}
      else if (model==100) {for (ii in (1:length(x))) y[ii]<-exp(alfa)*min(x[ii],beta)}
      return(y)
   }
   
   
   if (paper) dev<-"png" else dev<-"screen"
   if (incl.sp[1]=="all") sp.plot<-unique(all$Species) else sp.plot<-incl.sp
   
   if (makeAllGraphs) dev<-'png'
   
   
   len.dir<-length(dirs)
   
   plotvar<-function(sp,dat,plotType='p',addModel=FALSE) {
        if ((gi %% (nox*noy))==0  | gi==0) {
         
         filename<-paste0("compare_",run.ID,'_',sp)
         if (makeAllGraphs) filename=file.path(compare.dir,filename)
         
         newplot(dev,nox,noy,filename=filename,Portrait=F,w8=w8,w11=w11);
           par(mar=c(4,5,3,1))   # c(bottom, left, top, right)
      }
      
      for (l in labels ) {
         xx<-subset(dat,label==l)
         xx$incl<-xx$Year>=SSB.R.year[1,sp] & xx$Year<=SSB.R.year[2,sp] & xx$used
         if (!incl_not_used) xx<-subset(xx,incl)
         plot(xx$SSB,xx$Rec,xlab="SSB (kt)",ylab="Recruits (billions)",main=paste(l,sp), lwd=2,xlim=c(0,max(xx$SSB,na.rm=TRUE)), ylim=c(0,max(xx$Rec,na.rm=TRUE)*1.1),type=plotType )
         if ((noxy <5) && (include.year.labels)) text(x=xx$SSB,y=xx$Rec,labels=as.character(sprintf("%02.0f",xx$Year%%100)),pos=3)
         if (incl_not_used) {
            xx2<-subset(xx,incl==FALSE)
            points(xx2$SSB,xx2$Rec,col='red',lwd=2 )
            if ((noxy <5) && (include.year.labels)) text(x=xx2$SSB,y=xx2$Rec,labels=as.character(sprintf("%02.0f",xx2$Year%%100)),pos=3,col='red')
         }
         
         # SSB/R regression line
    
         x<-seq(0,max(xx$SSB), by=max(xx$SSB)/100)*facSSB
         y<-SSB_R(SSB=x,      
                  model=xx[1,'model'],
                  alfa=xx[1,'alfa'],
                  beta=xx[1,'beta'], 
                  info1=xx[1,'info1'],
                  info2=xx[1,'info2']) 
   
         x<-x/facSSB
         y<-y/facRec
         CV<- sqrt(exp(xx[1,'std']^2) - 1)
         smodel<-xx[1,'model']
         beta<-xx[1,'beta']
         lines(x,y,col=2)
         if (include.CV) {
            lines(x,y*exp(CV),col=4)
            lines(x,y*exp(-CV),col=4)
            if (include.CV2==1) {
               lines(x,y*exp(2*CV),col=5)
               lines(x,y*exp(-2*CV),col=5)
            }
         }
         if (include.mean) {
            lines(x,y*exp((CV^2)/2),col=6,lty=2)
         }
         if (smodel==100) abline(v=beta/facSSB,lty=2)
         
         
      }
   
      gi<<-gi+1
    }
   
   if (incl.sp=="all") sp.plot<-sp.names[first.VPA:nsp] else sp.plot<-incl.sp
   
   for (sp in (sp.plot)) {
      gi<-0
      plotvar(sp=sp,dat=subset(all,Species==sp),addModel=TRUE)
      if (dev %in% c('png','print')) cleanup()
   }
   
   if (dev=='png') cleanup()
}


