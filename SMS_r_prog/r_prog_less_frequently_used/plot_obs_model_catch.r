paper<-FALSE                    # output on paper (TRUE) or screen (FALSE)
file.name<-'myfile'             # graphical output file if paper<-"Y"
Portrait<-FALSE                 # graphical output orientation

##########################################################################

#####  Annual
cleanup()
 
if (paper) dev<-"wmf" else dev<-"screen"
nox<-1; noy<-1;
nox.noy<-nox*noy
plot.no<-nox.noy-1

if (!paper) cleanup()
#newplot(dev,nox,noy,filename=file.name,Portrait=Portrait)
#par(mar=c(3,4,3,2)) 

s<-Read.summary.data()

c.hat<-tapply(s$C.hat*s$weca,list(s$Year,s$Species),sum)/1000
c.obs<-tapply(s$C.obs*s$weca,list(s$Year,s$Species),sum)/1000

for (sp in first.VPA:nsp) {
  plot.no<-plot.no+1
  if (plot.no%%nox.noy==0){
      newplot(dev,nox,noy,Portrait=Portrait)     
      par(mar=c(5,4,4,2)+0.1) 
  }
  ssp<-sp.names[sp]
  if (nsp>1) main<-ssp else main<-' '
  year<-as.numeric(names(c.hat[,ssp]))
  plot(year,c.obs[,ssp],type='b',lwd=2,xlab='',lty=1,ylab='1000 tonnes',main=main,
                 ylim=c(0,max(c.hat[,ssp],c.obs[,ssp])),pch="o")
  legend("topleft",c('Observed','Predicted'),pch="o*",lty=c(1,3),lwd=rep(2,2),col=c(1,2))

  lines(year,c.hat[,ssp],type='b',lwd=2,lty=3,pch="*",col=2)

}
if (paper) cleanup()




#### Seasonal

cleanup()
if (paper) dev<-"wmf" else dev<-"screen"
nox<-2; noy<-2;
nox.noy<-nox*noy
plot.no<-nox.noy-1

#newplot(dev,nox,noy,filename=file.name,Portrait=Portrait)
#par(mar=c(2,4,2,2))  #c(bottom, left, top, right)

s<-Read.summary.data()

c.hat<-tapply(s$C.hat*s$weca,list(s$Species,s$Year,s$Quarter),sum)/1000
c.obs<-tapply(s$C.obs*s$weca,list(s$Species,s$Year,s$Quarter),sum)/1000
ftable(round(c.hat/c.obs,1))

c.hat<-tapply(s$C.hat*s$weca,list(s$Year,s$Species,s$Quarter),sum)/1000
c.obs<-tapply(s$C.obs*s$weca,list(s$Year,s$Species,s$Quarter),sum)/1000

for (sp in first.VPA:nsp) for (q in (1:dim(c.hat)[3])) {
  plot.no<-plot.no+1
  if (plot.no%%nox.noy==0){
      newplot(dev,nox,noy,Portrait=Portrait)     
      par(mar=c(2,4,4,2)+0.1) 
  }
  ssp<-sp.names[sp]
  if (nsp>1) main<-paste(ssp,' Q',q,sep='') else main<-paste('Season',q)
  year<-as.numeric(names(c.hat[,ssp,q]))
  plot(year,c.obs[,ssp,q],type='b',lwd=2,xlab='',lty=1,ylab='1000 tonnes',main=main,
                 ylim=c(0,max(c.hat[,ssp,q],c.obs[,ssp,q])),pch="o")
  legend("topleft",c('Observed','Predicted'),pch="o*",lty=c(1,3),lwd=rep(2,2),col=c(1,2))

  lines(year,c.hat[,ssp,q],type='b',lwd=2,lty=3,pch="*",col=2)

}
if (paper) cleanup()
