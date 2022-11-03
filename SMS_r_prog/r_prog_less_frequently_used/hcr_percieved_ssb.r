cleanup()
nox<-1; noy<-1;  dev="screen"
cleanup()

a<-Read.SSB_percieved(dir=file.path(data.path,scenario))
i<-0


a<-subset(a,Year==2013)

X11()
 par(mfcol=c(2,1))
 
by(a,list(a$Year,a$Species.n),function(x) {
      if ((i %% (nox*noy))==0 & nsp>1) {
         newplot(dev,nox,noy,Portrait=TRUE);
         par(mar=c(3,5,3,2))
       }
       i<<-i+1
       mean_SSB<-mean(x$SSB_percieved)
       CV_SSB<-sd(x$SSB_percieved)/mean_SSB
       if (nsp==1) main<-paste("mean:",round(mean_SSB), "t CV:",formatC(CV_SSB,digits=2,format='f'))
       b<-hist(x$SSB_percieved,freq=T,main=main,
             xlab='Percieved SSB')

})


a$logSSB_percieved<-log(a$SSB_percieved)
by(a,list(a$Year,a$Species.n),function(x) {
      if ((i %% (nox*noy))==0 & nsp>1) {
         newplot(dev,nox,noy,Portrait=TRUE);
         par(mar=c(3,5,3,2))
       }
       i<<-i+1
       mean_SSB<-mean(x$logSSB_percieved)
       CV_SSB<-sd(x$logSSB_percieved) 
       main=paste("Log values, mean:",round(mean_SSB,2), "t sd:",formatC(CV_SSB,digits=2,format='f'))
       b<-hist(x$logSSB_percieved,freq=T,main=main,
             xlab='Percieved SSB')

})
