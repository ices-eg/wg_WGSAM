useLog<-1  # 0= no log N and no log F, 1=log N and (no log) F, 2=log N and log F

# extract covariance or correlation matrix

fit<-read.fit()
#str(fit)

getCorCov<-function(vname,fit){
    idx<-which(fit$names %in% vname)
    COR<-fit$cor[idx,idx]
    COV<-fit$cov[idx,idx]
    labels<-paste(fit$names[idx],paste("sp",fit$species[idx],sep=''),paste("Age",fit$age[idx],sep=''),sep='.')
    dimnames(COR)<-list(labels,labels)
    dimnames(COV)<-list(labels,labels)
    val<-fit$est[idx]
    return(list(cov=COV,cor=COR,val=val,species=fit$species[idx],age=fit$age[idx],vari=fit$names[idx]))
}

 a<-getCorCov(vname=c("term_N_next","term_N_next"),fit=fit)
 a
 
 
if (useLog==0) a<-getCorCov(vname=c("term_N_next","term_F"),fit=fit)
if (useLog==1) a<-getCorCov(vname=c("log_term_N_next","term_F"),fit=fit)
if (useLog==2) a<-getCorCov(vname=c("log_term_N_next","log_term_F"),fit=fit)


COV<-a$cov
est<-a$val  # estimate used to for COV

if (T) {   # test input and output
  outFile<-file.path(data.path,"OP_Nnext_F_covariance.in")
  write.matrix(COV,file=outFile,sep=',')
  COV<-read.csv(file=outFile)
  dimnames(COV)[[1]]<-dimnames(COV)[[2]]
  outFile<-file.path(data.path,"OP_Nnext_F_value.in")
  write(est,file=outFile)
  est<-scan(file=outFile)
  
  outFile<-file.path(data.path,"OP_Nnext_F_labels.in")
  write.table(data.frame(Species=a$species,Age=a$age,vari=ifelse(a$vari=="log_term_N_next",'N','F')),file=outFile,col.names=T,row.names=F)

}

cleanup()
levelplot(a$cor,xlab='',ylab='',main='Before',at=seq(-1,1,0.05))

# get the values of F at age in the terminal year, and N  at age in the Terminal Year+1
n<-10000
x<-mvrnorm(n=n, mu=est, Sigma=COV)


Nlast<-min(grep("term_F",dimnames(COV)[[1]]))-1
fin<-length(est)

if (useLog==0) noiseFactor<-x/rep(est,each=n)
if (useLog==1) {
  noiseFactor<-x
  noiseFactor[,1:Nlast]<- exp(x[,1:Nlast]-rep(est[1:Nlast],each=n))
  noiseFactor[,(Nlast+1):fin]<- exp(x[,(Nlast+1):fin]-rep(est[(Nlast+1):fin],each=n))
}
if (useLog==2) noiseFactor<-x-rep(est,each=n)

X11()
levelplot(cor(noiseFactor),xlab='',ylab='',main='After',at=seq(-1,1,0.05))

apply(noiseFactor[,1:4],2,summary)

X11()
par(mfcol=c(7,7),mar=c(3,1,1,1))     #c(bottom, left, top, right)
for (i in (1:dim(noiseFactor)[[2]])) {
  hist(noiseFactor[,i],main=dimnames(noiseFactor)[[2]][[i]])
}

