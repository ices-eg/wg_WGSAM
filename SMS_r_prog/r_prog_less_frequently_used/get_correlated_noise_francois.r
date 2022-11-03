# Data are made by get_correlation3.R
# get the correlated noise from values of F at age in the terminal year, and N  at age in the Terminal Year+1

#data.path<-      # directory vi data

# read covariance matrix
outFile<-file.path(data.path,"Nnext_F_covariance.in")
COV<-read.csv(file=outFile)
COV<-as.matrix(COV)
dimnames(COV)[[1]]<- dimnames(COV)[[2]]


# read estimated values used for constructing covariance matrix
outFile<-file.path(data.path,"Nnext_F_value.in")
est<-scan(file=outFile)

#read labels for noise
outFile<-file.path(data.path,"Nnext_F_labels.in")
label<-read.table(file=outFile,header=T)


# find beging and end of each type of data
Nlast<-min(grep("term_F",dimnames(COV)[[1]]))-1
fin<-length(est)

# get the values of F at age in the terminal year, and N  at age in the Terminal Year+1
n<-1  # number of replicates
if (n==1) {
  noiseFactor<-mvrnorm(n=n, mu=est, Sigma=COV)
  noiseFactor[1:Nlast]<- exp(noiseFactor[1:Nlast]-est[1:Nlast])   # N
  noiseFactor[(Nlast+1):fin]<- noiseFactor[(Nlast+1):fin]/est[(Nlast+1):fin] # F
  noise<-data.frame(label,noiseFactor=noiseFactor)
}
if (n>1) {
  noiseFactor<-mvrnorm(n=n, mu=est, Sigma=COV)
  noiseFactor[,1:Nlast]<- exp(noiseFactor[,1:Nlast]-rep(est[1:Nlast],each=n))   # N
  noiseFactor[,(Nlast+1):fin]<- noiseFactor[,(Nlast+1):fin]/rep(est[(Nlast+1):fin],each=n) # F
  apply(noiseFactor[,1:4],2,summary)

  X11()
  par(mfcol=c(7,7),mar=c(3,1,1,1))     #c(bottom, left, top, right)
  for (i in (1:dim(noiseFactor)[[2]])) {
    hist(noiseFactor[,i],main=dimnames(noiseFactor)[[2]][[i]])
  }
}


noise

