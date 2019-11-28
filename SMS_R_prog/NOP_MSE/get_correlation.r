useLog<-0  # 0= log N only
           # 1= log N  and log exploitation patter


a<-Read.SMS.std()

aa<-subset(a,name=='log_exploi_pattern')
aa$exp=exp(aa$value)
aa


# extract covariance or correlation matrix

fit<-read.fit()
#str(fit)

getCorCov<-function(vname,fit){
    idx<-which(fit$names %in% vname)
    COR<-fit$cor[idx,idx]
    COV<-fit$cov[idx,idx]
    loc.names<-sub('log_exploi_pattern','Expl',fit$names[idx])
    loc.names<-sub('term_logN_next','N',loc.names)
    labels<-paste(loc.names,paste("Q",fit$quarter[idx],sep=''),paste("A",fit$age[idx],sep=''),sep='.')

    dimnames(COR)<-list(labels,labels)
    dimnames(COV)<-list(labels,labels)
    val<-fit$est[idx]
    return(list(cov=COV,cor=COR,val=val,species=fit$species[idx],age=fit$age[idx],vari=fit$names[idx]))
}


 
if (useLog==0) a<-getCorCov(vname=c("term_logN_next"),fit=fit)
if (useLog==1) a<-getCorCov(vname=c("term_logN_next","log_exploi_pattern"),fit=fit)


COV<-a$cov
est<-a$val  # estimate used to for COV

COV<-as.matrix(COV)
diag(COV)

cleanup()
levelplot(a$cor,xlab='',ylab='',main='Correlation Before',at=seq(-1,1,0.05))

# get the values of F at age in the terminal year, and N  at age in the Terminal Year+1
# mvrnom is doing the matrix decomposition via eigen; although a Choleski decomposition might be faster, the eigendecomposition is stabler.
n<-10000
x<-mvrnorm(n=n, mu=est, Sigma=COV,tol=1e-5)  # I had to change the tolerance from the default value

cleanup()
X11()
levelplot(a$cor,xlab='',ylab='',main='Correlation Before',at=seq(-1,1,0.05))
X11()
levelplot(cor(x),xlab='',ylab='',main='Correlation After',at=seq(-1,1,0.05))

X11()
par(mfcol=c(4,5),mar=c(3,1,1,1))     #c(bottom, left, top, right)
for (i in (1:dim(x)[[2]])) {
  hist(x[,i],main=dimnames(x)[[2]][[i]])
}

X11()
par(mfcol=c(4,5),mar=c(3,1,1,1))     #c(bottom, left, top, right)
for (i in (1:dim(x)[[2]])) {
  hist(exp(x[,i]),main=dimnames(x)[[2]][[i]])
}



qr(crossprod(COV))$rank
cat('Should be:',dim(COV)[[1]])

aa<-try(chol(COV))
#library(mvtnorm)



#  modified from rmvnorm in library(mvtnorm)
arrange<- function (sigma,   method = c("eigen", "svd", "chol","cholP"), pre0.9_9994 = FALSE) 
{
  method <- match.arg(method)
  R <- if (method == "eigen") {
    ev <- eigen(sigma, symmetric = TRUE)
    if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))) {
      warning("sigma is numerically not positive semidefinite")
    }
    t(ev$vectors %*% (t(ev$vectors) * sqrt(pmax(ev$values, 
                                                0))))
  }
  else if (method == "svd") {
    s. <- svd(sigma)
    if (!all(s.$d >= -sqrt(.Machine$double.eps) * abs(s.$d[1]))) {
      warning("sigma is numerically not positive semidefinite")
    }
    t(s.$v %*% (t(s.$u) * sqrt(pmax(s.$d, 0))))
  }
  else if (method == "cholP") {
    R <- chol(sigma, pivot = TRUE)
    R[, order(attr(R, "pivot"))]
  }
  else if (method == "chol") {
      R <- chol(sigma, pivot = FALSE)
   }
  return(R)
}


qr(crossprod(COV))$rank

use<-dim(COV)[[1]]  # the full Sigma is not positive-definite 
a1<-arrange(sigma=COV[1:use,1:use],method="eigen")
a2<-arrange(sigma=COV[1:use,1:use],method="svd")
a3<-arrange(sigma=COV[1:use,1:use],method="chol")
a4<-arrange(sigma=COV[1:use,1:use],method="cholP")

a1
a2  # same as a1
a3  #  sometimes fails
a4

n=100000
noise<-matrix(rnorm(n * use), n, use)

a11<-noise %*% a1
a21<-noise %*% a2
a31<-noise %*% a3
a41<-noise %*% a4

round(cov(a11),5)
round(cov(a21),5)
round(cov(a31),5)
round(cov(a41),5)
round(cov(a11)/cov(a21),5) # no diffrence
round(cov(a11)/cov(a41),5) # small difference
round(cov(a41)/cov(a31),5) # small difference

wr_cov<-function(COV,m=NULL,type=c("cov","decomposed"),var=c('n','nf')) {
  if (type=="cov" & var=="n") out<-file.path(data.path,'covariance_n.in')
  if (type=="cov" & var=="nf") out<-file.path(data.path,'covariance_nf.in')
  if (type=="decomposed" & var=="n") out<-file.path(data.path,'decomposition_n.in')
  if (type=="decomposed" & var=="nf") out<-file.path(data.path,'decomposition_nf.in')
  
  
  cat(1, "  # number of blocks\n",file=out)
  cat(dim(COV)[[1]], "  # dimension of matrix\n ",file=out,append=TRUE)
  cat("# variables:\n",paste("# ",colnames(COV),"\n") ,file=out,append=TRUE)
  if (type=="cov") write.table(COV,file=out,append=TRUE,col.names=FALSE,row.names=FALSE)
  if (type=="decomposed") write.table(m,file=out,append=TRUE,col.names=FALSE,row.names=FALSE)
}

if (useLog==0) wr_cov(COV,m=NULL,type="cov",var='n')
if (useLog==0) wr_cov(COV,m=a2,type="decomposed",var='n')

if (useLog==1) wr_cov(COV,m=NULL,type="cov",var='nf')
if (useLog==1) wr_cov(COV,m=a1,type="decomposed",var='nf')

