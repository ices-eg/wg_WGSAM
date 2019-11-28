# extract covariance or corellation matrix

fit<-read.fit()

getCorCov<-function(name,fit,sp=1,xtype='COR'){
    idx<-which(fit$names==name & sp==fit$species)
    if (xtype=="COR") x<-fit$cor[idx,idx] else x<-fit$cov[idx,idx]
    return(x)
  }


outFile<-file.path(data.path,"covariance_N.in")

cat("# co-variance matrix of  log stock number in the first year after the last assessment year (or period)\n",
    "# made by program get_correlation.R\n",file=outFile)                                       
for (s in (1:nsp)) {
  cat(paste("\n#",sp.names[s],"\n"),file=outFile,append=T)
  a<-getCorCov("log_term_N_next",fit,sp=s,xtype='COV')
  write.table(a,file=outFile,row.names = F,col.names = F,append=T)
  a<-SMS.control@species.info[s,"last-age"]+1
  while (a<=SMS.control@max.age.all ) {
    cat(rep(0,SMS.control@species.info[s,"last-age"]),file=outFile,append=T)
        cat("\n",file=outFile,append=T)
    a<-a+1
   } 
}





#############################
sp<-1
varName<-"log_term_N_next"
round(getCorCov(varName,fit,sp=sp,xtype='COR'),2)
getCorCov(varName,fit,sp=sp,xtype='COV')

ch<-getCorCov(varName,fit,sp=sp,xtype='COV')
chol(ch)
###################################

library(mvtnorm)
  sp<-1   # species number

  n<-10000  # 
 sigma <- getCorCov(varName,sp=sp,fit,xtype='COV')
 est<-fit$est[which(fit$names==varName & sp==fit$species) ]
 round(sigma,3)
 est
  
  retval <- chol(sigma, pivot = TRUE)
  retval
  o <- order(attr(retval, "pivot")) 
  rank(apply(cor(retval),1,sum))
  retval <- retval[, o]
  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n) %*% retval
  retval <- sweep(retval, 2, est, "+")
  colMeans(retval)
  var(retval)

# from library(mvtnorm) 
x <- rmvnorm(n=n, mean=est, sigma=sigma, method="chol")
round(colMeans(x) /est,2)
round(var(x)/sigma,2)

# standard R, seems better!
x<-mvrnorm(n, mu=est, Sigma=s)
round(colMeans(x),2)
round(colMeans(x) /est,2)
round(var(x),2)
round(var(x)/sigma,2)

  # 1.create (manually!!!) input data to simple.dat (est and covariance)
  # 2. run simple.exe
  
  # 3 read results from simple
 a<- matrix(scan(file='c:\\MV\\SMS\\program\\simple.out'),ncol=dim(sigma)[1],byrow=T)
 colMeans(a)
 round(var(a),3)
 round(var(a)/sigma,2)
 apply(a,2,var)
 
 
 b<-exp(a)
 apply(b,2,mean)/exp(est)
 apply(b,2,median)/exp(est)
 adjust<-exp(-diag(sigma)/2)   #median to mean adjustment
 apply(b,2,mean)*adjust/exp(est)
  
  
 est2<-fit$est[which(fit$names=="term2_N" & sp==fit$species) ]
 est2/exp(est)
 
 sigma2<-getCorCov("term2_N",fit,sp=sp,xtype='COV')
x<-mvrnorm(n, mu=est2, Sigma=sigma2)
apply(x,2,mean)
apply(b,2,mean)
apply(b,2,mean)*adjust
