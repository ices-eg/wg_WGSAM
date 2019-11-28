# some times set in another program 
#iter<-3  # number of iterations

library(MASS)
test<-F
useCovariance<-T


fit<-read.fit()
nopar<-fit$nopar
sigma<-fit$cov[1:nopar,1:nopar]

if (!useCovariance) {
 vari<-diag(sigma)
 sigma[,]<-0
 diag(sigma)<-vari
}

mu<-fit$est[1:nopar]
pars<-mvrnorm(n = iter, mu, sigma)

if (test) {
  hist(pars[,1]) # just a check
  m<-apply(pars,c(2),mean)
  std<-apply(pars,c(2),sd)
  CV<- std/m
  round(CV,2)
}

# write the sms.psv file
zz <- file(file.path(data.path,"sms.psv"), "wb")
writeBin(as.integer(nopar),zz)
for (i in (1:iter)) writeBin(as.double(pars[i,]),zz)

close(zz)


#testing
if (test) {
  zz <- file(file.path(data.path,"sms.psv"), "rb")
  nopar<-readBin(zz, integer())
  nopar
  a<-readBin(zz, numeric(), nopar )
  b<-readBin(zz, numeric(), nopar )
  a[1:10]
  b[1:10]
  a[1:10]/b[1:10]
  close(zz)
}
