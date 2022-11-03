library(car)
ex<-c('catage','simple','baltic-test')[3]
setwd(file.path("C:/_C_drev/SMS-git",ex))


source(file.path(prog.path,'function','getADMBHessian.R'))
if (ex=='baltic-test') ex<-'sms'
a<-read.admbFit(ex)
names(a)


hes<-getADMBHessian()
scale<-hes$scale
cov<-solve(hes$hes)
cov.bounded<-cov*(scale %o% scale)
sum(abs(a$cov[1:a$nopar,1:a$nopar]-cov.bounded))  # cov.bounded is the right one (the same as cov)
sum(abs(a$cov[1:a$nopar,1:a$nopar]-cov))



read_admodel_dep<-function() {
  fi<-'admodel.dep'
  d<-scan(file=fi,nmax=2,skip=0)
  n<-d[1]*d[2]
  matrix(scan(file=fi,nmax=n,skip=1),ncol=d[1],nrow=d[2],byrow=TRUE)
}
deps<-read_admodel_dep()


a$est[1:a$nopar] %*% deps[1,] ; a$est[a$nopar+1]# estimate of derived value, works only for "simple" which is a linear model
a$est[1:a$nopar] %*% deps[2,] ;a$est[a$nopar+2]# 

#################




#########################
calc_sd<-function(p=1) {
  coefs<-a$est[1:a$nopar]
  coefs
  names(coefs)<-paste0('X',seq(1:a$nopar))
  coefs
  eq<-paste0('X',seq(1:a$nopar),'*',deps[p,]/scale)
  eq<-paste(eq,collapse='+')
  eq
  res<-deltaMethod(coefs, g.= eq, vcov.=a$cov[1:a$nopar,1:a$nopar])
  return(unlist(res))
}

# the Estimate is wrong, but SE is right. The Estmate is also right for "simple" (linear model)
calc_sd(1);a$est[a$nopar+1];a$std[a$nopar+1]
calc_sd(2);a$est[a$nopar+2];a$std[a$nopar+2]
if (ex!='simple') {
  calc_sd(5);a$est[a$nopar+5];a$std[a$nopar+5]
  calc_sd(9);a$est[a$nopar+9];a$std[a$nopar+9]
  calc_sd(13);a$est[a$nopar+13];a$std[a$nopar+13]
}


Xp<-deps/rep(scale,each=dim(deps)[[1]])
Xp[1:3,1:5]

COV<-a$cov[1:a$nopar,1:a$nopar]

# variance of predictions 
diag(Xp %*% COV %*% t(Xp))

# more efficiently via
var.fit <- rowSums((Xp %*% COV) * Xp)  ## point-wise variance for predicted mean
var.fit
se<-sqrt(var.fit)
se[1:10]
a$std[(a$nopar+1):(a$nopar+11)]

sum(abs(a$std[(a$nopar+1):a$totPar]-se)) ## SSB har large std, which explain the large sum

###

a$cor[grep('other_pred_noise_fac',a$names)[1],] 

new_sd<-0.15
noise_fac<-COV[grep('other_pred_noise_fac',a$names)[1],grep('other_pred_noise_fac',a$names)[1]] 
sqrt(noise_fac)

COV[grep('other_pred_noise_fac',a$names)[1],grep('other_pred_noise_fac',a$names)[1]] <-new_sd^2

se2<- sqrt(rowSums((Xp %*% COV) * Xp))  ## point-wise variance for predicted mean)
se2-se  #larger for SSB
se2/se  # larger for SSB

### make a new COV with updated co-variance 
COR<-a$cor[1:a$nopar,1:a$nopar]
SE<-a$std[1:a$nopar]
SE[grep('other_pred_noise_fac',a$names)[1]] <-new_sd



COV2<-  diag(SE) %*% COR %*% diag(SE)  #get the covariance from the edited SE and the old correlation matrix
COV3<-   sweep(sweep(COR, 1, SE, "*"), 2, SE, "*")  #An efficient implementation, get the covariance from the edited SE and the old correlation matrix
sum(COV2-COV3)

se3<- sqrt(rowSums((Xp %*% COV3) * Xp))  ## point-wise variance for predicted mean)
se3/se2  # they are quite different






deps[grep('other_pred_noise_fac',a$names)[1],] #  other_pred_noise_fac dependency on all other 

deps[grep('hist_SSB',a$names)-a$nopar,grep('other_pred_noise_fac',a$names)[1]] # SSB dependency on other_pred_noise_fac
