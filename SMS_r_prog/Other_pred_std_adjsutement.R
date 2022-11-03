new_sd<-0.1
write_new_std_file<-FALSE

test<-FALSE
if (test) {
  ex<-c('catage','simple','Baltic-test-run_04')[3]
  setwd(file.path("C:/_C_drev/SMS-git",ex))
  if (ex=='baltic-test') ex<-'sms'
} else ex<-'sms'

source(file.path(prog.path,'function','getADMBHessian.R'))


a<-read.admbFit(ex)
names(a)


a$est[grep('other_pred_noise_fac',a$names)[1]]
sqrt(a$cov[grep('other_pred_noise_fac',a$names)[1],grep('other_pred_noise_fac',a$names)[1]])

#CV
sqrt(a$cov[grep('other_pred_noise_fac',a$names)[1],grep('other_pred_noise_fac',a$names)[1]])/a$est[grep('other_pred_noise_fac',a$names)[1]]

if (test) {
  hes<-getADMBHessian()
  scale<-hes$scale
  cov<-solve(hes$hes)
  cov.bounded<-cov*(scale %o% scale)
  sum(abs(a$cov[1:a$nopar,1:a$nopar]-cov.bounded))  # cov.bounded is the right one (the same as cov)
  sum(abs(a$cov[1:a$nopar,1:a$nopar]-cov))
}


read_admodel_dep<-function() {
  fi<-'admodel.dep'
  d<-scan(file=fi,nmax=2,skip=0)
  n<-d[1]*d[2]
  matrix(scan(file=fi,nmax=n,skip=1),ncol=d[1],nrow=d[2],byrow=TRUE)
}
deps<-read_admodel_dep()

if (test) { # estimate of derived value
  a$est[1:a$nopar] %*% deps[1,] ; a$est[a$nopar+1]# estimate of derived value, works only for "simple" which is a linear model
  a$est[1:a$nopar] %*% deps[2,] ;a$est[a$nopar+2]# 
}
#################

hes<-getADMBHessian()
Xp<-deps/rep(hes$scale,each=dim(deps)[[1]])    # parameters (in the linear model), same as Jacobian ?

COV<-a$cov[1:a$nopar,1:a$nopar]  # covariance of parameters (not derived values)

a$cov[grep('other_pred_noise_fac',a$names)[1],grep('other_pred_noise_fac',a$names)[1]]

# variance of predictions 
if (test) diag(Xp %*% COV %*% t(Xp))

# more efficiently via
var.fit <- rowSums((Xp %*% COV) * Xp)  ## point-wise variance for predicted mean
se<-sqrt(var.fit)

if (test) {
  se[1:10]
  a$std[(a$nopar+1):(a$nopar+10)]
  se[1:10]/a$std[(a$nopar+1):(a$nopar+10)]
  sum(abs(a$std[(a$nopar+1):a$totPar]-se)) ## SSB har large std, which explain the large sum (due to roundings? I hope)
  sum(abs(a$std[(a$nopar+1):a$totPar]/se)) ## SSB har large std, which explain the large sum (due to roundings? I hope)
}
###

if (test) {
  # parameters are highly correlated with other_pred_noise_fac 
  aa<-a$cor[grep('other_pred_noise_fac',a$names)[1],] 
  aa
 
 
  b<-Read.SMS.std(excludeNotUsed=FALSE)
  b<-subset(b,select=c(name,parG,year,age))
  bb<-b;bb$cor<-aa
  
  xtabs(~name,data=bb)
  xtabs(cor~name,data=bb)
  b<-paste(b$parG,b$year,b$age,sep=':')
  names(aa)<-b
  
  aa
  sort(aa)
}



noise_fac<-COV[grep('other_pred_noise_fac',a$names)[1],grep('other_pred_noise_fac',a$names)[1]] 
sqrt(noise_fac)  #old estimated
# new_sd<-sqrt(noise_fac)  # test



if (test) { #just change the variance of multiplier (but not the co-variance)
  COV[grep('other_pred_noise_fac',a$names)[1],grep('other_pred_noise_fac',a$names)[1]] <-new_sd^2
  
  se2<- sqrt(rowSums((Xp %*% COV) * Xp))  ## point-wise variance for predicted mean)
  se2-se  #larger for SSB
  se2/se  # larger for SSB
  if (test) hist(se2/se)  # s2 is larger than s
  compare_s<-se2/se

  names(compare_s)<-a$names[(a$nopar+1):a$totPar]
  sort(compare_s)
  
  b<-Read.SMS.std()
  b<-paste(b$parG,b$year,b$age,sep=':')
  names(compare_s)<-b[(a$nopar+1):a$totPar]
  sort(compare_s)
}

### make a new COV with updated co-variance from correlation matrix 
COR<-a$cor[1:a$nopar,1:a$nopar]
SE<-a$std[1:a$nopar]
SE[grep('other_pred_noise_fac',a$names)[1]] <-new_sd

# COV<-  diag(SE) %*% COR %*% diag(SE)  #get the covariance from the edited SE and the old correlation matrix
COV<-   sweep(sweep(COR, 1, SE, "*"), 2, SE, "*")  #An efficient implementation, get the covariance from the edited SE and the old correlation matrix


se3<- sqrt(rowSums((Xp %*% COV) * Xp))  ## point-wise variance for predicted mean)
hist(se3/se)  # they are quite different


compare_s<-se3/se

names(compare_s)<-a$names[(a$nopar+1):a$totPar]
sort(compare_s)

b<-Read.SMS.std()
b<-paste(b$parG,b$year,b$age,sep=':')
names(compare_s)<-b[(a$nopar+1):a$totPar]
sort(compare_s)


data.path2<-file.path(root,'Baltic-test-run_04')
sdfi<-read.table(file=file.path(data.path2,'sms.std'),skip=1) 


sdfi<-data.frame(index=sdfi$V1,name=sdfi$V2, value=sdfi$V3,  std=sdfi$V4)
head(sdfi)
sdfi[sdfi$name=='other_pred_noise_fac','std']<-new_sd
sdfi[(a$nopar+1):a$totPar,'std']<-se3

if (write_new_std_file) write.table(sdfi,file=file.path(data.path2,'sms.std'),col.names = TRUE,row.names = FALSE,quote = FALSE)

if (test) {
  
  a$cor
  deps[grep('other_pred_noise_fac',a$names)[1],] #  other_pred_noise_fac dependency on all other 
  deps[grep('hist_SSB',a$names)-a$nopar,grep('other_pred_noise_fac',a$names)[1]] # SSB dependency on other_pred_noise_fac
}
