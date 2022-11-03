# noise for MODEL 1
library(MASS)
set.seed(109)

ages<-SMS.control@species.info[,1:2] 
nop<-first.VPA-1
modelByAge<- c(-1,1,2)[2]  # -1: no noise, 1: same noise for all ages, 2:age dependent noise 
modelByAge<-rep(modelByAge,nop)

varAsParameter<-1 # 1=estimate variance as a parameter ; 0=use empirical variance
boundsOnVar<- c(0.01,1)  # lower and upper bounds on variance (if estimated as parameter)

correlated<-TRUE   # correlated noise between age groups of same species (it really does not matter in SMS)
correlation<-0.4

n<-20; 
(s<-0.1*sqrt(n))
s<-rep(s,nop)


w_factor<-1; phase<-3 ;empi<-TRUE 
lower<-0.5; upper<-1.5;   #parameter bounds


## test
if (n>=2) {
  a<-mvrnorm(n , mu = 1, Sigma = matrix(s^2,ncol=1,nrow=1), empirical = TRUE)
  mean(a)
  sd(a)
  
  #empirical = FALSE
  a<-mvrnorm(n , mu = 1, Sigma = matrix(s^2,ncol=1,nrow=1), empirical = FALSE)
  mean(a)
  sd(a)
  ## end test
}


fi<-file.path(data.path,"other_pred_n_noise.dat")
cat("# File, noise_other_pred.dat, with data for simulating uncertanties in abundance of other predators \n",file=fi)
cat(phase, " # phase for estimating noise\n",file=fi,append=T)
cat(lower, ' ', upper, " # Lower and upper bound for the parameters\n",file=fi,append=TRUE)
cat(" # Weighting factor for likelihood contributions for noise factor\n",file=fi,append=TRUE)
cat("# ",formatC(sp.names[1:nop],12),'\n',file=fi,append=TRUE)
cat("  ",formatC(rep(w_factor,nop),width=11),'\n',file=fi,append=TRUE)
cat(varAsParameter," # 1=estimate variance as a parameter ; 0=use empirical variance\n",file=fi,append=TRUE)
    
cat(boundsOnVar, " # lower and upper bounds on variance (if estimated as parameter)\n",file=fi,append=TRUE)


cat("1  # model type: 0=no noise, 1=from scaling factors  with mean 1 and std of the mean at a level set at target, 2=from noise on observations\n",file=fi,append=TRUE) 
cat(n,'  # Number of "observations" used used to fit uncertanty for each other predator. \n',file=fi,append=TRUE)



cat(" # Usage of age dependent noise factor. -1: no noise, 1: same noise for all ages, 2:age dependent noise \n",file=fi,append=TRUE)
cat("# ",formatC(sp.names[1:nop],12),'\n',file=fi,append=TRUE)
cat("  ",formatC(modelByAge,width=11),'\n',file=fi,append=TRUE)

for (sp in (1:nop)) {  
  set.seed(sp)
  cat(sp.names[sp],'\n')
  cat("######## ",sp.names[sp],'\n',file=fi,append=TRUE)
  if (modelByAge[sp]== -1)  cat(rep -1,n,'\n',fi=file,append=TRUE)
  if (modelByAge[sp]==  1) {
    b<-mvrnorm(n , mu = 1, Sigma = matrix(s[sp]^2,ncol=1,nrow=1), empirical = empi)
    cat('# "observations" used to mimic a factor with mean 1 and wanted std the mean at ',s[sp],' \n',file=fi,append=TRUE)
    cat(b,'\n',file=fi,append=TRUE)
  }
  if (modelByAge[sp]==  2) {
    if (!correlated) for (a in (ages[sp,2]:ages[sp,1])) {
      b<-mvrnorm(n , mu = 1, Sigma = matrix(s[sp]^2,ncol=1,nrow=1), empirical = empi)
      cat('# age ',a,' " observations" used to mimic a factor with mean 1 and wanted std the mean at ',s[sp],' \n',file=fi,append=TRUE)
      cat(b,'\n',file=fi,append=TRUE)
    }
    if (correlated) 
      nOages<-ages[sp,1]-ages[sp,2]+1
      SE<-rep(s[sp],nOages)
      COR<-matrix(correlation,nrow=nOages,ncol=nOages)
      diag(COR)<-1.0
      COV<-  diag(SE) %*% COR %*% diag(SE)
      for (a in (ages[sp,2]:ages[sp,1])) {
      b<-mvrnorm(n , mu = rep(1,nOages), Sigma = COV, empirical = empi)
      apply(b,2,mean)
      apply(b,2,sd)
      i<-0
      for (a in (ages[sp,2]:ages[sp,1])) { 
        i<-i+1
        cat('# age ',a,' CORRELATED " observations" used to mimic a factor with mean 1 and wanted std the mean at ',s[sp],' \n',file=fi,append=TRUE)
        cat(b[,i],'\n',file=fi,append=TRUE)
      }
    }
   }
}



 