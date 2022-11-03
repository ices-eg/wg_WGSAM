# first make a a determenisti and stochastic forcast and save the results in for.det and for.sto
# load('run/FORECAST.RData'); for.det<-forecast; summary(for.det); summary(for.det[[1]]);
# load('run/FORECAST.RData'); for.sto<-forecast ;  summary(for.sto);  summary(for.sto[[1]]);

# N2013
N2013.sto<-lapply(for.sto,function(x){exp(apply(x$last.state.sim[,1:10],2,median))}) 
N2013.det<-lapply(for.det,function(x){exp(apply(x$last.state.sim[,1:10],2,median))}) 
N2013.det
matrix(unlist(N2013.sto),ncol=10,byrow=T)/matrix(unlist(N2013.det),ncol=10,byrow=T)



# N2014
N2014.sto<-lapply(for.sto,function(x){exp(apply(x$s1.state.sim[,1:10],2,median))}) 
N2014.det<-lapply(for.det,function(x){exp(apply(x$s1.state.sim[,1:10],2,median))}) 
N2014.det
matrix(unlist(N2014.sto),ncol=10,byrow=T)/matrix(unlist(N2014.det),ncol=10,byrow=T)

SSB2014.sto<-lapply(for.sto,function(x){(apply(x$s1.ssb.sim,2,median))}) 
SSB2014.det<-lapply(for.det,function(x){(apply(x$s1.ssb.sim,2,median))}) 
unlist(SSB2014.sto)/unlist(SSB2014.det)


for.det[[1]]$s2.ssb.sim

# N2015          
N2015.sto<-lapply(for.sto,function(x){exp(apply(x$s2.state.sim[,1:10],2,median))}) 
N2015.det<-lapply(for.det,function(x){exp(apply(x$s2.state.sim[,1:10],2,median))}) 
N2015.det
round(matrix(unlist(N2015.sto),ncol=10,byrow=T)/matrix(unlist(N2015.det),ncol=10,byrow=T),4)

#WB herring
SSB2015.sto<-lapply(for.sto,function(x){median(x$s2.ssb.sim,2)}) 
SSB2015.det<-lapply(for.det,function(x){median(x$s2.ssb.sim,2)}) 


SSB2015.sto<-lapply(for.sto,function(x){(apply(x$s2.ssb.sim,2,median))}) 
SSB2015.det<-lapply(for.det,function(x){(apply(x$s2.ssb.sim,2,median))}) 

unlist(SSB2015.sto)/unlist(SSB2015.det)



# N2016          
N2016.sto<-lapply(for.sto,function(x){exp(apply(x$s3.state.sim[,1:10],2,median))}) 
N2016.det<-lapply(for.det,function(x){exp(apply(x$s3.state.sim[,1:10],2,median))}) 
N2016.det
round(matrix(unlist(N2016.sto),ncol=10,byrow=T)/matrix(unlist(N2016.det),ncol=10,byrow=T),4)



SSB2016.sto<-lapply(for.sto,function(x){(apply(x$s3.ssb.sim,2,median))}) 
SSB2016.det<-lapply(for.det,function(x){(apply(x$s3.ssb.sim,2,median))}) 
 
#WB herring
SSB2016.sto<-lapply(for.sto,function(x){median(x$s3.ssb.sim,2)}) 
SSB2016.det<-lapply(for.det,function(x){median(x$s3.ssb.sim,2)}) 

round(unlist(SSB2016.sto)/unlist(SSB2016.det),4)

for.det[[1]]$s2.catch.sim
C2015.sto<-lapply(for.sto,function(x){(median(x$s2.catch.sim))}) 
C2015.det<-lapply(for.det,function(x){(median(x$s2.catch.sim))}) 
round(unlist(C2015.sto)/unlist(C2015.det),4)



#WB herring
SSB2015.sto<-lapply(for.sto,function(x){median(x$s2.ssb.sim,2)}) 
SSB2015.det<-lapply(for.det,function(x){median(x$s2.ssb.sim,2)}) 


# extract N 2015 from the first forecast option
matrix(unlist(N2015.sto),ncol=10,byrow=T)
N<-matrix(unlist(N2015.sto),ncol=10,byrow=T)[1,]
N

# just checing they are the same
for.sto[[1]]$ave.sw
for.det[[1]]$ave.sw

sum(for.sto[[1]]$ave.sw*for.sto[[1]]$ave.pm*N)   # the simple way
N %*% (for.sto[[1]]$ave.sw*for.sto[[1]]$ave.pm)  # the Anders way
SSB2015.sto[[1]]
SSB2015.sto[[1]] / sum(for.sto[[1]]$ave.sw*for.sto[[1]]$ave.pm*N)

sum(for.det[[1]]$ave.sw*for.sto[[1]]$ave.pm*N)   # the simple way
N %*% (for.sto[[1]]$ave.sw*for.sto[[1]]$ave.pm)  # the Anders way
SSB2015.det[[1]]
SSB2015.det[[1]] /sum(for.det[[1]]$ave.sw*for.sto[[1]]$ave.pm*N) 


#The right way
lapply(for.det,function(x){apply(exp(x$s2.state.sim[,1:10])*rep(x$ave.sw*x$ave.pm,each=2),1,sum)}) 
SSB2015.det<-lapply(for.det,function(x){median(apply(exp(x$s2.state.sim[,1:10])*rep(x$ave.sw*x$ave.pm,each=2),1,sum))}) 

lapply(for.sto,function(x){apply(exp(x$s2.state.sim[,1:10])*rep(x$ave.sw*x$ave.pm,each=1000),1,sum)})
SSB2015.sto<-lapply(for.sto,function(x){median(apply(exp(x$s2.state.sim[,1:10])*rep(x$ave.sw*x$ave.pm,each=1000),1,sum))}) 

unlist(SSB2015.sto)/unlist(SSB2015.det)

# ###########################################
 library(MASS)
 
noSim<-30000
var1<-0.8
var2<-var1*0.75
var12<-0.1    # co-var
mu1<-4
mu2<-mu1/2
weight<-c(1,10)
sigma<-matrix(c(var1,var12,var12,var2),ncol=2)
logN<-mvrnorm(noSim, mu=c(mu1,mu2), Sigma=sigma)

plot(logN[,1],logN[,2])
hist(logN[,1])

N<-exp(logN)
N.median<-apply(N,2,median)
#N.median
if (noSim<=10) log(N.median)
if (noSim<=10) c(mu1,mu2)    # there is no bias in N
bio.deter<-exp(c(mu1,mu2))*weight
if (noSim<=10) bio.deter
bio.deter<-sum(bio.deter)
if (noSim<=10) bio.deter

if (noSim<=10) N
bio<-N*rep(weight,each=noSim)
if (noSim<=10) bio

bio<-apply(bio,1,sum)
if (noSim<=10) bio
bio.sto<-median(bio)

bio.sto/bio.deter
#######

# The same, but mu and variance and weight is the same for both of them

noSim<-1000000
sd1<-0.8
var1<-sd1**2
var2<-var1
var12<-var1/4    # co-var
mu1<-4
mu2<-mu1*1
weight<-c(1,1)
sigma<-matrix(c(var1,var12,var12,var2),ncol=2)
logN<-mvrnorm(noSim, mu=c(mu1,mu2), Sigma=sigma)

par(mfcol=c(2,2))
hist(logN[,1],xlab='logN1',main=paste("age 1, logN, mean:",round(mean(logN[,1]),4)))
abline(v=mean(logN[,1]),col='red')

hist(exp(logN[,1]),xlab='exp(logN1)',main=paste("age 1, exp(logN),median:",round(median(exp(logN[,1])),4),'\n exp(logN):',round(exp(mu1),4)))
abline(v=median(exp(logN[,1])),col='red')

hist(logN[,2],xlab='logN2',main=paste("age 2, logN, mean:",round(mean(logN[,2]),4)))
abline(v=mean(logN[,2]),col='red')

hist(exp(logN[,2]),xlab='exp(logN1)',main=paste("age 2, exp(logN),median:",round(median(exp(logN[,2])),4),'\n exp(logN):',round(exp(mu1),4)))
abline(v=median(exp(logN[,2])),col='red')

par(mfcol=c(1,1))

N<-exp(logN)
bio.deter<-exp(c(mu1,mu2))*weight
bio.deter<-sum(bio.deter)
bio.deter

bio<-N*rep(weight,each=noSim)
bio<-apply(bio,1,sum)
bio.sto<-median(bio)
hist(bio,main=paste('median bio:',round(bio.sto,4),'\n determ bio:',round(bio.deter,4),'\n ratio stock:determ',round(bio.sto/bio.deter,4)))
abline(v=bio.sto,col='red')

#######

neafc<-function(sd1=0.8,var2Fac=1,mu1=4,mu2Fac=1,noSim=100000,weight=c(1,10)){
  var1<-sd1*sd1
  var2<-var1*var2Fac
  var12<-var1/5    # co-var 
  
  mu2<-mu1*mu2Fac
  sigma<-matrix(c(var1,var12,var12,var2),ncol=2)
  logN<-mvrnorm(noSim, mu=c(mu1,mu2), Sigma=sigma)
  N<-exp(logN)
  bio.sto<-N*rep(weight,each=noSim)
  bio.sto.corrected<-bio.sto # "corrected" later on

  bio.sto<-apply(bio.sto,1,sum)
  bio.sto<-median(bio.sto)

  #cat('\nbefore adjustement')
  #print(bio.sto.corrected) 
  
  cor.fac<-exp(rep(0.5*c(var1,var2),each=noSim))
  #print(cor.fac)
  
  bio.sto.corrected<-bio.sto.corrected/cor.fac
  
  #cat('\nafter adjustement\n')
  #print(bio.sto.corrected)
   bio.sto.corrected<-apply(bio.sto.corrected,1,sum)
  #cat('\nsum after adjustement\n')
  #print(bio.sto.corrected)
  bio.sto.corrected<-mean( bio.sto.corrected)
  # cat('\nmean of sum after adjustement\n')
  #print(bio.sto.corrected)
   
  bio.deter<-sum(exp(c(mu1,mu2))*weight)
  
  return(list(dterm=bio.deter,stoch=bio.sto,stoch.corr=bio.sto.corrected))
}


sd1<-seq(0.1,1.0,0.05)
do.it<-function(i){neafc(sd1=sd1[i],mu2Fac=1,var2Fac=1,weight=c(1,1)) }
res<-lapply(1:length(sd1),do.it)

res<-matrix(unlist(res),ncol=3,byrow=T)
matplot(sd1,res,ylab='Absolute values') #abs values

matplot(sd1,res/rep(res[,1],each=3),ylab='Relativ to deterministic') #relative to deterministic values


