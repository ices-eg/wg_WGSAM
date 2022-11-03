
read.in.files<-function(years,quarters,areas,species,ages,filename) {
  
  a<-expand.grid(Area=areas,Species.n=species,Year=years,Quarter=quarters,Age=ages)
  a<-a[order(a$Area,a$Species.n,a$Year,a$Quarter,a$Age),]
  a$variable<-head(scan(file.path(data.path,filename),comment.char='#'),-1)
  a
}

op<-read.in.files(years=SMS.control@first.year:SMS.control@last.year,
                    quarters=1:SMS.control@last.season,
                    areas=1:SMS.control@no.areas,
                    species=1:1,
                    ages=SMS.control@first.age:SMS.control@max.age.all,
                    filename="other_pred_N.in")

names(op)<-c("Area","Species.n","Year","Quarter","Age","N")

head(op)

op<-subset(op,Age>0)

op2<-tapply(op$N,list(op$Species.n,op$Quarter,op$Year,op$Age),sum)
op2[1,1,,]

corMat<- cor(log(op2[1,1,,])) # age correlation, does not make sense, but just to get some numbers
corMat
# change to no correlation
corMat[]<-0; diag(corMat)<-1; corMat

stddev <- c(0.75,0.5,rep(0.35,9))
stddev

#Create the covariance matrix:
covMat <- stddev %*% t(stddev) * corMat
covMat

mu <- rep(0,11)
#Sample values. If you use empirical = FALSE, the means and covariance values represent the population values. Hence, the sampled data-set most likely does not match these values exactly.
#                If you sample with empirical = TRUE, the properties of the sampled data-set match means and covariances exactly. n has to be 2 or more

library(MASS)
dat1 <- mvrnorm(n = 212, mu = mu, Sigma = covMat, empirical = TRUE)
colMeans(dat1)
apply(dat1,2,sd)

exp(colMeans(dat1))

exp(dat1)
cor(dat1)
corMat

## now real data
library(tidyverse)

opw<-spread(data=op, key='Age', value='N')
dim(opw)
set.seed(1)
dat1 <- mvrnorm(n = dim(opw)[[1]], mu = mu, Sigma = covMat, empirical = FALSE)
colMeans(dat1)
apply(dat1,2,sd)

dat1<-exp(dat1)

head(opw)
head(dat1)

ophat<-opw[,5:15]*dat1
head(ophat)

newop<-opw

newop[,5:15]<-ophat

head(opw)
head(newop)

colnames(newop) <- c(colnames(newop)[1:4],paste0('A.',formatC(as.numeric(colnames(newop)[5:15]),width=2,flag='0')))
head(newop)

newop<-newop %>% gather(Age,N,A.01:A.11)
head(newop)

a<-filter(newop,Age=='A.01') %>% mutate(Age='A.00',N= -1)
newop<-rbind(newop,a)
fy<-min(newop$Year)
op2<-tapply(newop$N,list(newop$Species.n,newop$Year,newop$Quarter,newop$Age),sum)
op2[1,1,,]

dim(op2)

fi<-'other_pred_n_noise.in'
cat('# N other with noise\n',file=fi)
for (y in (1:dim(op2)[[2]])) {
  cat('# Year:',y+fy-1,'\n',file=fi,append=TRUE)
  write.table(round(op2[1,y,,],1),append=TRUE,file=fi,row.names = FALSE, col.names = FALSE)
}
cat("-999 # Checksum\n",file=fi,append=TRUE)

