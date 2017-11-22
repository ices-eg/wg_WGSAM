

file<-file.path(data.path,'sms.psv')
zz <- file(file, "rb")
nParm<-readBin(zz, integer(), 1)
parm<-t(matrix(readBin(zz, numeric(), 1E6),nrow=nParm))
  
summary(parm[,1])

plot(parm[,12])

cor(parm,parm)

#test for normality, lower p value indicates larger deviation from normal distribution 
a<-apply(parm,2,shapiro.test)
p.val<-rep(1,length(a))
for (i in (1:length(a))) p.val[i]<-unlist(a[[i]][2])
p.val<-data.frame(nr=1:length(a),p.val)

index<-order(p.val$p.val)
p.val<-p.val[index,]

dev<-"print"
dev<-"screen"
nox<-5; noy<-5;
noxy<-nox*noy

newplot(dev,nox,noy);
par(mar=c(3,4,3,2))

i<-0

for (n in (1:25)) {
  if (i==noxy) {newplot(dev,nox,noy);par(mar=c(3,4,3,2)); i<-0 }
hist(parm[,p.val[n,1]],main=paste("parameter:",p.val[n,1]),xlab="")
  i<-i+1
}

for (n in (26:50)) {
  if (i==noxy) {newplot(dev,nox,noy);par(mar=c(3,4,3,2)); i<-0 }
hist(parm[,p.val[n,1]],main=paste("parameter:",p.val[n,1]),xlab="")
  i<-i+1
}
