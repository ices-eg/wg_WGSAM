#Open psv file
fil<-file(file.path(data.path,"sms.psv"), "rb")

#Read dimension of vector = number of parameters
dim<-readBin(fil,integer(),1)
dim

#read values, assume a very high number of observations 
psv<-readBin(fil, numeric(), 1E6)
close(fil)

# calc number of MCMC set
dim2<-length(psv)/dim

#reformat
psv<-matrix(psv,  dim,dim2)

CV<-round(abs(apply(psv,1,sd)/apply(psv,1,mean)*100))


a<-read.table(file.path(data.path,"sms.std"),comment.char = "#",header=FALSE,skip=1)
tmp<-data.frame(index=a$V1,name=a$V2, mean=a$V3, CV.round=round(a$V4/a$V3*100), std=a$V4)
print(tmp)


  
cleanup()
dev<-"screen"
nox<-4
noy<-4
newplot(dev,nox,noy);
 
j<-1
first<-600
last<-789

for (i in (first:last)) {
  j<-j+1
  if (j==noxy) {newplot(dev,nox,noy); j<<-0 }
  plot(density(psv[i,]),main=paste(i,tmp[i,'name']))
#   plot((psv[i,]),main=paste("par:",i),type='l',ylab='')

}

