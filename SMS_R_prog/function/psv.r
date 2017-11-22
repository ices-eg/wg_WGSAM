#Open psv file
fil<-file(paste(data.path,"sms.psv",sep=""), "rb")

#Read dimension of vector = number of parameters
dim<-readBin(fil,integer(),1)

#read values, assume a very high number of observations 
psv<-readBin(fil, numeric(), 1E6)

# calc number of MCMC set
dim2<-length(psv)/dim
#reformat
psv<-matrix(psv,  dim,dim2)

par(mfcol=c(3,4))

for (i in seq(590,601)) plot(density(psv[i,]),main=paste("par:",i))

for (i in seq(590,601)) plot((psv[i,]),main=paste("par:",i),type='l',ylab='')
