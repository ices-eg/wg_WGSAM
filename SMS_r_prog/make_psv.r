
npar<-1472   # North Sea. Take the number from number of parameters in the std file

# read SMS.std 
a<-read.table(file.path(data.path,"sms.std"),comment.char = "#",header=FALSE,skip=1) 
parf<-as.vector(a$V3)
parf<-parf[1:npar]

# write the sms.psv file
zz <- file(file.path(data.path,"sms.psv"), "wb")
writeBin(as.integer(length(parf)),zz)
writeBin(as.double(parf),zz)
close(zz)
