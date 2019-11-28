


zz <- file(file.path(data.path,"mcout_SSB.bin"), "rb")
a<-readBin(zz, numeric(),1000)
close(zz)
matrix(a,ncol=5)