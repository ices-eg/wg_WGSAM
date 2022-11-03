#  this script create the n_proportion_M2.in file with  "1" values 
dat<-Read.summary.data(read.init.function=T)
dat<-droplevels(subset(dat,Species.n>=first.VPA))


M<-tapply(dat$M,list(dat$Quarter,dat$Age,dat$Species.n),mean)  # template
M[]<- 1 


outf<-file.path(data.path,'n_proportion_M2.in')
cat('# Proportion of N used for calculation of M2 values\n',file=outf)

for (sp in (first.VPA:nsp)) {
  for (y in (SMS.control@first.year:SMS.control@last.year )) {
  
  cat(paste('# ',sp.names[sp],y,'\n'),file=outf,append=T)
  write.table(round(M[,,as.character(sp)],3),append=T,file=outf,row.names = F,col.names = F)
 }
}

cat(-999," # checksum",file=outf,append=T)
