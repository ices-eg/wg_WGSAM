#  this script calculates the mean M (=M1+M2) over all years and write it to a file
dat<-Read.summary.data(read.init.function=T)
dat$M<-dat$M1+dat$M2
dat<-droplevels(subset(dat,Species.n>=first.VPA))

if (T) {  # average M over all years
  M<-tapply(dat$M,list(dat$Quarter,dat$Age,dat$Species.n),mean)
  M[is.na(M)]<- -1
  
  
  outf<-file.path(data.path,'natmor_avg.in')
  cat('# single species from average multispecies M1+M2 over the full period\n',file=outf)
  
  for (sp in (first.VPA:nsp)) {
    for (y in (SMS.control@first.year:SMS.control@last.year )) {
    
    cat(paste('# ',sp.names[sp],y,'\n'),file=outf,append=T)
    write.table(round(M[,,as.character(sp)],3),append=T,file=outf,row.names = F,col.names = F)
  }
  }
} 

if (T) { # M by year
  M<-tapply(dat$M,list(dat$Year,dat$Quarter,dat$Age,dat$Species.n),sum)
  M[is.na(M)]<- -1
  
    outf<-file.path(data.path,'natmor_by_year.in')
  cat('# single species from average multispecies M1+M2\n',file=outf)
  
  for (sp in (first.VPA:nsp)) {
    for (y in (SMS.control@first.year:SMS.control@last.year )) {
      
      cat(paste('# ',sp.names[sp],y,'\n'),file=outf,append=T)
      write.table(round(M[as.character(y),,,as.character(sp)],3),append=T,file=outf,row.names = F,col.names = F)
    }}    
}
cat(-999," # checksum",file=outf,append=T)
