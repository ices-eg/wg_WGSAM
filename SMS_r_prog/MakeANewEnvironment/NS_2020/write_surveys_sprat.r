
surv.file<-file.path(finalExchangeDir,paste0('fleet_catch.in'))
surv.name.file<-file.path(finalExchangeDir,paste0('fleet_names.in'))

cat('#\n',file=surv.file)
cat('',file=surv.name.file)
write.surv<-function(sp='COD',effort=1E-04,nr=1,digits=3) {
  x<-all.surveys[[sp]]
  if (nr==1) cat('###   ',sp,'  #####################################################\n',file=surv.file,append=TRUE)
  surv.name<-names(x)[nr]
  surv.name<-sub("\t+$", "", surv.name)  ## 
  cat(paste(sp,surv.name,sep='_'),'\n',file=surv.name.file,append=TRUE)
  x<-x[[nr]]
  dims<-dimnames(x)
  cat("## ",sp,'  fleet;',nr,'  ',surv.name," age ",min(dims[[2]]),'-',max(dims[[2]]),' timing:',attr(x,'time'),', ',dims[[1]][1],'-',tail(dims[[1]],1),'\n',file=surv.file,append=TRUE)
  for (y in dimnames(x)[[1]]) {
    cat(effort,formatC(x[y,],digits=digits, width=12,format='f'),paste0('   #',y,'\n'),file=surv.file,append=TRUE) 
  }
}

write.surv(sp='SPR',nr=1,effort=1E-04,digits=0)
write.surv(sp='SPR',nr=2,effort=1E-04,digits=0)
write.surv(sp='SPR',nr=3,effort=1E-04,digits=0)


a<-readLines(surv.name.file)
a<-formatC(a,format='s',flag='-',width=26)
a<-gsub(' ','_',a)
writeLines(a,surv.name.file)
cat("12345678901234567890123456\n",file=surv.name.file,append=TRUE)
cat('Please note, exactly 26 caharacters in fleet names, space as "_"\n',file=surv.name.file,append=TRUE)

