
read.fit<-function(baseF='sms'){
  # Code modified from Anders Nielsen's SAM code

  # Function to read a basic fit
  
  ret<-list()

  parfile<-as.numeric(scan(file.path(data.path,paste(baseF,".par",sep='')),
                      what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nlogl<-parfile[2]
  ret$maxgrad<-parfile[3]

  lin<-readLines(file.path(data.path,paste(baseF,".cor",sep='')))
  ret$npar<-length(lin)-2
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))

  ret$cor<-matrix(NA, ret$npar, ret$npar)
  for(i in 1:ret$npar){
    ret$cor[1:i,i]<-as.numeric(unlist(lapply(sublin[i],
      function(x)x[5:(4+i)])))
    ret$cor[i,1:i]<-ret$cor[1:i,i]
  }
  ret$cov<-ret$cor*(ret$std%o%ret$std)

  b<-read.table(file.path(data.path,"par_exp.out"),comment.char = "#",header=T)
  ret$species<-b$species
  ret$year<-b$year
  ret$quarter<-b$quarter
  ret$age<-b$age
  ret$predator<-b$predator
  ret$prey<-b$prey
  ret$fleet<-b$fleet
  

  mslh<-function(name){
    idx<-which(ret$names==name)
      x<-cbind(ret$est[idx], ret$std[idx], ret$est[idx]-2*ret$std[idx],
             ret$est[idx]+2*ret$std[idx])
    colnames(x)<-c('est', 'std', 'low', 'hig')
    return(x)
  }
  ret$ssb<-mslh('hist_SSB')
  return(ret)
}
