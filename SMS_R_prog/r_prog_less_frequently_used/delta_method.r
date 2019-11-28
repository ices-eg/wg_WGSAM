read.fit<-function(file){
  #
  # Function to read a basic AD Model Builder fit.
  #
  # Use for instance by:
  #
  #   simple.fit <- read.fit('c:/admb/examples/simple')
  #
  # Then the object 'simple.fit' is a list containing sub-objects
  # 'names', 'est', 'std', 'cor', and 'cov' for all model
  # parameters and sdreport quantities.
  #
  ret<-list()
  parfile<-as.numeric(scan(paste(file,'.par', sep=''),what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parfile[1])
  ret$nlogl<-parfile[2]
  ret$maxgrad<-parfile[3]
  file<-paste(file,'.cor', sep='')
  lin<-readLines(file)
  ret$npar<-length(lin)-2
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
  ret$cor<-matrix(NA, ret$npar, ret$npar)
  corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
  ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
  ret$cov<-ret$cor*(ret$std%o%ret$std)

  dim<-as.numeric(scan('admodel.dep', what='', n=2, quiet=TRUE))
  ret$dep<-matrix(as.numeric(scan('admodel.dep', what='', n=dim[1]*dim[2]+2, quiet=TRUE))[-c(1,2)],
                  byrow=TRUE, nrow=dim[2], ncol=dim[1])
  return(ret)
}

setwd(scenario.dir.optim)

fit<-read.fit('OP') 

if (T) { 
  
  dim<-as.numeric(scan('admodel.dep', what='', n=2, quiet=TRUE))   #the derivatives of dependent variables with respect to the independent variables
  
  dep<-matrix(as.numeric(scan('admodel.dep', what='', n=dim[1]*dim[2]+2, quiet=TRUE))[-c(1,2)],
                  byrow=T, nrow=dim[2], ncol=dim[1])
                  
  dimnames(dep)[[1]]<- c( paste(rep(sp.names[first.VPA:nsp],4),fit$names[(fit$nopar+1):fit$npar]))  
  dimnames(dep)[[2]]<- paste(sp.names[first.VPA:nsp],'F')              
  print(round(dep,0))
  
  condensed<-read.table('OP_condensed.out',header=TRUE)
  condensed<-droplevels(subset(condensed,Year>=2016))
  totYield<-tapply(condensed$yield,condensed$Species.n,mean)
  cat("\nAverage Yield\n")
  print( round(totYield,0))
   
   
  avg_F<-tapply(condensed$Fbar,list(condensed$Species.n),mean)
  cat("\nAverage F\n")
  print(round(avg_F,3))
 } 

  
#fit<-read.fit('SMS')                                        
S<-fit$cov[1:fit$nopar,1:fit$nopar]
A<-fit$dep

cov.sdrep<-A %*% S %*% t(A)  # delta methode

# est of std of sd-report  variables
delta.std<-(sqrt(diag(cov.sdrep)))
adm.std<-fit$std[(fit$nopar+1):fit$npar]

cat("\n\n")
cat("adm.std  :",round(adm.std,1),'\n')
cat("delta.std:",round(delta.std,1),'\n')
cat("ratio    :",round(adm.std/delta.std,2),'\n')
setwd(data.path)
