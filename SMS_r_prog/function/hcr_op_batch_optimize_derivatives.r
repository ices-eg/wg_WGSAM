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
  ret$npar<-0   # initial value
  file<-paste(file,'.cor', sep='')
  if (file.exists(file)) if (file.info('op.cor')$size>1) {
    lin<-readLines(file)
    ret$npar<-length(lin)-2
    #ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
    sublin<-lapply(strsplit(lin[1:ret$npar+2], ' '),function(x)x[x!=''])
    ret$names<-unlist(lapply(sublin,function(x)x[2]))
    ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
    ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
    ret$cor<-matrix(NA, ret$npar, ret$npar)
    corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
    ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
    ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)]
    ret$cov<-ret$cor*(ret$std%o%ret$std)
  } else { cat("\n no uncertanties\n"); return(ret) }
  dim<-as.numeric(scan('admodel.dep', what='', n=2, quiet=TRUE))
  ret$dep<-matrix(as.numeric(scan('admodel.dep', what='', n=dim[1]*dim[2]+2, quiet=TRUE))[-c(1,2)],
                  byrow=TRUE, nrow=dim[2], ncol=dim[1])
  return(ret)
}

setwd(scenario.dir.optim)

fit<-read.fit('op') 

if (file.exists('admodel.dep')) {
  dim<-as.numeric(scan('admodel.dep', what='', n=2, quiet=TRUE))   #the derivatives of dependent variables with respect to the independent variables
  dep<-matrix(as.numeric(scan('admodel.dep', what='', n=dim[1]*dim[2]+2, quiet=TRUE))[-c(1,2)],byrow=T, nrow=dim[2], ncol=dim[1])
  
  if (fit$npar==0) std.avail<-F else std.avail<-T
  if (!std.avail) {   # no std file available
    fit$npar<-dim[[2]]+dim[[1]]
    nam<-read.table(file='par_exp.out',header=T,sep=' ')
    fit$names<-nam$par
  }
  n.sdvars<-length(unique(fit$names[(fit$nopar+1):fit$npar]))  # number of sd variable types
                 
  dimnames(dep)[[1]]<- c( paste(rep(sp.names[first.VPA:nsp],n.sdvars),fit$names[(fit$nopar+1):fit$npar]))  
  dimnames(dep)[[2]]<- paste(sp.names[first.VPA:nsp],'F',sep='_')              
  
  sink('_derivatives.txt')
    print(round(dep,0))
  sink()

  n<-dim[[1]]  # number of VPA species
  
  if (!std.avail) {   # no std file available
    condensed<-read.table('op_condensed.out',header=TRUE)
    condensed<-droplevels(subset(condensed,Year>=my.first.year & Year<=my.last.year))
    
    avg_F<-tapply(condensed$Fbar,list(condensed$Species.n),mean)
    #cat("\nAverage F\n"); (round(avg_F,3)); (round(fit$est[1:fit$nopar],3))  # should be the same as above
    
    avg_SSB<-tapply(condensed$SSB,list(condensed$Species.n),mean)
    #cat("\nAverage SSB\n");(round(avg_SSB,3))
    
    avg_yield<-tapply(condensed$yield,list(condensed$Species.n),mean)
    #cat("\nAverage yield\n");(round(avg_yield))
    
    condensed<-droplevels(subset(condensed,Year==my.last.year))
    
    SSB_ly<-tapply(condensed$SSB,list(condensed$Species.n),sum)
    #cat("\nSSB_ly\n");(round(SSB_ly))
     
    yield_ly<-tapply(condensed$yield,list(condensed$Species.n),sum)
    #cat("\nyield_ly\n");(round(yield_ly))
  }
  if (std.avail) {
    i<-0; avg_F<-fit$est[(i*n+1):((i+1)*n)]
    i<-1; avg_yield<-fit$est[(i*n+1):((i+1)*n)]
    i<-2; yield_ly<-fit$est[(i*n+1):((i+1)*n)]    
    i<-3; avg_SSB<- fit$est[(i*n+1):((i+1)*n)]    
    i<-4; SSB_ly<-  fit$est[(i*n+1):((i+1)*n)]     
  }
  
  dep.mean<-matrix(c(rep(avg_yield,n),rep(yield_ly,n),rep(avg_SSB,n),rep(SSB_ly,n)),byrow=T,ncol=n,nrow=4*n)
  rel.change.in.F<-0.10
  
  dep.relative1<- dep/dep.mean 
  dep.relative<- dep/dep.mean *rel.change.in.F*rep(avg_F,each=n*n.sdvars)
  dep.relative.percent<-dep.relative*100
  round(dep.relative.percent,1)
  
  #make a nice table
  NArow<-avg_F
  NArow[]<-NA
  a<-rbind(avg_F,NArow,avg_yield/1000,NArow,dep.relative.percent[1:n,],NArow,avg_SSB/1000,NArow,dep.relative.percent[(n*2+1):(n*3),])
  dimnames(a)[[2]]<-sp.names[first.VPA:nsp]
  
  a<-a[,-grep('Sole',dimnames(a)[[2]])]
  a<-a[,-grep('Plaice',dimnames(a)[[2]])]
  
  a<-a[-grep('Sole',dimnames(a)[[1]]),]
  a<-a[-grep('Plaice',dimnames(a)[[1]]),]
  
  if (!NS.Key.2014) {ndec<-8;spl<-c("--Cod", "--Whiting","--Haddock","--Saithe","--Herring","--Sandeel","--Norway pout","--Sprat")}
  if (NS.Key.2014) {ndec<-9; spl<-c("--Cod", "--Whiting","--Haddock","--Saithe","--Herring","--Northern sandeel","--Southern sandeel","--Norway pout","--Sprat") }
  
  dimnames(a)[[1]]<- c( "F at maximum","  ","Average yield (1000 t) at maximum F", "- Change(%) in Yield by species -" ,spl,
                  " ","Average SSB (1000 t) at maximum F", "- Change (%) in SSB by species -" ,spl)      
  
  xtab3(a, caption=paste("Effects of a",round(rel.change.in.F*100),"% increase in F at maximum based on partial derivatives.  ",ifelse(std.avail,' converged','non convergence'),' max grad:',fit$maxgrad), cornername='  ',
           file=file.path('_derivatives.html'), dec=c(2,0,0,0,rep(1,ndec),0,0,0,rep(1,ndec)), width='"100%"',units=' ')
           
}           
setwd(data.path)

