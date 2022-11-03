## This file contains useful functions for interacting with ADMB's
## hessian and covariance files, as well as how to recreate some of
## the calculations.

## A lot of this is in progress and thus somewhat untested. If you find any
## mistakes please let me know.

## Cole Monnahan | monnahc@uw.ed | 9/2012

writeADMBCovariance <- function(correlation.user, directory=getwd()){
  ## This function takes a user specified correlation matrix and
  ## writes it to admodel.hes. Note that this requires a current
  ## admodel.hes file to be in the directory specified. This
  ## function currently doesn't handle not positive definite
  ## Hessians. Cole Monnahan | monnahc@uw.edu | 9/2012
  
  ## Make a backup of the original
  temp <- file.exists(paste(directory, "/admodel.cov", sep=""))
  if(!temp) stop(paste("Couldn't find file ",directory, "/admodel.cov", sep=""))
  temp <- file.copy(from=paste(directory, "/admodel.cov", sep=""),
                    to=paste(directory, "/admodel_original.cov", sep=""))
  
  ## Read in the output files
  results <- getADMBHessian()
  hes <- results$hes    # the unbounded Hessian
  cov <- solve(hes)                # unbounded Covariance
  scale <- results$scale
  num.pars <- results$num.pars
  cov.bounded <- cov *(scale %o% scale)  # the bounded Cov
  se <- sqrt(diag(cov.bounded))
  
  ## Create new covariance matrix
  cor <- correlation.user
  if(nrow(cor) != num.pars | ncol(cor) != num.pars)
    stop(paste("Invalid size of correlation matrix, should be:", num.pars,
               "by",num.pars))
  cov.bounded <- cor * (se %o% se)
  cov.unbounded <- cov.bounded/(scale %o% scale)
  
  ## Write it back to file, note need to write the .cov *not* the .hes file.
  ## temp <- file.remove(paste(directory, "/admodel.cov", sep=""))
  ## if(!temp) stop("Couldn't delete admodel.cov")
  file.new <- file(paste(directory, "/admodel.cov", sep=""),"wb")
  on.exit(close(file.new))
  writeBin(as.integer(num.pars), con=file.new)
  writeBin(as.vector(as.numeric(cov.unbounded)), con=file.new)
  writeBin(as.integer(results$hybrid_bounded_flag), con=file.new)
  writeBin(as.vector(scale), con=file.new)
}




## ------------------------------------------------------------
## truncate or relax the high correlations
RelaxADMBCovariance <- function(model.name, directory=getwd(), cor.max=.9,
                                make.plot=TRUE){
  ## This function takes the outputted covariance file from ADMB, renames it
  ## for posterity, reads it in, trunctates covariances (the off-diagonals),
  ## and then reads this new covariance matrix to the original default ADMB
  ## file name. For use in manipulating the Cov used by the MVN jump function
  ## for MCMCs. Experimental still! Cole Monnahan | monnahc@uw.edu | 9/2012
  
  ## Make a backup of the original
  temp <- file.exists(paste(directory, "/admodel.cov", sep=""))
  if(!temp) stop(paste("Couldn't find file ",directory, "/admodel.cov", sep=""))
  temp <- file.copy(from=paste(directory, "/admodel.cov", sep=""),
                    to=paste(directory, "/admodel_original.cov", sep=""))
  
  
  ## Read in the output files and calculate correlation matrix
  results <- getADMBCovariance()          # the unbounded Cov
  cov <- results$cov
  scale <-getADMBHessian()$scale
  cov.true <- cov *(scale %o% scale)  # the bounded Cov
  se <- sqrt(diag(cov.true))
  cor <- cov.true/(se %o% se)         # bounded Cor, should match .cor file
  num.pars <- results$num.pars
  res <- read.admbFit(model.name)
  if(num.pars != res$nopar){
    stop(".cor and .cov files had different numbers of parameters")
  }
  if(1e-4 < (maxdiff <- max(abs(res$cor[1:num.pars,1:num.pars]-cor)))){
    warning(paste("The largest difference in correlation matrices is:",
                  maxdiff,", are you sure they are from the same model?? They should be very close, unless you've already relaxed this .cov file, then ignore this message."))
  }
  
  ## If desired, plot it
  if(make.plot){
    xx <- 1:num.pars
    param.names <- res$names[xx]
    image(x=xx, y=xx, z=t(cor), axes=FALSE,
          ann=FALSE,zlim=c(-1,1),
          col=colorRampPalette(c("blue","white","red"))(1000))
    axis(1, at=xx, labels=param.names, cex=2)
    axis(2, at=xx, labels=param.names, cex=2)
    box()
    abline(v=c(1:num.pars)+.5, h=c(1:num.pars)+.5)
    grid <- expand.grid(xx,rev(xx))
    cor.vec <- round(as.vector(t(cor[rev(xx),])),2)
    text(x=grid[,1], y=grid[,2], labels=cor.vec, cex=2)
  }
  
  ## Create relaxed Correlation
  cor.relaxed <- cor
  cor.relaxed[cor > cor.max] <- cor.max
  cor.relaxed[cor < -cor.max] <- -cor.max
  diag(cor.relaxed) <- 1
  cov.relaxed.true <- cor.relaxed * (se %o% se)
  cov.relaxed <- cov.relaxed.true/(scale %o% scale)
  
  ## Write it back to file
  temp <- file.remove(paste(directory, "/admodel.cov", sep=""))
  if(!temp) stop("Couldn't delete admodel.cov")
  file.new <- file(paste(directory, "/admodel.cov", sep=""),"wb")
  writeBin(as.integer(num.pars), con=file.new)
  writeBin(as.vector(as.numeric(cov.relaxed)), con=file.new)
  writeBin(as.integer(results$hybrid_bounded_flag), con=file.new)
  writeBin(as.vector(scale), con=file.new)
  close(file.new)
}
## ------------------------------------------------------------

## ------------------------------------------------------------
## The bounded transformation(s)
boundp <- function(x, minb, maxb, hbf){
  ## The internal transformations used in ADMB depending on the value of the
  ## Hybrid_bounded_flag (hbf) value.
  if(hbf==1)
    result <- minb+(maxb-minb)/(1+exp(-x))
  else if(hbf==0)
    result <- minb+(maxb-minb)*(.5*sin(x*pi/2)+.5)
  else stop("Invalid hbf value, should be 0 or 1")
  return(result)
}

boundpin <- function(x, minb, maxb, hbf) {
  ## The inverse of the transformation
  if(hbf==1)
    result <- -log( (maxb-x)/(x-minb) )
  else if(hbf==0)
    result <- asin(2*(x-minb)/(maxb-minb)-1)/(pi/2)
  else stop("Invalid hbf value, should be 0 or 1")
  return(result)
}

ndfboundp <- function(x, minb, maxb, hbf) {
  ## The derivative used to find the "scales"
  if(hbf==1)
    result <- (maxb-minb)*exp(-x)/(1+exp(-x))^2
  else if(hbf==0)
    result <- (maxb-minb)*.5*pi/2*cos(x*pi/2)
  else stop("Invalid hbf value, should be 0 or 1")
  return(result)
}
## ------------------------------------------------------------


## ------------------------------------------------------------
## Reading the binary files
getADMBCovariance <- function(){
  ## This function reads in all of the information contained in the
  ## admodel.cov file. Some of this is needed for relaxing the
  ## covariance matrix, and others just need to be recorded and
  ## rewritten to file so ADMB "sees" what it's expecting.
  filename <- file("admodel.cov", "rb")
  on.exit(close(filename))
  num.pars <- readBin(filename, "integer", 1)
  cov.vec <- readBin(filename, "numeric", num.pars^2)
  cov <- matrix(cov.vec, ncol=num.pars, nrow=num.pars)
  hybrid_bounded_flag <- readBin(filename, "integer", 1)
  scale <- readBin(filename, "numeric", num.pars)
  result <- list(num.pars=num.pars, cov=cov,
                 hybrid_bounded_flag=hybrid_bounded_flag, scale=scale)
  return(result)
}

getADMBHessian <- function(){
  ## This function reads in all of the information contained in the
  ## admodel.hes file. Some of this is needed for relaxing the
  ## covariance matrix, and others just need to be recorded and
  ## rewritten to file so ADMB "sees" what it's expecting.
  filename <- file("admodel.hes", "rb")
  on.exit(close(filename))
  num.pars <- readBin(filename, "integer", 1)
  hes.vec <- readBin(filename, "numeric", num.pars^2)
  hes <- matrix(hes.vec, ncol=num.pars, nrow=num.pars)
  hybrid_bounded_flag <- readBin(filename, "integer", 1)
  scale <- readBin(filename, "numeric", num.pars)
  result <- list(num.pars=num.pars, hes=hes,
                 hybrid_bounded_flag=hybrid_bounded_flag, scale=scale)
  return(result)
}
## ------------------------------------------------------------

## ------------------------------------------------------------
## Reading the model output files
read.admbFit<-function(model.name){
  ## Function to read AD Model Builder fit from its par and cor
  ## files. I borrowed this from http://qfc.fw.msu.edu/userfun.asp
  ## and made some modifications as I needed. Note that this one
  ## reads the model output files with the model.name prefix, NOT
  ## the admodel.hes or admodel.cov. Thus this is a good way to
  ## check that my correlation calculations are coming out the same.
  
  ret<-list()
  parmodel.name<-as.numeric(scan(paste(model.name,'.par', sep=''),
                                 what='', n=16, quiet=TRUE)[c(6,11,16)])
  ret$nopar<-as.integer(parmodel.name[1])
  ret$nloglike<-parmodel.name[2]      #objective function value
  ret$maxgrad<-parmodel.name[3]
  model.name<-paste(model.name,'.cor', sep='') # read cor model.name
  lin<-readLines(model.name) # total parameter including sdreport variables
  ret$totPar<-length(lin)-2  #log of the determinant of the hessian
  ret$logDetHess<-as.numeric(strsplit(lin[1], '=')[[1]][2])
  sublin<-lapply(strsplit(lin[1:ret$totPar+2], ' '),function(x)x[x!=''])
  ret$names<-unlist(lapply(sublin,function(x)x[2]))
  ret$est<-as.numeric(unlist(lapply(sublin,function(x)x[3])))
  ret$std<-as.numeric(unlist(lapply(sublin,function(x)x[4])))
  ret$cor<-matrix(NA, ret$totPar, ret$totPar)
  corvec<-unlist(sapply(1:length(sublin), function(i)sublin[[i]][5:(4+i)]))
  ret$cor[upper.tri(ret$cor, diag=TRUE)]<-as.numeric(corvec)
  ret$cor[lower.tri(ret$cor)] <- t(ret$cor)[lower.tri(ret$cor)] # covariance matrix
  ret$cov<-ret$cor*(ret$std %o% ret$std)
  return(ret)
}
## ------------------------------------------------------------

if (FALSE) {
  
  ### Mortens code
  hes<-getADMBHessian()
  
  scale<-hes$scale
  
  cov<-solve(hes$hes)
  cov.bounded<-cov*(scale %o% scale)
  
  
  se<-sqrt(diag(cov.bounded))
  se
  
  cor<-cov.bounded/(se %o% se)
  cor[]
}