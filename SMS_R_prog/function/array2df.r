
arr2df <- function(arr) {
  if(is.null(dimnames(arr))){dimnames(arr)<-lapply(dim(arr), FUN=function(x)1:x)}
  dn <- dimnames(arr)
  #if (any(unlist(lapply(dn,is.null)))) stop('Length of dimnames must equal length of dimension.')
  for (i in 1:length(dim(arr))) if (is.null(dn[[i]])) dn[[i]]<-as.character(1:(dim(arr)[i]))
  if(is.null(names(dn))){names(dn)<-paste('index', 1:length(dim(arr)), sep=".")}
  ans <- cbind(expand.grid(dn), stomobs=as.vector(arr))
    return(as.data.frame(ans))
}


arr2dfny <- function(arr,name='y') {
  if(is.null(dimnames(arr))){dimnames(arr)<-lapply(dim(arr), FUN=function(x)1:x)}
  dn <- dimnames(arr)
  #if (any(unlist(lapply(dn,is.null)))) stop('Length of dimnames must equal length of dimension.')
  for (i in 1:length(dim(arr))) if (is.null(dn[[i]])) dn[[i]]<-as.character(1:(dim(arr)[i]))
  if(is.null(names(dn))){names(dn)<-paste('index', 1:length(dim(arr)), sep=".")}
  ans <- cbind(expand.grid(dn),as.vector(arr))
  colnames(ans)<-c(colnames(ans)[-ncol(ans)],name)
    return(as.data.frame(ans))
}
