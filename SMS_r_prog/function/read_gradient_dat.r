#Read gradiet.dat file

Read.gradient.dat<-function(dir=data.path,gfile="gradient.dat"){
    a<-read.table(file.path(dir,gfile),header=TRUE)
    a$ParName<-as.character(a$ParName)
    a$no<-1:dim(a)[[1]]
    
 
    
  return(a)

}
