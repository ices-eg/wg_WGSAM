#Function to read a stomach related data
Read.stomach.data<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'summary_stom.out')
  s<-read.table(file,header=TRUE)
  Read.stomach.data<-data.frame(Predator=sp.names[s$Predator.no],Prey=sp.other.names[s$Prey.no+1],Quarter=paste("Q",s$Quarter.no,sep=""),s,stan.residual=s$Residual/sqrt(s$Stom.var ) )
}

#Function to read a stomach related data
Read.stomach.data.start<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'summary_stom_start.out')
  s<-read.table(file,header=TRUE)
  Read.stomach.data<-data.frame(Predator=sp.names[s$Predator.no],Prey=sp.other.names[s$Prey.no+1],Quarter=paste("Q",s$Quarter.no,sep=""),s)
}
