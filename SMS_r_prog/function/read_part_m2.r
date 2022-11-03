#Function to read a stomach related data
Read.part.M2.data<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'partial_m2.out')
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Predator.no],Prey=sp.other.names[s$Prey.no+1],s)
}


Read.part.M2.prediction.data<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'partial_m2_prediction.out')
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Predator.no],Prey=sp.other.names[s$Prey.no+1],s)
}


Read.part.M2.OP.prediction.data<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'op_part_m2.out')
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Predator.no],Prey=sp.other.names[s$Prey.no+1],s)
}
