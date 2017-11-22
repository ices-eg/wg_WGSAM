#Function to read a other predator biomass

Read.other.predator<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'other_predators.out')
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Species.n],s)
}


Read.other.predator.prediction<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'other_predator_prediction.out')
  s<-read.table(file,header=TRUE)
  data.frame(Predator=sp.names[s$Species.n],s)
}
