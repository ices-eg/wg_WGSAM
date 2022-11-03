#Function to read summary data
Read.objective.function<-function(dir=data.path,extend=FALSE,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'objective_function.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}

Read.objective.function.fleet<-function(dir=data.path,extend=FALSE,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'objective_function_fleet.out')
  s<-read.table(file,header=TRUE)
  a<-data.frame(Species=sp.names[s$Species.n],s)
  fleet.names<-Read.fleet.names()
  fleet.names<-sub(" +$", "", fleet.names)  ## spaces only
  fleet.names<-fleet.names[fleet.names!='']
  a$fleetName<-fleet.names
  
  return(a)
}
