#Function to read summary data
Read.objective.function<-function(dir=data.path,extend=FALSE,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'objective_function.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}
