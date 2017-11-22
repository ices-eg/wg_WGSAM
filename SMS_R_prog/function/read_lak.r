#Function to read Length Age Key (LAK)
Read.LAK<-function(dir=data.path,read.init.function=FALSE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'LAK.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.no],s)
}
