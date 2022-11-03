#Function to read summary data
Read.summary.data<-function(dir=data.path,infile='summary.out',extend=FALSE,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  if (!extend) s<-subset(s,Z>-1)
  data.frame(Species=sp.names[s$Species.n],s)
}


#Function to read prices at age
Read.price<-function(dir=data.path,infile='op_price.in')
{
  Init.function()
  file<-file.path(dir,infile)
  s<-matrix(scan(file=file,comment.char = "#" ),nrow=nsp-first.VPA+1,byrow=T)
  dimnames(s)<-list(sp.names[first.VPA:nsp],SMS.control@first.age:SMS.control@max.age.all) 
  s<-arr2dfny(s,name='Price')
  names(s)<-c('Species','Age','Price')
  s
}


Read.summary.table<-function(dir=data.path,infile='summary_table_raw.out',read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,infile)
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}



#Function to read summary data by areas
Read.summary.data.areas<-function(dir=data.path,extend=FALSE,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'summary_areas.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}


#Function to read summary data
Read.summary.MCMC.data<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'summary_MCMC.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}


#Function to read summary data
Read.mean.weights<-function(dir=data.path)
{
  file<-file.path(dir,'mean_weights.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}


#Function to read summary data
Read.mean.weights.OP<-function(dir=data.path)
{
  file<-file.path(dir,'op_mean_weights.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$Species.n],s)
}


#Function to read size pref
Read.size.preference<-function(dir=data.path)
{
  file<-file.path(dir,'size_pref.out')
  s<-read.table(file,header=TRUE)
  data.frame(Species=sp.names[s$species.n],s)
}
