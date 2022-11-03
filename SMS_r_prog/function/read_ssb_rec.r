#Function to read parameters for SSB recruit relation model
Read.SSB.Rec.data<-function(dir=data.path,read.init.function=TRUE)
{
   if (read.init.function) Init.function()
   file<-file.path(dir,'ssb_r.out')
   s<-read.table(file,header=TRUE)
   data.frame(Species=sp.names[s$Species.n],s)
}

#Function to read parameters for SSB recruit relation model
Read.SSB.Rec.data.in<-function(dir=data.path,read.init.function=TRUE)
{
   if (read.init.function) Init.function()
   file<-file.path(dir,'ssb_in.in')
   s<-read.table(file,header=FALSE)
   names(s)<-c("model","alfa","beta","std")
   s
}

Read.Rec_scale.out<-function(dir=data.path,read.init.function=TRUE)
{
   if (read.init.function) Init.function()
   file<-file.path(dir,'rec_scale.out')
   s<-read.table(file,header=TRUE)
   s<-data.frame(Species=sp.names[s$species],Species.n=s$species,rec_scale=s$rec_scale)
   return(s)
}
