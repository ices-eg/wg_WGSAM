#Function to read options for HCR 
Read.HCR.options<-function() 
{
file<-file.path(data.path,'HCR_options.out') 
s<-read.table(file,header=TRUE)
Read.HCR.options<-data.frame(Species=name[s$Species.n+1],s)
}
