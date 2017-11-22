#Function to species names
Read.species.names<-function(dir=data.path)
{
  file<-file.path(dir,'Species_names.in')
  s<-readLines(file, n=nsp)
  for (i in (1:10)) s<-sub('_',' ',s)
  s<-sub('[[:space:]]+$', '', s)
  Read.species.names<-c('Other',s)
}

Read.area.names<-function(dir=data.path)
{
  file<-file.path(dir,'Area_names.in')
  s<-readLines(file, n=SMS.control@no.areas)
  for (i in (1:10)) s<-sub('_',' ',s)
  s<-sub('[[:space:]]+$', '', s)
  s
}

