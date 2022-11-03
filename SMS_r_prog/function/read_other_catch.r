#Function to read a other predator catch
Read.other.catch<-function(dir=data.path,read.init.function=TRUE)
{
  if (read.init.function) Init.function()
  file<-file.path(dir,'other_catch.in')
  s<-scan(file,comment.char='#',quiet=TRUE)
  ny<-SMS.control@last.year-SMS.control@first.year+1
  b<-data.frame(Species.n=rep(1:(first.VPA-1),each=ny),Year=rep(SMS.control@first.year:SMS.control@last.year),  catch=head(s,-1))
  data.frame(b,Species=sp.names[b$Species.n])
}


