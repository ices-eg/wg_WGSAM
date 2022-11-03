########################## read hand-picked years used for SSB/Recruitment  #############
read_recruit_years<-function(dir=data.path,read.init.function=FALSE) {
  file<-file.path(dir,'recruitment_years.in')
  if (read.init.function) Init.function(dir)
  
  a<-head(scan(file=file.path(file),comment.char='#'),-1)
  
  nVPA<-nsp-first.VPA+1
  years<-seq(SMS.control@first.year,SMS.control@last.year)
  
  a<-data.frame(used=(a==1),Year=rep(years,nVPA),Species=rep(sp.names[first.VPA:nsp],each=length(years)),
                Species.n=rep(first.VPA:nsp,each=length(years)))
  return(a)
}

