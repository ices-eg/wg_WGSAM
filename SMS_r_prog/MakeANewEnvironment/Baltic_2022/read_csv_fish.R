read.csv.fish<-function (filen)  {
 # filen<-'Catch-numbers-at-age.csv'
 
  hn<-readLines(filen)[1]
  hn<-strsplit(hn,',')[[1]]
  hn<-suppressWarnings(as.numeric(hn))
  hnn<-!is.na(hn)
  hn<-hn[hnn]
  
  a<-read.csv(filen,check.names = FALSE)
  years<-a[,1]
  a<-a[,hnn]
  a<-as.matrix(a)
  rownames(a)<-years
  # colnames(a)<-hn
  a
}

