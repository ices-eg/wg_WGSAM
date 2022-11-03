years<-my.years
#update length classes files (assuming that additional years have the same length classes as the final year in the not updated set)


for (ds in alldataSets) {  
  l<-read_delim(file=file.path(root,exchangeDir,paste0("length_classes_",ds,".csv")),delim=',')  

  oldly<-max(l$year)
  newyears<-years[years>oldly]
  if (length(newyears)>0) {
    d<-filter(l,year==oldly)
    for (y in newyears) {
      d$year<-y
      l<-rbind(l,d)
    }
    write.csv(l,file =file.path(root,exchangeDir,paste0("length_classes_",ds,".csv")),row.names = FALSE )
  }
}