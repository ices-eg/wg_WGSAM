baseF<-file.path('H:','Bootsmave','SAS-Baltic')
a<-readLines(con=file.path(baseF,'Stombalt_oldData','balt_stomNO.dat'))

b<-readLines(con=file.path(baseF,'Bootsmave_old','balt_stom.dat'))

head(a)
substr
head(b)

aa<-unlist(lapply(a,function(x) substr(x,3,200)))
head(a)
head(aa)
writeLines(aa,con=file.path(baseF,'a.dat'))
bb<-unlist(lapply(b,function(x) substr(x,3,200)))
head(b)
head(bb)
writeLines(bb,con=file.path(baseF,'b.dat'))