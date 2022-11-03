
 my.data.path<-file.path(data.path,"HCR_1_deter_01_HCR1_0_Rec0__2051")


 
load( file =file.path(my.data.path, "a.RData"))
head(a)
dim(a)
a<-droplevels(subset(a,COD<=0.6 & WHG<=0.5 & SAN<=0.6 & NOR<=0.6 & SPR<=1.0))
dim(a)
save(a, file =file.path(my.data.path, "a.RData"))
rm(a)

  


load( file =file.path(my.data.path, "condensed.RData"))
head(condensed)
dim(condensed)
condensed<-droplevels(subset(condensed,COD<=0.6 & WHG<=0.5 & SAN<=0.6 & NOR<=0.6 & SPR<=1.0))
dim(condensed)
save(condensed, file =file.path(my.data.path, "condensed.RData"))
rm(condensed)


load( file =file.path(my.data.path, "Fcomb.RData"))
head(Fcomb)
dim(Fcomb)
Fcomb<-droplevels(subset(Fcomb,COD<=0.6 & WHG<=0.5 & SAN<=0.6 & NOR<=0.6 & SPR<=1.0))
dim(Fcomb)
save(Fcomb, file =file.path(my.data.path, "Fcomb.RData"))
rm(Fcomb)


load( file =file.path(my.data.path, "indicators.RData"))
head(indi)
dim(indi)
indi<-droplevels(subset(indi,COD<=0.6 & WHG<=0.5 & SAN<=0.6 & NOR<=0.6 & SPR<=1.0))
dim(indi)
save(indi, file =file.path(my.data.path, "indicators.RData"))
rm(indi)



