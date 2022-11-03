a<-Read.ALK.stom()
head(a)
a$N<-a$proportion.adjusted*a$Nbar
a$yq<-paste0(a$year,'-Q',a$quarter)

tst<-subset(a,year==1987 & quarter==2)
subset(tst,N==0)
tapply(tst$N,list(tst$Species,tst$LengthGroup),sum)

NbyLength<-tapply(a$N,list(a$yq,a$Species,a$LengthGroup),sum)
any(NbyLength==0) # There should be none

ftable(round(NbyLength,0))
