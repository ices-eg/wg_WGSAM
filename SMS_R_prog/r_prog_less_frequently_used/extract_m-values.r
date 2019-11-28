
sumQuarerly<-T  # use sum of quarterly M2, or calc M2 from annual numbers dead.
first.year.on.plot<-1974
last.year.on.plot<-2050

if (sumQuarerly) { # calc M2 as sum of quarterly M2
     a<-Read.summary.data(read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & M2>=0),
               select=c(Species, Year,Age,M2,M1) ,drop=T)
  values<-tapply(a$M1+a$M2,list(a$Year,a$Age,a$Species),sum)
    values2<-tapply(a$M1+a$M2,list(a$Species,a$Year,a$Age),sum)
    
} else {  # calc M2 an an annual basis
 
     a<-Read.summary.data(read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & Z>0))
     a<-data.frame(Year=a$Year, Species=a$Species, Age=a$Age,Z=a$Z,
           dead=a$N.bar*a$Z,deadm=a$N.bar*(a$M2+a$M1))
 

  list.ZF<- list(a$Year,a$Age,a$Species)
  dead<-tapply(a$dead,list.ZF,sum,na.rm=T)
  deadM<-tapply(a$deadm,list.ZF,sum)
  Z<-tapply(a$Z,list.ZF,sum)
  values2<-deadM/dead*Z
}

round(values,3) 
ftable(round(values2,3))
