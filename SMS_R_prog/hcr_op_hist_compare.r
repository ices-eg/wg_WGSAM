
file.name<-'ICES stock summary sum'             # graphical output file if paper<-TRUE

first.year<-2004              #first year on plot, negative value means value defined by data
last.year<- 2013                 #last year on plot

##########################################################################


ref<-Read.reference.points.OP()
Init.function()
Blim<-as.vector(ref[,'Blim'])
Bpa<-as.vector(ref[,'Bpa'])
Fpa<-as.vector(ref[,'Fpa'])

a<-subset(Read.summary.data(), select=c(Species.n,Species,Year,Age,Yield))
price<-Read.price()
a<-merge(a,price)
a$Value<-a$Price*a$Yield
a<-aggregate(Value~Species.n+Species+Year,data=a,sum)

b<-Read.summary.table()
b<-merge(a,b)

b<-droplevels(subset(b,Species !='Plaice' & Species !='Sole' & Year>=first.year & Year<=last.year))
b$belowBlim<-ifelse(b$SSB<Blim[b$Species.n-first.VPA+1],1,0)
b$belowBpa<-ifelse(b$SSB<Bpa[b$Species.n-first.VPA+1],1,0)
b$aboveFpa<-ifelse(b$mean.F>Fpa[b$Species.n-first.VPA+1],1,0)
 

minY<-min(b$Year)
maxY<-max(b$Year)
ny<-maxY-minY+1
  
  a<-aggregate(cbind(Yield,SOP,Value,SSB,mean.F)~Species.n+Species,mean,data=b)

  aa<-aggregate(cbind(belowBlim,belowBpa,aboveFpa)~Species.n+Species,sum,data=b)
  
  aa$belowBlim<- aa$belowBlim/ny*100
  aa$belowBpa<- aa$belowBpa/ny*100
  aa$aboveFpa<- aa$aboveFpa/ny*100
  
  aa<-merge(a,aa)
  aa$labels<-paste(aa$Species.n-first.VPA+1,aa$Species)
   
  a<-cbind(
    t(t(tapply(aa$Yield,aa$labels,sum)/1000)),
    t(t(tapply(aa$SOP,aa$labels,sum)/1000)),
    t(t(tapply(aa$Value,aa$labels,sum)/1000)),
    t(t(tapply(aa$SSB,aa$labels,sum)/1000)) ,
     t(t(tapply(aa$mean.F,aa$labels,sum))),
    t(t(tapply(aa$belowBlim,aa$labels,sum))),
    t(t(tapply(aa$belowBpa,aa$labels,sum))),
    t(t(tapply(aa$aboveFpa,aa$labels,sum))))
  
  all<-colSums(a) 
  all[5:8]<-NA   
  
  a<-rbind(a,all)
      
  my.dimnames<-c('Yield','Catch','Value','SSB','Fbar','SSB below Blim','SSB below Bpa','F above Fpa') 
  
  dimnames(a)[[2]]<-my.dimnames 
  
  print(a)
  my.units<-c('(kt)','(kt)','(m Euro)','(kt)',' ','(%)','(%)','(%)')        
  my.dec<-c(0,0,0,0,2,0,0,0)

  xtab(a, caption='', cornername='  ',
             file=file.path(paste('hist_performance_',first.year,'-',last.year,'.html',sep='')),
             dec=my.dec, width='"100%"',units=my.units)

 