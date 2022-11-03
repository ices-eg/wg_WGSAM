
#library(mgcv)   # GAM
library(sp)     # basic spatial package
#library(maps)
#library(mapdata)
library(maptools)
library(shapefiles)
library(mapproj)
library(rgdal)


#non GIS lib
library(lattice)
library(mgcv)   # GAM

# Read the shapefile for ICES squares 
filen<-file.path("C:","MV","Storage","GIS-shape","ICES","ices_squares_europe.shp")
getinfo.shape(filen)

#ICESsquaresEurope <- readShapePoly(filen,proj4string=CRS("+proj=longlat +datum=NAD27"))
ICESsquaresEurope <- readShapePoly(filen)
names(ICESsquaresEurope)

# just to see it works!
#spplot(ICESsquaresEurope,zcol = "AR_WAT_KM2",xlim=c(-5,13),ylim=c(50,62),scales=list(draw = TRUE))
sq<-data.frame(Square=ICESsquaresEurope$ICESNAME,km2water=ICESsquaresEurope$AR_WAT_KM2)
subset(sq,Square %in% c('43F7','43F8','43F9'))


a<-read.csv(file=file.path(root,exchangeDir,'CPUE per age per subarea_2020-09-11 14_02_25','CPUE per age per subarea_2020-09-11 14_02_25.csv'))
a<-subset(a,Year>=1974)
head(a)
a$DateofCalculation<-NULL
a$AphiaID<-NULL
head(a)
b<-reshape(data=a,idvar=c("Survey","Year","Quarter","Area","SubArea","Species"), 
        varying=list(7:17),   v.names='cpue',  direction='long')
b$Age<-b$time-1; b$time<-NULL
b<-subset(b,Age<=6 & Year<=2019)
tapply(b$cpue,list(b$Species,b$Age),mean,na.rm=T)

b<-subset(b,!(Age >3 & Species=="Sprattus sprattus"))
b<-subset(b,!(Age>3 &  Species=="Trisopterus esmarkii"))
tapply(b$cpue,list(b$Species,b$Age),mean,na.rm=T)

b[b$Age>=5,'Age']<-'5+'

b<-subset(b,!(Quarter==1 & Age==0))
tapply(b$cpue,list(b$Species,b$Age),mean,na.rm=T)
b$Sea<-'North Sea'
b[b$Area==10,'Sea']<-'Channel'
b[b$Area==8,'Sea']<-'Skagerrak'
b[b$Area==9,'Sea']<-'Kattegat'
unique(b$Species)


b<-subset(b,!(Species=="Gadus morhua" & Area ==9))
b<-subset(b,!(Species=="Melanogrammus aeglefinus" & (Area ==9 | Area==10)))
b<-subset(b,!(Species=="Merlangius merlangus" &  (Area ==9 | Area==8)))
b<-subset(b,!(Species=="Pollachius virens" & Area ==10))
b<-subset(b,!(Species=="Sprattus sprattus" & Area ==10))
b<-subset(b,!(Species=="Trisopterus esmarkii" & (Area ==9 | Area==10))) # Kattegat deleted on purpose
b<-subset(b,!(Species=="Pleuronectes platessa" & (Area ==9 | Area==10))) 


# water area stuff
ar<-unique(subset(b,select=c(Quarter, Area, SubArea)))
sq2<-merge(x=ar,y=sq,by.x="SubArea",by.y='Square',all.x=TRUE)
area_weight<-aggregate(km2water~Quarter+Area,sum,data=sq2)


#b$one<-1
#round(ftable(tapply(b$one,list(b$Year,b$Quarter,b$Species,b$Sea,b$Age),sum,na.rm=T)))

#round(ftable(tapply(b$cpue,list(b$Species,b$Sea,b$Age),sum,na.rm=T)))
#round(ftable(tapply(b$cpue,list(b$Quarter,b$Species,b$Sea,b$Age),sum,na.rm=T)))

if (TRUE) {
  b1<-aggregate(cpue~Year+Quarter+Species+Sea+Area+Age,na.rm=T,mean,data=b)
  dim(b1);head(b1);head(area_weight)
  b1<-merge(b1,area_weight)
  b1$cpue<-b1$cpue*b1$km2water
  b1<-aggregate(cpue~Year+Quarter+Species+Age+Sea,na.rm=T,sum,data=b1)
  b2<-aggregate(cpue~Year+Quarter+Species+Age,na.rm=T,sum,data=b1)
  b2$sum<-b2$cpue;b2$cpue<-NULL} else
{ # simple sum
  b1<-aggregate(cpue~Year+Quarter+Species+Sea+Age,na.rm=T,sum,data=b)
  b2<-aggregate(cpue~Year+Quarter+Species+Age,na.rm=T,sum,data=b1)
  b2$sum<-b2$cpue;b2$cpue<-NULL
}


b3<-merge(b1,b2)
head(b3)
b3$proportion<-b3$cpue/b3$sum
b3[b3$sum==0,'proportion']<-NA

head(b3)
b3<-droplevels(b3)
round(ftable(tapply(b3$proportion,list(b3$Species,b3$Quarter,b3$Sea,b3$Age),mean,na.rm=T)),2)
round(ftable(tapply(b3$proportion,list(b3$Species,b3$Sea,b3$Age),mean,na.rm=T)),2)


head(subset(b3,Species=="Pleuronectes platessa"))


b3<-subset(b3,!(Species=="Pleuronectes platessa" & Age<=1))

b3<-subset(b3,!(Species=="Pollachius virens" & Age<=2 & Year<1991))

b3<-droplevels(b3)


by(b3,list(b3$Species,b3$Quarter),function(x){
  stock<-x[1,'Species']; q<-x[1,'Quarter']
  cat(stock,q,'\n')
 trellis.device(device='png',file=file.path(OutputDir,paste0('StockDist_Q',q,' ',stock,'.png')),width = 1000, height = 800)
  print(xyplot(proportion~Year|paste('Age',Age),group=Sea,auto.key =TRUE,type='b',ylab='Stock proportion',main=paste0(stock,' Q:',q),data=x))
 cleanup()
})

if (FALSE) {
  by(b3,list(b3$Species,b3$Quarter),function(x){
    stock<-x[1,'Species']; q<-x[1,'Quarter']
    cat(stock,q,'\n')
    trellis.device(device='png',file=file.path(OutputDir,paste0('Stock2Dist_Q',q,' ',stock,'.png')),width = 1000, height = 1400)
    print(round(ftable(tapply(x$proportion,list(x$Year,x$Sea,x$Age),mean)),2))
  
    print(barchart(proportion~as.character(Year)|paste('Age',Age),group=Sea,horizontal = FALSE,stack = TRUE,scales = list(x = list(rot = 45,tick.number=5)),
                   auto.key =TRUE,ylab='Stock proportion',main=paste0(stock,' Q:',q),data=x))
    cleanup()
  })
}
#######################
# re-calculate proportions without the Enlish Channel, as data years are few
b<-subset(b,Area != 10)

if (TRUE) {
  b1<-aggregate(cpue~Year+Quarter+Species+Sea+Area+Age,na.rm=T,mean,data=b)
  dim(b1);head(b1);head(area_weight)
  b1<-merge(b1,area_weight)
  b1$cpue<-b1$cpue*b1$km2water
  b1<-aggregate(cpue~Year+Quarter+Species+Age+Sea,na.rm=T,sum,data=b1)
  b2<-aggregate(cpue~Year+Quarter+Species+Age,na.rm=T,sum,data=b1)
  b2$sum<-b2$cpue;b2$cpue<-NULL} else
  { # simple sum
    b1<-aggregate(cpue~Year+Quarter+Species+Sea+Age,na.rm=T,sum,data=b)
    b2<-aggregate(cpue~Year+Quarter+Species+Age,na.rm=T,sum,data=b1)
    b2$sum<-b2$cpue;b2$cpue<-NULL
  }


b3<-merge(b1,b2)
head(b3)
b3$proportion<-b3$cpue/b3$sum
b3[b3$sum==0,'proportion']<-NA

head(b3)
b3<-droplevels(b3)
round(ftable(tapply(b3$proportion,list(b3$Species,b3$Quarter,b3$Sea,b3$Age),mean,na.rm=T)),2)


head(subset(b3,Species=="Pleuronectes platessa"))


b3<-subset(b3,!(Species=="Pleuronectes platessa" & Age<=1))

b3<-subset(b3,!(Species=="Pollachius virens" & Age<=2 & Year<1991))

b3<-droplevels(b3)


trend<-by(b3,list(b3$Species,b3$Quarter,b3$Age),function(x){
  x<-subset(x,Sea=='North Sea' & !is.na(proportion))
  stock<-x[1,'Species']; q<-x[1,'Quarter']; a<-x[1,'Age']
  cat(stock,q,a,'\n')
  #print(x)
  gns<-mean(x$proportion,na.rm=T)
 # b<-gam(proportion~s(Year,k=4),data=x)
  
  if (!all(x$proportion==1) & !(any(is.na(x$proportion))) ) {
    x[x$proportion==1,'proportion']<-0.99
    x[x$proportion==0,'proportion']<-0.01
    b<-gam(proportion~s(Year,k=4),data=x,family=betar(link="logit"))
    
    predicted<-predict(b,type="response")
    bs<-summary(b)
    s<-bs$s.table
  } else {
    predicted<-1
  }

 
  if (length(x$proportion)==length(predicted)) data.frame(Stock=stock,Year=x$Year,Quarter=q,Age=a,proportion=x$proportion,predicted=predicted,gns=gns,df=s[2],prob=s[4]) 
 })


bb<-dimnames(trend)
res<-NULL
for (Species in bb[[1]]) {
  for (Quarter in bb[[2]]) {
    for (Age in bb[[3]]) {  
      cat(Species,Quarter,Age,'\n')

      res<-rbind(res,trend[Species,Quarter,Age][[1]])
    } 
  }
}

#subset(res,Stock=="Gadus morhua")

#xyplot settings

show.settings()
my.col<-c('black','red','blue','orange','darkgreen','brown','yellow')
mt<-trellis.par.get()
mt$superpose.symbol$col<-my.col
mt$superpose.symbol$pch<-1:7
mt$superpose.line$col<-my.col
mt$superpose.line$lwd<-2
trellis.par.set(mt)
show.settings()
#####



head(res)
by(res,list(res$Stock,res$Quarter),function(x){
  stock<-x[1,'Stock']; q<-x[1,'Quarter'];
  x$age<-paste0('Age:',x$Age,' df:',round(x$df,2),' prob:',round(x$prob,3),' avg:',round(x$gns,2))
  cat(stock,q,'\n')
  trellis.device(device='png',file=file.path(OutputDir,paste0('Stock3Dist_Q',q,' ',stock,'.png')),width = 1000, height = 1000)
  print(xyplot(predicted+proportion~Year|age,type='b',data=x,ylab="Proportion in the North Sea",main=paste0(stock,' Q:',q)))
 cleanup()
})


by(res,list(res$Stock),function(x){
  stock<-x[1,'Stock']; 
  q1<-subset(x,Quarter==1,select=c(Year,Age,predicted))
  q3<-subset(x,Quarter==3,select=c(Year,Age,predicted))
  q3$predQ<-q3$predicted;q3$predicted<-NULL;
  x<-merge(q1,q3,all=TRUE)
  x$age<-paste0('Age:',x$Age)
  print(head(x))
  
  trellis.device(device='png',file=file.path(OutputDir,paste0('Stock4Dist_',stock,'.png')),width = 1000, height = 1000)
  print(xyplot(predicted + predQ~Year|age,type='b',data=x,ylab="Proportion in the North Sea",main=paste0(stock)))
  cleanup()
})

unique(res$Stock)
cod<-subset(res,Stock=="Gadus morhua" & (Quarter ==1 | (Quarter==3 & Age=='0'))) 
ftable(round(tapply(cod$predicted,list(cod$Year,cod$Age),sum),2))
 
codt<-subset(res,Stock=="Gadus morhua" & (Quarter ==1 )) 
ftable(round(tapply(codt$predicted,list(codt$Year,codt$Age),sum),2))


cod<-subset(cod,Age!='0')
cod0<-subset(cod,Age=='1')
cod0$Age<-'0'
cod<-rbind(cod,cod0)

cod5<-subset(cod,Age=='5+')
cod5.10<-NULL
for (a in as.character(5 :10)) {
  cod5$Age<-a
  cod5.10<-rbind(cod5.10,cod5)
}

cod<-rbind(cod,cod5.10)
cod<-subset(cod,Age!='5+')
ftable(round(tapply(cod$predicted,list(cod$Year,cod$Age),sum),2))

save(cod,file=file.path(root,exchangeDir,'cod_dist.Rdata'))

# sprat
unique(res$Stock)
spr<-subset(res,Stock=="Sprattus sprattus" & (Quarter ==1 | (Quarter==3 & Age=='0'))) 
ftable(round(tapply(spr$predicted,list(spr$Year,spr$Age),sum),2))
ftable(round(tapply(spr$predicted,list(spr$Age),mean,na.rm=TRUE),2))
mean(spr$predicted)

