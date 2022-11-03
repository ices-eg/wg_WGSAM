
readOldMAC<-function() {
  CW<-data.frame(year=1974:1979, cw=c(607586,784014,828235,620247,736726,843155)) # total catach weight from WGWIDE 2016, table 8.3.1.1.
  old<-read.csv(file=file.path(root,exchangeDir,'vpa_ca01_MAC_4M.csv'))
  old<-subset(old,year>=1974  & year<1980)
  old[old$age>10,'age']<-10 # to 10+
  
  old$CN<-old$WCATCH*old$CATCHN
  old<-aggregate(cbind(CN,CATCHN)~year+age+sub_area+species,data=old,sum )
  old$WCATCH<-old$CN/old$CATCHN
  
  oldSum<-aggregate(CN~year,data=old,sum )
  
  oldSum<-merge(oldSum,CW)
  oldSum$ratio<-oldSum$cw/oldSum$CN
  oldSum<-subset(oldSum,select=c(year,ratio))
  old<-merge(old,oldSum)
  old$CATCHN<-old$CATCHN*old$ratio
  old$CN<-old$ratio<-NULL
  head(old)
  tapply(old$CATCHN*old$WCATCH,list(old$year),sum)
  old$quarter<-1
  return(old)
}

MAC<-readOldMAC()
MAC$PROP_CAT<-1; 
head(MAC)


NEW<-read.csv(file=file.path(root,exchangeDir,'vpa_ca01_MAC_SAM.csv'))
NEW$fleet<-NEW$cat<-NULL
head(NEW)

MAC<-rbind(MAC,NEW)
MAC[MAC$age>10,'age']<-10 # to 10+

MAC$CN<-MAC$WCATCH*MAC$CATCHN
MAC<-aggregate(cbind(CN,CATCHN)~year+quarter+age+sub_area+species,data=MAC,sum )
MAC$WCATCH<-MAC$CN/MAC$CATCHN
MAC$CN<-NULL
MAC$sub_area<-'1'

t(t(tapply(MAC$WCATCH*MAC$CATCHN,list(MAC$year),sum)))
round(tapply(MAC$WCATCH*MAC$CATCHN,list(MAC$year,MAC$age),sum),0)

# mean catch weights
old<-read.csv(file=file.path(root,exchangeDir,'vpa_ca01_MAC_4M.csv'))
xyplot(WCATCH~age|quarter,group=year,data=old,type='b')
xyplot(WCATCH~year|quarter,group=age,data=old,type='b')



# mean stock weights
old<-read.csv(file=file.path(root,exchangeDir,'vpa_bi01_MAC_4M.csv'))

show.settings()
my.col<-c('black','red','blue','orange','darkgreen','brown','yellow')
mt<-trellis.par.get()
mt$superpose.symbol$col<-my.col

#mt$superpose.symbol$pch<-1:7
mt$superpose.symbol$pch<-as.character(1:7)
mt$superpose.line$col<-my.col
trellis.par.set(mt)

xyplot(WSEA~age|paste("Quarter:",quarter),group=year,data=old,type='b',ylab='Mean weight in the sea (kg)')
xyplot(WSEA~year|paste("Quarter:",quarter),group=age,data=old,type='b',ylab='Mean weight in the sea (kg)')
xyplot(WSEA~age,group=paste(quarter,year),data=old,type='b',ylab='Mean weight in the sea (kg)')

old$year<-NULL
old<-unique(old)
trellis.par.set(mt)
xyplot(WSEA~age,group=quarter,data=old,type='b',ylab='Mean weight in the sea (kg)')

old<-subset(old,age<=12)
old[old$age>10,'age']<-10

old<-aggregate(WSEA~quarter+sub_area+species+age+M+M1,data=old,mean)
xyplot(WSEA~age,group=paste(quarter),data=old,type='b',ylab='Mean weight in the sea (kg)')

sam<-read.csv(file=file.path(root,exchangeDir,'vpa_bi01_MAC_SAM.csv'))
sam<-subset(sam,select=c(-WSEA,-M,-M1,-sub_area,-quarter))

bio<-merge(old,sam)

ftable(round(tapply(bio$WSEA,list(bio$year,bio$quarter,bio$age),sum),3))
head(bio)

b<-subset(bio,year==1980,select=c(-year))
a<-data.frame(year=1974:1979,species='MAC')
ab<-merge(a,b)
bio<-rbind(bio,ab)

