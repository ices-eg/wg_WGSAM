

load(file=file.path(root,exchangeDir,'BIO_01.Rdata'),verbose=T)

#Mackerel
a<-read.csv(file=file.path(root,exchangeDir,'ByStock','MAC','catchPerAreaWGWIDE2020.csv'),skip=6)
head(a)

a$Prop4_3a<-a$cat34/a$catch
plot(a$Year,a$Prop4_3a)

b<-cbind(a$cat34,a$cat6,a$cat78,a$cat1254,a$cat89)
rownames(b)<-a$Year
colnames(b)<-c('SUBAREAS 3 AND 4','SUBAREA 6','SUBAREA 7 AND DIVISIONS 8.ABDE','SUBAREAS 1 2 5 AND 14','DIVISIONS 8.C AND 9.A')
b<-t(b)/1000
b
barplot(b,legend = rownames(b),ylab="Catches (1000 tobbes)",args.legend = list(x = "topleft"))

plot(a$Year,a$Prop4_3a,xlab='',ylab='Proportion within area 3a and 4',type='b',lwd=2)
#abline(lm(Prop4_3a~Year,data=a, subset=Year>=2000),lwd=2 ,col='red')
mm<-subset(a,Year>=2000,select=c(Year,Prop4_3a))
mm$factor<-mm$Prop4_3a/mm[1,"Prop4_3a"]

mac_reg<-lm(Prop4_3a~Year,data=a, subset=Year>=2000)
# abline(mac_reg,lwd=2 ,col='red')


Mcomp<-read.csv(file=file.path(root,exchangeDir,'ByStock','MAC',"NEA_stock_dist.CSV")) # distribution by component
head(Mcomp)

d<-read.csv(file=file.path(root,exchangeDir,'ByStock','MAC',"macDist.CSV")) # distribution by component
head(d)
chk<-round(tapply(d$perc,list(d$year,d$quarter,paste(d$species,d$age_g)),sum))

ftable(round(chk),0)

chk['2019',,]==chk['1998',,]



g<-merge(x=Mcomp,y=d,all.x=T,by.x='Year',by.y='year')

g$propWithin<-ifelse(g$species=='N_M',g$NorthSea*g$perc/100,g$Western*g$perc/100)
g2<-aggregate(propWithin~Year+quarter+age_g,data=g,sum)

trellis.device(device='png',file=file.path(OutputDir,paste0('proportion_NS_MAC_in_the_North_Sea','.png')),width = 1000, height = 800)
print(xyplot(propWithin~as.numeric(Year)|paste('Age',age_g),data=g2,type='a',groups=quarter,scales = "free",xlab='year', ylab='Proportion within the North Sea',lwd=2,
             auto.key =   list(space = "right", points = FALSE, lines = TRUE)))
cleanup()


#North Sea component halved since 2000
fac<-0.5/(2016-2000+1)  #2017 version

ftable(round(tapply(g2$propWithin,list(g2$Year,g2$quarter,g2$age_g),sum)*100),0)
head(g2)
refYear<-2000
g2$newPropWithin<-ifelse(g2$Year>refYear & g2$age_g!='1',g2$propWithin*(1-(g2$Year-refYear)*fac),g2$propWithin)

trellis.device(device='png',file=file.path(OutputDir,paste0('proportion_M2_MAC_Q1Q4','.png')),width = 1000, height = 800)

print(xyplot(newPropWithin~as.numeric(Year)|paste('Age',age_g),data=g2,type='a',groups=quarter,scales = "free",xlab='year', ylab='Proportion within the North Sea',lwd=2,
             auto.key =   list(space = "right", points = FALSE, lines = TRUE)))
cleanup()



g2$newPROP_N<-g2$newPropWithin
g2$year<-g2$Year
a<-subset(g2,select=c(year,quarter,age_g,newPROP_N))
a$species<-'MAC'

b<-data.frame(age=1:10,age_g=c('1','2',rep('>2',8)))

d<-merge(x=a,y=b)

d$age_g<-NULL

a<-merge(x=BIO.01,y=d,all.x=T)
head(a)
a[!is.na(a$newPROP_N),'PROP_M2']<-a[!is.na(a$newPROP_N),'newPROP_N']
a$newPROP_N<-NULL

if (FALSE) {
  m<-subset(a,species=='MAC' & year==1997)
  tapply(m$PROP_M2,list(m$age,m$quarter),sum)
}


# Whiting
a[a$species=='WHG','PROP_M2']<-0.90

# Haddock
a[a$species=='HAD','PROP_M2']<-0.88

# Saithe
a[a$species=='POK','PROP_M2']<-0.906


# Sprat
stock<-'SPR'
b<-read.csv(file=file.path(root,exchangeDir,'ByStock',stock,'proportion_NS_Sprat.csv'))
head(b)
b$catch<-1-b$catch
b$HERAS<-1-b$HERAS
herY<-subset(b,!is.na(HERAS),select=c(year))
b<-subset(b,year>=1974)
trellis.device(device='png',file=file.path(OutputDir,paste0('stock_dist',stock,'.png')),width = 500, height = 500)
print(xyplot(catch+HERAS~year,type='b',data=b,ylab="Proportion in the North Sea"))


bp<-gam(catch~s(year),data=b,family=betar(link="logit"))
bp<-gam(HERAS~s(year),data=b,family=betar(link="logit"))
#bp<-gam(catch~s(year,k=8),data=b,family=betar(link="logit"))

predicted<-predict(bp,type="response")
b[b$year %in% herY$year,'predicted']<-predicted
#b$predicted<-predicted
trellis.device(device='png',file=file.path(OutputDir,paste0('stock_dist_trend',stock,'.png')),width = 500, height = 500)

print(xyplot(HERAS+predicted~year,type='b',data=b,ylab="Proportion in the North Sea"))
cleanup()

first<-b[b$year==min(herY$year),'predicted']
b[is.na(b$predicted),'predicted']<-first

q<-data.frame(quarter=1:4); 
spr<-merge(x=b,y=q) ; 
spr$species<-stock
spr<-subset(spr,select=c(species,year,quarter,predicted))

a<-merge(x=a,y=spr,all.x=T)
head(a)

a[!is.na(a$predicted),'PROP_M2']<-a[!is.na(a$predicted),'predicted']
a$predicted<-NULL


#a[a$species=='SPR','PROP_M2']<-0.80



#### cod

load(file=file.path(root,exchangeDir,'cod_dist.Rdata'))
head(cod)
head(a);
cod<-data.frame(species='COD',year=as.character(cod$Year),age=cod$Age,newPROP_N=cod$predicted)

q<-data.frame(quarter=1:4)
cod<-merge(x=cod,y=q)

a<-merge(x=a,y=cod,all.x=T)
head(a)

a[!is.na(a$newPROP_N),'PROP_M2']<-a[!is.na(a$newPROP_N),'newPROP_N']
a$newPROP_N<-NULL

head(a)

####
BIO.01<-a
save(BIO.01,file=file.path(root,exchangeDir,'BIO_01.Rdata'))

newP<- subset(BIO.01, WSEA>0,select=c(species,year,quarter,age,PROP_M2))


if (makeGraphs) {
  
  OldBio<-read.csv(file=file.path(root,exchangeDir,paste0('VPA_Bi01_',old_key_label,'.csv')))
  oldP<- subset(OldBio,(year<=2016 & WSEA>0),select=c(species,year,quarter,age,PROP_M2))
  
  
  newP$oldNew<-'key-2020';
  # head(subset(NewBio,quarter==1 & species=='COD' & age==10),200)
  oldP$oldNew<-'key-2017'
  a<-rbind(newP,oldP)
  
  b<-subset(a,quarter==1)
  b<-aggregate(PROP_M2~species+year+age+oldNew,sum,na.rm=T,data=b)
  summary(b)
  
  by(b,b$species,function(x) {
    trellis.device(device='png',file=file.path(OutputDir,paste0('proportion_M2_Q1',x[1,'species'],'.png')),width = 1000, height = 800)
    print(xyplot(PROP_M2~as.numeric(year)|
                   paste(species,formatC(age, digits = 0, width = 2, format = "f")),groups=oldNew,data=x,type='a',lwd=2,
                 scales = list(x=list(relation="same"),y=list(relation='free')),xlab='year',ylab='Proportion within the North Sea, Quarter 1',
                 auto.key =   list(space = "right", points = FALSE, lines = TRUE)))
    cleanup()  
  })
  
  
  a<-rbind(newP,oldP)
  
  b<-subset(a,species=='MAC')
  b<-aggregate(PROP_M2~species+year+quarter+age+oldNew,sum,na.rm=T,data=b)

  by(b,list(b$species,b$quarter),function(x) {
    trellis.device(device='png',file=file.path(OutputDir,paste0('proportion_M2_MAC_Q',x[1,'quarter'],'.png')),width = 1000, height = 800)
    print(xyplot(PROP_M2~as.numeric(year)|
                   paste(species,"Q:",quarter,'Age:',formatC(age, digits = 0, width = 2, format = "f")),groups=oldNew,data=x,type='a',lwd=2,
                 scales = list(x=list(relation="same"),y=list(relation='free')),xlab='year',ylab='Proportion within the North Sea, Quarter 1',
                 auto.key =   list(space = "right", points = FALSE, lines = TRUE)))
    cleanup()  
  })
  
  
  
}


