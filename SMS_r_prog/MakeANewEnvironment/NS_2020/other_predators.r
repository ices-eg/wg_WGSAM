
###################################################
# Other predators

a<-read.csv(file=file.path(root,exchangeDir,'other_sp_key_2017.csv'))


sort(unique(a$species))
a<-subset(a,!(species %in% c('N_M','W_M')),select=-species.n)
a<-subset(a,!(species %in% c('HAK','HKE')))

# just add the new years, re-using old data
b<-subset(a,year==lastYold)
for (y in ((lastYold+1):lastY)) {
  b$year<-y
  a<-rbind(a,b)
}
a$species.n<-a$sub_area<-NULL  # added later on;


# birds
birds<-c("FUL", "GBG", "GLT", "GNT", "GSE",  "HEG", "KTW" , "PUF", "RAZ" )
# do nothing - so far

# Hake
species2<-'HKE'
sp<-'Hake'
fdir<-file.path( root,"Data_NorthSea","input_NS_2020",'ByStock',species2)  
b<-read.csv(file=file.path(fdir,'HAKE_N_WSEA.csv'))

a<-subset(a,species!=species2)
a<-rbind(a,subset(b,select=names(a)))
sort(unique(a$species))


 # NEW data from Nis
 
 # Rays
  sp<-'RAJ'
  spName<-'Amblyraja radiata'
  old<-subset(a,species==sp)
  old$biomass<-old$N*old$WSEA
  t1<-tapply(old$biomass,list(old$year,old$quarter),sum)
  aver<-apply(t1,1,mean)
  old<-round(cbind(t1,aver))
  old
  mean(old[as.character(1982:2013),5])
 

   
  fdir<-file.path( root,"Data_NorthSea","input_NS_2020",'ByStock',sp)  
  b<-read_csv(file=file.path(fdir,"starry_ray_predicted.csv")) 
  mutate(b,N=biomass/weight)
  fyd<-min(b$Year)
  fydata<-filter(b,Year==fyd)

  for (y in (firstY:(fyd-1))) {
    fydata<-mutate(fydata,Year=y)
    b<-rbind(b,fydata)
  }  
  b<-transmute(b, year=Year,quarter=Quarter,species=sp,age=unclass(as.factor(size)), WSEA=weight/1000, N=no*1E6) %>% filter(year<=lastY)
  
  t1<-tapply(b$N*b$WSEA,list(b$year,b$quarter),sum)
  aver<-apply(t1,1,mean)
  new<-round(cbind(t1,aver))
  new
  mean(old[as.character(1982:2013),5])
  mean(new[as.character(1982:2013),5])
 
   new/old

  # something is wrong, I use the SAS output
  b<-read.table(file=file.path(fdir,"raj2020.dat"),header=TRUE) 
  head(b)
  b<-subset(b,year<=lastY)
  t1<-tapply(b$N*b$WSEA,list(b$year,b$quarter),sum)
  ftable(t1)
  
  aver<-apply(t1,1,mean)
  new<-round(cbind(t1,aver))
  new
  mean(new[as.character(1982:2013),5])  # 100000
  dim(old);dim(new)
  new/old
  
  
  a<-subset(a,species!=sp)
  a<-rbind(a,subset(b,select=names(a)))


  
  # grey gurnard
  sp<-'GUR'
  spName<-'grey gurnard'

  old<-subset(a,species==sp)
  old$biomass<-old$N*old$WSEA
  t1<-tapply(old$biomass,list(old$year,old$quarter),sum)
  aver<-apply(t1,1,mean)
  old<-round(cbind(t1,aver))
  old
  mean(old[as.character(1977:2013),5])
  
  fdir<-file.path( root,"Data_NorthSea","input_NS_2020",'ByStock',sp)  
  b<-read_csv(file=file.path(fdir,"gray_gurnard_predicted.csv")) 
  mutate(b,N=biomass/weight)
  head(b)
   b<-transmute(b, year=Year,quarter=Quarter,species=sp,age=unclass(as.factor(size)), WSEA=weight/1000, N=cpue*1E6) %>% filter(year<=lastY)
  
  t1<-tapply(b$N*b$WSEA,list(b$year,b$quarter),sum)
  aver<-apply(t1,1,mean)
  new<-round(cbind(t1,aver))
  new
  mean(old[as.character(1977:2013),5])
  mean(new[as.character(1977:2013),5])
  new/old
  
  # something is wrong, I use the SAS output
  b<-read.table(file=file.path(fdir,"gur2020.dat"),header=TRUE) 
  head(b)
  b<-subset(b,year<=lastY)
  t1<-tapply(b$N*b$WSEA,list(b$year,b$quarter),sum)
  aver<-apply(t1,1,mean)
  new<-round(cbind(t1,aver))
  new
  mean(new[as.character(1977:2013),5])
  dim(old);dim(new)
  new/old
  
  
  a<-subset(a,species!=sp)
  a<-rbind(a,subset(b,select=names(a)))
 

  
# horse mackerel
sp<-'W_H'
spName<-'Western Horsemackerel'
old<-subset(a,species==sp)
old$biomass<-old$N*old$WSEA
old<-round(tapply(old$biomass,list(old$year,old$quarter),sum))
old


a<-subset(a,species!=sp)

fdir<-file.path( root,"Data_NorthSea","input_NS_2020",'ByStock',sp,"SS3")  
b <- read.csv(file=file.path(fdir,"horse_mac_N_West_2020.csv"),header=TRUE)
b<-subset(b,species==sp)
head(b)
new<-round(tapply(b$N*b$WSEA,list(b$year,b$quarter),sum))
new
old
a<-rbind(a,subset(b,select=names(a)))


sp<-'N_H'
spName<-'North Sea  Horsemackerel'
a<-subset(a,species!=sp)

b <- read.csv(file=file.path(fdir,"horse_mac_N_West_2020.csv"),header=TRUE)
b<-subset(b,species==sp)
a<-rbind(a,subset(b,select=names(a)))


if (dim(a[a$N>0 & a$WSEA<=0,])[[1]]>0) stop('Other predator N>0 and Wsea <=0')

round(tapply(a$N*a$WSEA,list(a$year,a$species),sum)/4)
####

BIO.02<-a

subset(a,species=='W_H' & year==1986)

save(BIO.02,file=file.path(root,exchangeDir,'BIO_02.Rdata'))

########################
old<-read.csv(file=file.path(root,exchangeDir,'other_sp_key_2017.csv'))
load(file=file.path(root,exchangeDir,'BIO_02.Rdata'),verbose=TRUE)
new<-BIO.02

new$oldNew<-'new'
old$oldNew<-'old'
old$sub_area<-NULL
old$species.n<-NULL

head(old)
head(new)
old[old$species=='HAK','species']<-'HKE'

if (makeGraphs) {

  a<-rbind(new,old)
  a$bio<-a$N*a$WSEA
  a<-subset(a,N>0)
  
  b<-aggregate(bio~species+year+quarter+oldNew,sum,na.rm=T,data=a)
  

  
  by(b,b$species,function(x) {
    trellis.device(device='png',file=file.path(OutputDir,paste0('Other_bio_',x[1,'species'],'.png')),width = 1000, height = 800)
    print(xyplot(bio~as.numeric(year)|
                   paste(species,'Q:',quarter),groups=oldNew,data=x,type='a',lwd=2,
                 scales = list(x=list(relation="same"),y=list(relation='free')),xlab='year',ylab='biomass',
                 auto.key =   list(space = "right", points = FALSE, lines = TRUE)))
    cleanup()  
  })
  

  b<-aggregate(N~species+year+quarter+age+oldNew,sum,na.rm=T,data=a)
  
  
  by(b,b$species,function(x) {
    trellis.device(device='png',file=file.path(OutputDir,paste0('Other_N_',x[1,'species'],'.png')),width = 1000, height = 800)
    print(xyplot(N~as.numeric(year)|
                   paste(species,"Q:",quarter,"age:",formatC(age, digits = 0, width = 2, format = "f", flag = "0")),groups=oldNew,data=x,type='a',lwd=2,
                 scales = "free",xlab='year',ylab='Number',
                 auto.key =   list(space = "right", points = FALSE, lines = TRUE)))
    cleanup()  
  })
}  
