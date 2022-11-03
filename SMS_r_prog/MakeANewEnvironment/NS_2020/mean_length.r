
###################################################
# mean length

a<-read.csv(file=file.path(root,exchangeDir,'mean_l_key_2017.csv'))
head(a)

sort(unique(a$species))
a[a$species=='HAK','species']<-'HKE'
#hk<-subset(a,species=='HKE' & year ==2016)
#tapply(hk$mean_l,list(hk$age,hk$quarter),sum)

a<-subset(a,!(species %in% c('N_M','W_M')),select=-species.n)

# just add the new years, re-using old data
b<-subset(a,year==lastYold)
for (y in ((lastYold+1):lastY)) {
  b$year<-y
  a<-rbind(a,b)
}

# do not change anything, so far


# new plusgroup for herring;
sp<-'HER'
h<-read.table(file=file.path(finalExchangeDir,'mean_l_from_SAS.dat'),header=TRUE)

h<-subset(h,species==sp,select=c(-spno))
h$SMS_area<-1
head(h)

head(a)
a<-subset(a,species=!sp)
a<-rbind(a,h)


# new agegroups for N and W Horsemackerel;
sp<-'W_H'
fdir<-file.path( root,"Data_NorthSea","input_NS_2020",'ByStock',sp,"SS3")  
b <- read.csv(file=file.path(fdir,"horse_mac_N_West_2020.csv"),header=TRUE)
unique(b$species)
b<-transmute(b,year=year,species=species,quarter=quarter,age=age,SMS_area=1,mean_l=len*10)
hm<-c("W_H","N_H")
b<-subset(b,species  %in% hm)
unique(b$species)

a<-subset(a,!(species %in% hm))
a<-rbind(a,b)



# hake
sp<-'HKE'
a<-subset(a,species!='HKE')
b<-read.csv(file=file.path(root,exchangeDir,'ByStock',sp,'HAKE_length.csv'))
b$mean_l<-round(b$mean_l*10)

head(a)
head(b)
a<-rbind(a,subset(b,select=names(a)))


#mackerel
m<-read.csv(file.path(root,exchangeDir,'ByStock','MAC','meanL_mackerel_WGWIDE_2017_tab_8_4_1_1.csv'))
m[is.na(m$man_l),'mean_l']<- -1
m$year<-firstY

mm<-subset(m,year=firstY)
for (y in ((firstY+1):lastY)) {
  mm$year<-y
  m<-rbind(m,mm)
}
m$species<-'MAC'
m$SMS_area<-1


a<-rbind(a,m)

# sandeel
head(subset(a,species=='COD'))
head(san_length)
san_length$new.mean_l<-round(san_length$MEANL*10)

b<-subset(san_length,select=c(year, species, age, quarter,new.mean_l))
head(b)

a<-merge(x=a,y=b,all.x=TRUE)
a[!is.na(a$new.mean_l),'mean_l']<-a[!is.na(a$new.mean_l),'new.mean_l']
a$new.mean_l<-NULL

if (FALSE) {
  tst<-subset(a,species=='NSA' & year==2007)
  tapply(tst$mean_l,list(tst$quarter,tst$age),sum)
}
head(a)

####

BIO.MEANL<-a

save(BIO.MEANL,file=file.path(root,exchangeDir,'BIO_MEANL.Rdata'))


write.table(BIO.MEANL,file=file.path(finalExchangeDir,paste0('mean_l.dat')),row.names = F,quote = T,sep=',')

