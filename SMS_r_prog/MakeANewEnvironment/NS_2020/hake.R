

# Hake
species<-'HKE'
sp<-'Hake'
MVrun<-FALSE # re-run SS3 assessment, takes an hour 
source(file.path(root,exchangeDir,'ByStock',species,'hake_assessment_MV.R'))

fdir<-file.path( root,"Data_NorthSea","input_NS_2020",'ByStock',species)  
setwd(fdir)


str(myreplist,1,list.len=200)
nl<-myreplist$lbinspop    # length bins used for population
write.csv(nl,file='lengthBin.csv',row.names =FALSE)


convL<-function(x,v.names='n') {
  # test:  x<-N
  x<-subset(x,x[,"Beg/Mid"]=='M')
  x<-subset(x,select=c("Yr","Seas",as.character(nl)))
  
  x<-reshape(x,direction='long',varying=list(3:(2+length(nl))),v.names=v.names)
  x$Length<-nl[x$time]
  x<-subset(x,select=c("Yr","Seas",'Length',v.names))
  return(x)
}



# Data for SMS (all by length classess)
str(myreplist$inputs)
str(myreplist,1,list.len=300)
myreplist$Growth_Parameters
nl<-myreplist$lbinspop    # length bins used for population
write.csv(nl,file='lengthBin.csv',row.names =FALSE)

#myreplist$mean_body_wt 

convL<-function(x,v.names='n') {
  # test:  x<-N
  # there are three BirthSeas ?
  
  x<-subset(x,x[,"Beg/Mid"]=='M')
  
  x<-subset(x,select=c("Yr","Seas",as.character(nl)))
  
  x<-reshape(x,direction='long',varying=list(3:(2+length(nl))),v.names=v.names)
  x$Length<-nl[x$time]
  x<-subset(x,select=c("Yr","Seas",'Length',v.names))
  return(x)
}



# head(myreplist$natlen)
n<-convL(x=myreplist$natlen,v.names='n')
head(n)
n$w<-myreplist$Growth_Parameters[1,'WtLen1']*n$Length**myreplist$Growth_Parameters[1,'WtLen2']

head(subset(n,Length==50))


# tst<-subset(n,Length==30 & Seas==1)  # there are three BirthSeas ?
# subset(tst,Yr==1998)

n$age<-cut(n$Length,breaks=c(0,25,30,35,40,50,60,70,80,100,150),include.lowest =TRUE)
#n$age<-ifelse(n$Length<25,1,ifelse(n$Length<60,2,3))

n$nl<-n$Length*n$n
n$nw<-n$w*n$n
nn<-aggregate(cbind(n,nl,nw) ~ Yr+Seas+age,data=n,sum)
nn$ml<-nn$nl/nn$n
nn$mw<-nn$nw/nn$n
nn$bio<-nn$n*nn$mw
head(nn)


tapply(nn$ml,list(nn$Yr,nn$Seas,nn$age),sum)
tapply(nn$mw,list(nn$Yr,nn$Seas,nn$age),sum)
round(tapply(nn$bio,list(nn$Yr,nn$Seas),sum),0)

#write.csv(nb,file='SS3_results.csv',row.names =FALSE)


#proportion in the North Sea

a<-read.csv(file=file.path(root,exchangeDir,'ByStock',species,"Hake_catch_WGBIE_20202.csv"),skip=3)
a<- filter(a,year>=1974)
a[is.na(a)]<-0

#proportion i area 4 of landings in 3456 combined, data from 2013
p4<-filter(a,year>=2013) %>% transmute(year=year,p4=lan4/(lan3+lan4+lan5+lan6)) %>% summarize(p4=mean(p4))

p4<-mutate(a,lan4=if_else(year>=2013,lan4,lan3456*as.numeric(p4))) %>%
  transmute(year=year,p4=lan4/lanTotal)



cleanup()

filename=file.path(paste0(sp,'_prop_NS.png'))
newplot(dev='png',nox=1,noy=1,Portrait=TRUE,filename=filename,dir=fdir,w11=6,w8=8);
#par(mar=c(3,5,3,2))
plot(x=p4$year,y=p4$p4,ylim=c(0,0.25),ylab='Proportion within the North Sea',xlab='year',type='b')
cleanup()


#a<-read_csv(file=file.path(root,exchangeDir,'ByStock',species,"MV_hake_ass","bootstrap","initial","data",paste0("Table_1_","2019",".csv")))
#write_csv(a,file.path(root,exchangeDir,'ByStock',species,"MV_hake_ass","bootstrap","initial","data",paste0("Table_1_","2019",".csv")))

# from detailed catch data
years<-2013:2019
a<-lapply(years,function(x){
  cat(x,'\n')
  a<-read.csv(file=file.path(root,exchangeDir,'ByStock',species,"MV_hake_ass","bootstrap","initial","data",paste0("Table_1_",x,".csv")))
 # print(head(a))
  a$Area<-trimws( as.character(a$Area))
  subset(a,select=c(Year,Area,Season,CATON))
})

b<-do.call(rbind,a)
sort(unique(b$Area))
head(b)
ns<-c( "27.4","27.4.a","27.4.b","27.4.c" ,"IV","IVa","IVb","IVc" )
b$ns<- 'out'; b[b$Area %in% ns,'ns']<-'ns'
tapply(b$CATON,list(b$Year),sum)

#filter annual data  data
bb<-filter(b,Season <=4) %>% group_by(Year,ns,Season) %>% summarize(w=sum(CATON))
tapply(bb$w,list(bb$Year),sum)

bbb<-filter(b,Season <=4) %>% group_by(Year,Season) %>% summarize(annuw=sum(CATON))

p4q<-left_join(bb,bbb) %>% transmute(year=Year,quarter=Season,p4=w/annuw) %>% filter(ns=='ns')
tapply(p4q$p4,list(p4q$year,p4q$quarter),sum)

a<-group_by(p4q,quarter) %>% summarize(p4=mean(p4))
avp4<-mean(a$p4)
a<- mutate(a,p4=p4/avp4)



n<- select(nn,year=Yr,quarter=Seas,age,n,mw,ml)

n1<-left_join(x=filter(n,year<2013),y=p4) %>% mutate(n=n*p4) %>% select(year,quarter,age,n,mw,ml)
n2<-left_join(x=filter(n,year>=2013),y=p4q) %>% mutate(n=n*p4)  %>% select(year,quarter,age,n,mw,ml)
a<-filter(n1,year==1978)
n0<-NULL
for (y in (1974:1977)) {
  a$year<-y
  n0<-rbind(a,n0)
}

hake_n<-rbind(n0,n1,n2) %>% mutate(bio=n*mw) 

# change into "ages", but delete the 0-25 cm length group
hake_n$age<-unclass(hake_n$age)-1
hake_n<-subset(hake_n,age>=1)
summary(hake_n)

ohake <- hake_n %>% transmute(year=year,quarter=quarter,species=species,age=age,sub_area=1,WSEA=mw,N=n)
write.csv(ohake,file='HAKE_N_WSEA.csv',row.names=FALSE)

ohake <- hake_n %>% transmute(year=year,quarter=quarter,species=species,age=age,sub_area=1,mean_l=ml,SMS_area=1)
write.csv(ohake,file='HAKE_length.csv',row.names=FALSE)


pl<- hake_n %>% group_by(year,quarter) %>% summarize(BIO=sum(bio))

cleanup()
#dev<-"print"
dev<-'png'

filename=file.path(paste0(sp,'_BIO.png'))
newplot(dev,nox=2,noy=2,Portrait=TRUE,filename=filename,dir=fdir,w11=8);
#par(mar=c(3,5,3,2))

by(pl,list(pl$quarter),function(x){
  b<- tapply(x$BIO/1000,list(x$year),sum)
  barplot(b,space=0.0,ylab='Biomass (1000 tonnes)')
  title(main=paste(species,' Q:', x[1,'quarter'],sep=''))
  
})
cleanup()
