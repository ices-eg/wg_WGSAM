# expand stomach data to include "all combinations" of prey and sizes within a given area, year and quarter

expanded.stomcon<-1E-5
#list.data.path=file.path(root,"data_baltic","2013-data")
list.data.path=file.path(root,"data_NorthSea","SMS-input-2013")

makeCheck<-F

a<-read.table(file=file.path(list.data.path,'stomcon_list.dat'),header=T)
a$mean.stom<-NULL
a<-droplevels(subset(a,type %in% c('obs','mid')))

# for checking
#a<-droplevels(subset(a,year==1977 | year==1982))



 #head(a)
#SMS_area year quarter pred pred.no pred.size pred.size.class pred.mean.length prey prey.no prey.size prey.size.class prey.mean.length stomcon type mean.weight haul.no haul.prey.no

of<-file.path(list.data.path,'expanded_stom.dat')
cat('SMS_area year quarter pred pred.size prey prey.size stomcon\n',file=of)
b<-by(a,list(a$SMS_area,a$pred,a$year,a$quarter,a$prey),function(x) {
   x<-droplevels(x)
   bb<-tapply(x$stomcon,list(x$SMS_area,x$year,x$quarter,x$pred,x$pred.size,x$prey,x$prey.size),sum)
   #print(ftable(bb))
   bb<-arr2dfny(bb,name='stomcon')
   write.table(bb,append=T,row.names=F,col.names=F,file=of,quote = F)
 }
)

aa<-read.table(of,header=T)
aa[is.na(aa$stomcon),'stomcon']<-0
aa<-subset(aa,prey !='OTH' | (prey=='OTH' & prey.size==999))
aa<-subset(aa,prey =='OTH' | (prey!='OTH' & prey.size!=999))


aa<-droplevels(aa)
names(aa)

if (makeCheck) by(aa,list(aa$SMS_area,aa$pred,aa$year,aa$quarter,aa$prey),function(x) {
   x<-droplevels(x)
  ftable(tapply(x$stomcon,list(x$SMS_area,x$year,x$quarter,x$pred,x$pred.size,x$prey.size),sum))
 })


new.aa<-subset(aa,aa$stomcon==0)

# predator specific data
dd<-subset(a,select=c(SMS_area, year, quarter, pred, pred.no, pred.size, pred.size.class, pred.mean.length, haul.no, haul.prey.no))
dd<-unique(dd)
head(dd)
head(new.aa)
dim(dd); dim(new.aa)

ad<-merge(x=dd,y=new.aa,all.y=T)
dim(ad)
head(ad)

# prey specific
dd<-subset(a,select=c(year,pred,prey, prey.no,prey.size,prey.size.class ))
dd<-unique(dd)
dim(dd);dim(ad)
ad<-merge(x=dd,y=ad,all.y=T)
dim(ad)

ad$type<-'exp'

ad$prey.mean.length<-NA
ad$mean.weight<-NA

if ("prey.mean.length.ALK" %in% names(a)) ad$prey.mean.length.ALK<-0

#head(a); head(ad)

f<-rbind(a,ad)
f$exist<- 10
f[f$stomcon>0,'exist']<-1
summary(f)

fof<-order(f$SMS_area,f$year, f$quarter, f$pred, f$pred.no, f$prey.no,f$prey,f$prey.size,f$exist,f$pred.size)
f<-f[fof,]
head(subset(f,prey !='OTH'))

# just checking
if (makeCheck) {
  ac<-droplevels(subset(a,prey!='OTH'))
  by(ac,list(ac$SMS_area,ac$pred,ac$year,ac$quarter,ac$prey),function(x) {
    x<-droplevels(x)
    ftable(tapply(x$prey.size.class,list(x$year,x$quarter,x$pred,x$pred.size,x$prey,x$prey.size),mean,na.rm=T))
   })
}

subset(f,year==1993 & quarter==4 & prey=="SPR")


for (i in (1:dim(f)[[1]])) {
  if (f[i,'exist']==1) {
     l.prey.mean.length<-f[i,'prey.mean.length']
     l.mean.weight<-f[i,'mean.weight']
     l.prey.mean.length.ALK <-f[i,'prey.mean.length.ALK']
   } else {
     f[i,'prey.mean.length'] <-l.prey.mean.length
      f[i,'mean.weight'] <-l.mean.weight
      f[i,'stomcon']<- expanded.stomcon
      f[i,'prey.mean.length.ALK']<-l.prey.mean.length.ALK 
   }
}
f$exist<-NULL


subset(f,year==1993 & quarter==4 & prey=="SPR")
# just checking, it should provide table with intergers
if (makeCheck) {
  ac<-droplevels(subset(f,prey!='OTH'))
  by(ac,list(ac$SMS_area,ac$pred,ac$year,ac$quarter,ac$prey),function(x) {
   x<-droplevels(x)
   ftable(tapply(x$prey.size.class,list(x$year,x$quarter,x$pred,x$pred.size,x$prey,x$prey.size),mean))
  })
}
summary(f)

write.table(f,append=F,row.names=F,col.names=T,file=of,quote = F)