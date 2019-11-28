root<- file.path("C:","MV","SMS","data_wb","WB SMS_til Morten")

plus<-data.frame(species=c('COD','HER','SPR'),plus=c(7,7,7))


### CANUM
a1<-read.csv(file=file.path(root,"cod_canum_weca_sd_sms.csv"))
a2<-read.csv(file=file.path(root,"her_canum_weca_sd_sms.csv"))
a3<-read.csv(file=file.path(root,"sprat_canum_weca_sd_sms.csv"))
a<-rbind(a1,a2,a3)
catch<-aggregate(CATCHN~year+quarter+sub_area+species+age,data=a,sum,na.rm=T)  # for weigthing purposes

a<-merge(a,plus)
a[a$age>a$plus,'age']<-a[a$age>a$plus,'plus']
a$plus<-NULL

a$obs<-NULL
a$PROP_landed<-1

a$yield<-a$WCATCH*a$CATCHN

a1<-a
a1$sub_area<-99

a2<-rbind(a,a1)
#ftable(round(tapply(a2$yield,list(a2$species,a2$year,a2$sub_area),sum,na.rm=T)/1000))

a$yield<-NULL

write.csv(a,file=file.path(root,"VPA_Ca01.in"),row.names = F)


###

# Bio
a1<-read.csv(file=file.path(root,"cod_wsea_mat_sd_sms.csv"))
a2<-read.csv(file=file.path(root,"her_wsea_mat_sd_sms.csv"))
a3<-read.csv(file=file.path(root,"sprat_wsea_mat_sd_sms.csv"))
a<-rbind(a1,a2,a3)
a$obs<-NULL
a$M<-0.2
a$M1<-0.2

a<-merge(a,catch)
a<-merge(a,plus)
a[a$age>a$plus,'age']<-a[a$age>a$plus,'plus']
a$plus<-NULL


a$WC<-a$WSEA*a$CATCHN
a$MC<-a$M*a$CATCHN
a$M1C<-a$M1*a$CATCHN
a$PC<-a$PROPMAT*a$CATCHN

a<-aggregate(cbind(CATCHN,WC,MC,M1C,PC)~year+quarter+sub_area+species+age,data=a,sum,na.rm=T)
a$M<-a$MC/a$CATCHN
a$M1<-a$M1C/a$CATCHN
a$PROPMAT<-a$PC/a$CATCHN
a$WSEA<-a$WC/a$CATCHN

ftable(round(tapply(a$WSEA,list(a$species,a$sub_area,a$year,a$quarter,a$age),sum,na.rm=T),3))

write.csv(a,file=file.path(root,"VPA_BI01.in"),row.names = F)
###


# distribution
a1<-read.csv(file=file.path(root,"cod_dist_key.csv"))
a2<-read.csv(file=file.path(root,"her_dist_key.csv"))
a3<-read.csv(file=file.path(root,"sprat_dist_key_SD 22_24.csv"))
a<-rbind(a1,a2,a3)

a<-merge(a,plus)
a<-subset(a,age<=plus)
a$plus<-NULL
a$obs<-NULL


ftable(round(tapply(a$prop*1000,list(a$species,a$year,a$quarter,a$sub_area,a$age),sum,na.rm=T),0))
write.csv(a,file=file.path(root,"distribution.in"),row.names = F)
###


#######################
# make data for one area (sd 22, 23 and 24 combined)

out.root<- file.path("C:","MV","SMS","data_wb","data-2014-one-area")

a<-read.csv(file=file.path(root,"VPA_Ca01.in"))

a$WC<-a$WCATCH*a$CATCHN
a$WP<-a$PROP_landed*a$CATCHN

a<-aggregate(cbind(WC,WP,CATCHN)~year+quarter+fleet+cat+species+age,data=a,sum)
a$WCATCH<-a$WC/a$CATCHN
a$PROP_landed<-a$WP/a$CATCHN
a$WC<-NULL; a$WP<-NULL
a$sub_area<-"A"
write.csv(a,file=file.path(out.root,"VPA_CA01.in"),row.names = F)

#
a<-read.csv(file=file.path(root,"VPA_BI01.in"))
b<-read.csv(file=file.path(root,"distribution.in"))
dim(a)
dim(b)
a<-merge(x=a,y=b,all.x=T)
dim(a)

a[is.na(a$prop),'prop']<-0.001
a$ww<-a$WSEA*a$prop
a$pp<-a$PROPMAT*a$prop
a$mm<-a$M*a$prop
a$mm1<-a$M1*a$prop
a<-aggregate(cbind(ww,pp,prop,mm,mm1)~year+quarter+species+age,data=a,sum)


a$WSEA<-a$ww/a$prop
a$PROPMAT<-a$pp/a$prop
a$M<-a$mm/a$prop
a$M1<-a$mm1/a$prop

head(a)
a$ww<-NULL
a$pp<-NULL
a$mm<-NULL
a$mm1<-NULL
a$prop<-NULL

a<-subset(a, !(age==0 & quarter %in% c(1,2)))
a$sub_area<-"A"

write.csv(a,file=file.path(out.root,"VPA_BI01.in"),row.names = F)


