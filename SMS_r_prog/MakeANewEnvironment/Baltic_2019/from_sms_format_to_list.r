From_SMS_format_to_list<-function(otherPredExist=TRUE,catchMultiplier=1,code.name=c('COD','WHG'),exchangeDir=file.path(root,exchangeDir),addfn='a') {
#code.name<-c('COD','HER','SPR')
#code.name<-c('SPR')
la<-SMS.control@max.age.all
fa<-SMS.control@first.age
years<-c(1,1)
years[1]<-SMS.control@first.year
years[2]<-SMS.control@last.year
ny<-years[2]-years[1]+1
npr<-sum(SMS.control@species.info[,'predator']>=1)
nsp<-SMS.control@no.species
nq<-SMS.control@last.season
noAreas<-SMS.control@no.areas

#############  catch data

CATCHN<-head(scan(file.path(data.path,'canum.in'),comment.char='#'),-1)
WCATCH<-head(scan(file.path(data.path,'weca.in'),comment.char='#'),-1)
Prop.landed<-head(scan(file.path(data.path,'proportion_landed.in'),comment.char='#'),-1)
Prop.landed<-Prop.landed[1:length(WCATCH)]
b<-expand.grid(sub_area=1:noAreas,species.n=first.VPA:nsp,year=years[1]:years[2],quarter=1:nq,age=fa:la,cat='A')
cat(min(b$species.n),max(b$species.n))
b$species<-code.name[b$species.n]
b<-b[order(b$sub_area,b$species.n,b$year,b$quarter,b$age),]
b<-data.frame(b,CATCHN=CATCHN,WCATCH=WCATCH,PROP_CAT=Prop.landed)
b<-subset(b,select=c(year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT))
b$CATCHN<-catchMultiplier*b$CATCHN
print(file.path(exchangeDir,paste0('VPA_Ca01_',addfn,'.csv')))
write.table(b,file=file.path(exchangeDir,paste0('VPA_Ca01_',addfn,'.csv')),row.names = F,quote = T,sep=',')

############## bio data

WSEA<-head(scan(file.path(data.path,'west.in'),comment.char='#'),-1)
WSEA<-WSEA[((first.VPA-1)*noAreas*ny*(la-fa+1)*nq+1):length(WSEA)]
PROPMAT<-head(scan(file.path(data.path,'propmat.in'),comment.char='#'),-1)
M<-head(scan(file.path(data.path,'natmor.in'),comment.char='#'),-1)
M1<-head(scan(file.path(data.path,'natmor1.in'),comment.char='#'),-1)
PROP_M2<-head(scan(file.path(data.path,'n_proportion_m2.in'),comment.char='#'),-1)

b<-expand.grid(sub_area=1:noAreas,species.n=first.VPA:nsp,year=years[1]:(years[2]),quarter=1:nq,age=fa:la)
b$species<-code.name[b$species.n]
b<-b[order(b$sub_area,b$species.n,b$year,b$quarter,b$age),]

b<-data.frame(b,WSEA=WSEA, PROPMAT=PROPMAT,M=M,M1=M1,PROP_M2=PROP_M2)
b<-subset(b,select=c(year,species,quarter,age,sub_area,WSEA,PROPMAT,M,M1,PROP_M2))
write.table(b,file=file.path(exchangeDir,paste0('VPA_Bi01_',addfn,'.csv')),row.names = F,quote = T,sep=',')

################  other_sp

if (otherPredExist) {
  WSEA<-head(scan(file.path(data.path,'west.in'),comment.char='#'),-1)
  WSEA<-WSEA[1:((first.VPA-1)*noAreas*ny*(la-fa+1)*nq)]
  length(WSEA)
  N<-head(scan(file.path(data.path,'other_pred_N.in'),comment.char='#'),-1)
  b<-expand.grid(sub_area=1:noAreas,species.n=1:(first.VPA-1),year=years[1]:years[2],quarter=1:nq,age=fa:la)
  b$species<-code.name[b$species.n]
  b<-b[order(b$sub_area,b$species.n,b$year,b$quarter,b$age),]
  length(N)
  if ( length(WSEA) !=length(N) ) stop('differen number of observations in west.in for "other predators") and other_pred_N.in') 
    
  b<-data.frame(b,WSEA=WSEA, N=N)
  b<-subset(b,select=c(year,species,species.n,age,quarter,sub_area,WSEA,N))
  b$N<-b$N*catchMultiplier
  write.table(b,file=file.path(exchangeDir,paste0('other_sp_',addfn,'.csv')),row.names = F,quote = T,sep=',')
}
###################### mean l

l<-head(scan(file.path(data.path,'lsea.in'),comment.char='#'),-1)
b<-expand.grid(SMS_area=1:noAreas,species.n=1:nsp,year=years[1]:years[2],quarter=1:nq,age=fa:la)
b$species<-code.name[b$species.n]
b<-b[order(b$SMS_area,b$species.n,b$year,b$quarter,b$age),]

b<-data.frame(b,mean_l=l)
b<-subset(b,select=c(year,species,species.n,age,quarter,SMS_area,mean_l))
write.table(b,file=file.path(exchangeDir,paste0('mean_l_',addfn,'.csv')),row.names = F,quote = T,sep=',')

###########################  consum

CONSUM<-head(scan(file.path(data.path,'consum.in'),comment.char='#'),-1)
b<-expand.grid(SMS_area=1:noAreas,species.n=1:npr,year=years[1]:years[2],quarter=1:nq,age=fa:la)
b$species<-code.name[b$species.n]
b<-b[order(b$SMS_area,b$species.n,b$year,b$quarter,b$age),]

b<-data.frame(b,CONSUM=CONSUM)
b<-subset(b,select=c(year,species,species.n,quarter,age,SMS_area,CONSUM))
write.table(b,file=file.path(exchangeDir,paste0('consum_',addfn,'.csv')),row.names = F,quote = T,sep=',')

}