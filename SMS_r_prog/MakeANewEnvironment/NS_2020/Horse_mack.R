# horse mackerel

sp<-'W_H'
spName<-'Western Horsemackerel'

source(file.path(prog.path,RexchangeDir,newEnv,"whm_ss3.R"))  # write file.path(ss3_dir,'SS3_results.csv')

a<-read.csv(file=file.path(root,exchangeDir,'ByStock',sp,'stock_dist_W_H.csv'),skip=13,header=TRUE)
head(a)
nl<-read.csv(file.path(ss3_dir,'lengthBin.csv'))
min(nl$x)
#convert into length assuming a 25 cm HM is 4 years
a$fage<- a$lage<- -9
a[a$age_g=='a1-4','fage']<-min(nl$x)
a[a$age_g=='a1-4','lage']<-25
a[a$age_g=='>4','fage']<-26
a[a$age_g=='>4','lage']<-max(nl$x)
summary(a)


a$rr<-1:nrow(a)
b<-by(a,list(a$rr), function(x) {
  data.frame(expand.grid(year=x[1,"f_year"]:x[1,"l_year"],age=x[1,'fage']:x[1,'lage']),stock=x[1,"stock"],quarter=x[1,"quarter"],age_g=x[1,"age_g"],perc=x[,"perc"])
})

b<-do.call(rbind,b)
b[b$year==1998 & b$quarter==4 & b$age_g==">4","perc"]<-10

b<-subset(b,year>=1974)

cat('Percentage of Western Horse mackerel stock in the North Sea area\n');
ftable(tapply(b$perc,list(b$year,b$quarter,b$age),sum))
b<- b%>% rename(len=age)
unique(b$age_g)

a<-read.csv(file=file.path(ss3_dir,'SS3_results.csv'),header=TRUE)
head(a)
a<- a%>% rename(year=Yr,quarter=Seas)
head(a)
head(b)

ab<-merge(x=a,y=b,all.x=TRUE,by=c('year','quarter','len'))
ab<-subset(ab,len>10 ) %>%mutate(n=n*perc/100,perc=NULL)
ab$lage<-cut(ab$len,c(0,25,30,35,99),right=FALSE)
head(ab)

unclass(ab$lage)

w_h<-filter(ab,n>0) %>% group_by(year,quarter,lage) %>% summarize(bio=sum(n*w),nl=sum(n*len),n=sum(n)) %>%
  mutate(sw=bio/n,len=round(nl/n),nl=NULL,age=unclass(lage)-1) %>% ungroup() %>% mutate(species=sp)
w_h
select(w_h,lage,age) %>% distinct()


### North Sea stock is 7% of the western stock !
sp<-'N_H'
spName<-'North Sea Horsemackerel'
pecentInTheNorthSea=0.07/0.93;  # N=0.07*(N+W)  leads to N=0.07/0.93*w

a<-read.csv(file=file.path(ss3_dir,'SS3_results.csv'),header=TRUE)
a<- a%>% rename(year=Yr,quarter=Seas)
head(a)
a<-subset(a,len>=10 ) %>%mutate(n=n*pecentInTheNorthSea,lage=cut(len,c(10,15,20,25,30,35,99),right=FALSE))
head(a)

unclass(a$lage)

n_h<-filter(a,n>0) %>% group_by(year,quarter,lage) %>% summarize(bio=sum(n*w),nl=sum(n*len),n=sum(n)) %>%
  mutate(sw=bio/n,len=round(nl/n),nl=NULL,age=unclass(lage)) %>% ungroup() %>% mutate(species=sp)
n_h
select(n_h,lage,age) %>% distinct()


summary(n_h)
#mean stock 
n_h_mean<-n_h %>% group_by(quarter,lage,age,species) %>% summarize(bio=mean(bio),sw=mean(sw),n=mean(n), len=mean(len)) %>% ungroup() 

#ly_hom<-filter(n_h,year==ly_hom)

if (FALSE) {
  
    for (y in (firstY:(min(n_h$year)-1))) {
      n_h_first$year<-y
     n_h<-rbind(n_h,n_h_first)
}}


nn<-  expand.grid(year=firstY:lastY,quarter=1:4,species=c('N_H'),age=0:10)  
 
head(n_h_mean)  
head(nn) 
n_h<-merge(x=nn,y=n_h_mean,all.x=TRUE)


n_h$lage<-NULL
w_h$lage<-NULL

# n_h; w_h


a<-bind_rows(w_h,n_h) %>% rename(N=n,WSEA=sw) %>% mutate(bio=NULL)
head(a)

b<-expand.grid(year=firstY:lastY,quarter=1:4,species=c('W_H','N_H'),age=0:10)

ab<-merge(x=b,y=a,all.x=TRUE)
head(ab)
ab[is.na(ab$N),'N']<- -1
ab[is.na(ab$WSEA),'WSEA']<- -1
ab[is.na(ab$WSEA),'len']<- -1

write.csv(ab,file=file.path(ss3_dir,"horse_mac_N_West_2020.csv"))

