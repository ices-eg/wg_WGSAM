
SAS_tab<-function(filen,mult=FALSE){
#Mean stomach contents by roundfish area, before allocation of partly identified prey items (FROM SAS Bootsmave)
SAS<-read_csv(file.path(SAS_dir,filen),col_types = cols())
if ("nregur" %in% colnames(SAS)) SAS<-SAS %>% mutate(nstom=nregur+nempty+nfood+nskelr) else {
  if ("nstom" %in% colnames(SAS)) SAS<-mutate(SAS,haul=9990,Sample=9999,fac=9999) else if ("sum_fac" %in% colnames(SAS)) SAS<-mutate(SAS,nstom=sum_fac,haul=9990,Sample=9999,fac=9999) else SAS<-mutate(SAS,nstom=9999,haul=9990,Sample=9999,fac=9999)
}
if (!("Preyvgt" %in% colnames(SAS))) SAS <- mutate(SAS,Preyvgt=weight)

a<-read_csv(file.path(config_dir,"species_info.csv"),col_types = cols()) %>% filter(nodc>0 & prey_sp) %>%
  select(species,nodc)%>%distinct() %>%rename(species_group=species,First=nodc) %>% mutate(Last=First,named=TRUE)

b<-read_csv(file.path(config_dir,"other_food_nodc.csv"),col_types = cols()) %>%mutate(named=FALSE)
NODC_split<-bind_rows(a,b)
NODC_split


aa<- left_join(SAS,NODC_split,by=c('Prey'='First')) %>% mutate(prey=if_else(named==FALSE | is.na(named) ,'other',species_group,))
if (!("Square" %in% colnames(aa))) aa<-mutate(aa,Square='ALL')
bb<-aa %>% group_by(haul,Sample,Square,nstom,fac,prey,Prey_l) %>% summarize(prey_w=sum(Preyvgt))
if (mult) bb<- bb %>% mutate(prey_w=prey_w*nstom)

pred<-bb %>% ungroup()%>% select(haul,Sample,Square,nstom) %>% unique()
sum(pred$nstom)

by(bb,list(paste(bb$Sample,bb$Square,bb$nstom)),function(x){
  x<-droplevels(x)
  y<-tapply(x$prey_w,list(x$Prey_l,x$prey),sum)
  y<-cbind(y,rowSums(y,na.rm=TRUE))
  y<-rbind(y,colSums(y,na.rm=TRUE))

  round(y,1)
})
}

load(file=file.path(Rdata,paste0('cod_700_.Rdata')),verbose=T)
pred<-s[['PRED']] %>% mutate(lat=NULL,lon=NULL,temp=NULL,depth=NULL,pred_w=NULL,est_alt_lon=NULL,pred_NODC=NULL,pred_age=NULL)
xtabs(n_tot~rectangle,pred)
sum(pred$n_tot)
pred$sample_id
print(pred,n=50)

aa<-as.data.frame(s) %>% group_by(year,quarter,sample_id,fish_id,rectangle,n_tot,prey_name,prey_size) %>%summarise(prey_w=sum(prey_w))%>%
  mutate(sampl=as.character(sample_id)) %>% mutate(sampl=paste(substr(sampl,nchar(sampl)-5,nchar(sampl)),as.character(rectangle),n_tot))
aa
by(aa,aa$sampl,function(x){
  x$prey_name<-as.character(x$prey_name)
  x$prey_size<-as.character(x$prey_size)
  a1<-xtabs(prey_w~prey_size+prey_name,data=x)
  rbind(a1,all=colSums(a1))

})
a1<-xtabs(prey_w~prey_size+prey_name+sampl,data=aa)
a2<-apply(a1,c(3,2),sum)
a2
#view(pred)


filen='test1_cod.csv'


sink(file=file.path(output_dir,'SAS_data_output.dat'))
cat("********************   test1.csv\n")
SAS_tab('test1_cod.csv',mult=TRUE)  #obs per sample
sink()

cat("********************   test2.csv\n")
SAS_tab('test2.csv',mult=FALSE) # per rectangle
cat("********************   test3.csv\n")
SAS_tab('test3.csv',mult=FALSE) # # for roundfish area 1
cat("********************   test4.csv\n")
q<-SAS_tab('test4.csv',mult=FALSE)[[1]] # for total=roundfish area 1
q
round(q/q[nrow(q),ncol(q)]*100,1)
sink()


fac<-read_csv(file.path(SAS_dir,'test2_fac.csv'),col_types = cols()) %>% select(Square,sqr_fac)


d<-read_csv(file.path(SAS_dir,'test1.csv'),col_types = cols())
nstom<-select(d,haul,Sample,Square,nstom,CPUE) %>% distinct()

a<-read_csv(file.path(config_dir,"species_info.csv"),col_types = cols()) %>% filter(nodc>0 & prey_sp) %>%
  select(species,nodc)%>%distinct() %>%rename(species_group=species,First=nodc) %>% mutate(Last=First,named=TRUE)
b<-read_csv(file.path(config_dir,"other_food_nodc.csv"),col_types = cols()) %>%mutate(named=FALSE)
NODC_split<-bind_rows(a,b)
NODC_split


aa<- left_join(d,NODC_split,by=c('Prey'='First')) %>% mutate(prey=if_else(named==FALSE | is.na(named) ,'other',species_group,))
bb<-aa %>% group_by(haul,Sample,Square,nstom,fac,prey,Prey_l) %>% summarize(prey_w=sum(Preyvgt)) %>% mutate(prey_w=prey_w*nstom)
bb

by(bb,list(bb$Sample,bb$Square),function(x){
  x<-droplevels(x)
  y<-tapply(x$prey_w,list(x$Prey_l,x$prey),sum)
  y<-cbind(y,rowSums(y,na.rm=TRUE))
  y<-rbind(y,colSums(y,na.rm=TRUE))
  round(y,1)
})


# by rectangle
bs<-bb %>% group_by(Square,prey,Prey_l) %>% summarize(prey_w=sum(prey_w))
nstom<-nstom %>% group_by(Square) %>% summarize(nstom=sum(nstom))
bs<-left_join(left_join(bs,nstom),fac)  %>% mutate(prey_w=prey_w/nstom)   # average absolute stom content

by(bs,list(bs$Square),function(x){
  x<-droplevels(x)
  y<-tapply(x$prey_w,list(x$Prey_l,x$prey),sum)
  y<-cbind(y,rowSums(y,na.rm=TRUE))
  y<-rbind(y,colSums(y,na.rm=TRUE))
  round(y,1)
})

# by roundfish

sum_fac<-sum(fac$sqr_fac)
sum_fac

b2<-bs %>% mutate(prey_w=prey_w*sqr_fac) %>% group_by(prey,Prey_l) %>% summarize(prey_w=sum(prey_w)) %>% ungroup() %>%
    mutate(prey_w=prey_w/sum_fac)

y<-tapply(b2$prey_w,list(b2$Prey_l,b2$prey),sum)
y<-cbind(y,rowSums(y,na.rm=TRUE))
y<-rbind(y,colSums(y,na.rm=TRUE))

round(y,1)



