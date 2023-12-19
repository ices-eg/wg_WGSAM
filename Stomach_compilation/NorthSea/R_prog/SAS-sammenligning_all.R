
SAS_tab<-function(filen,mult=FALSE,pred,y,q,l,r){
  #Mean stomach contents by roundfish area, before allocation of partly identified prey items (FROM SAS Bootsmave)
  SAS<-read_csv(file.path(SAS_dir,filen),col_types = cols())
  SAS<-filter(SAS,Pred==!!pred & Year==!!y & Quarter==!!q  & pred_l==!!l & roundfish==!!r)

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


  by(bb,list(paste(bb$Sample,bb$Square,bb$nstom)),function(x){
    x<-droplevels(x)
    y<-tapply(x$prey_w,list(x$Prey_l,x$prey),sum)
    y<-cbind(y,rowSums(y,na.rm=TRUE))
    y<-rbind(y,colSums(y,na.rm=TRUE))
    round(y,1)
  })
}

sink(file=file.path(output_dir,'SAS_data_output.dat'))
  cat("********************   test1.csv\n")
  SAS_tab(filen='test1_all.csv',mult=TRUE,pred=8835280103,y=1991,q=3,l=300,r=1)  #obs per sample


  cat("********************   test2.csv\n")
  SAS_tab(filen='test2_all.csv',mult=FALSE,pred=8835280103,y=1991,q=3,l=300,r=1)  # per rectangle

  cat("********************   test3.csv\n")
  q<-SAS_tab('test3_all.csv',mult=FALSE,pred=8835280103,y=1991,q=3,l=300,r=1)[[1]] # for roundfish area
  q
  round(q/q[nrow(q),ncol(q)]*100,2)
sink()


fac<-read_csv(file.path(SAS_dir,'test2_fac.csv'),col_types = cols()) %>% select(Square,sqr_fac)


d<-read_csv(file.path(SAS_dir,'test1_all.csv'),col_types = cols())
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



