split_sandeel<-function(x){
# sandeel into North and south, and one NODC code for both
# if area in (1,2,3,7,8) then prey='NSA';
# else if area in (4,5,6,10) then prey='SSA';

  # if pred='HAR' then do;
  #   if (prey='SAN') then do;
  #   tmp=stomcon;
  #   prey='NSA'; stomcon=stomcon*0.66; output;
  #   prey='SSA'; stomcon=stomcon*0.34; output;
  #   end;
  #   else output;
  # end;



 prey<-left_join(x[['PREY']],select(x[['PRED']],area,sample_id,fish_id),by = c("sample_id", "fish_id")) %>%
   mutate(prey_name=as.character(prey_name),prey_name=if_else(prey_name %in% c("Ammodytes","Ammodytes marinus","Ammodytes tobianus","Ammodytoidei"),"Ammodytidae",prey_name))

 hp<-filter(prey,substr(as.character(sample_id),1,3)=='HP_' & prey_name=="Ammodytidae" )
 prey<-filter(prey,!(substr(as.character(sample_id),1,3)=='HP_' & prey_name=="Ammodytidae" ))

 prey <-   prey %>%
   mutate(prey_name=if_else(prey_name=="Ammodytidae" & area %in% c(1,2,3,7,8) ,"N Ammodytidae",prey_name)) %>%
   mutate(prey_name=if_else(prey_name=="Ammodytidae" & area %in% c(4,5,6),"S Ammodytidae",prey_name))

 if (dim(hp)[[1]]>0) {
   hp<-bind_rows (mutate(hp,prey_name="N Ammodytidae",prey_w=prey_w*0.66),
                  mutate(hp,prey_name="S Ammodytidae",prey_w=prey_w*0.34))
   prey<-bind_rows(prey,hp)
 }

 # prey<- prey %>% mutate(prey_name=fct_explicit_na(prey_name),area=NULL) %>%
    prey<- prey %>% mutate(prey_name=fct_na_value_to_level(prey_name),area=NULL) %>%
     mutate(prey_nodc=if_else(prey_name %in% c("N Ammodytidae","S Ammodytidae"),8845010000,prey_nodc))
  x[['PREY']]<-prey
 return(x)
}


split_sandeel_GSE<-function(x,splitD){
  # grey seal only: sandeel into North and south according to set  north_south_san_GSE, and one NODC code for both

 # x<-ns; splitD<-north_south_GSE_quarter
  remains<-subset(x,pred_name!="Halichoerus grypus")
  gse<-subset(x,pred_name=="Halichoerus grypus")

  noso<-splitD %>% mutate(year=as.integer(year),quarter=as.integer(quarter))
  gse<- as.data.frame(gse)

  gsesan<- gse %>% filter(prey_name=="N Ammodytidae")
  gseother<- gse  %>% filter(prey_name!="N Ammodytidae")

  gsesan<-left_join(gsesan,noso,by = join_by(year, quarter))
  gsesanN<-   gsesan %>% mutate(prey_name="N Ammodytidae",prey_w=prey_w*North,North=NULL,South=NULL)
  gsesanS<-   gsesan %>% mutate(prey_name="S Ammodytidae",prey_w=prey_w*South,North=NULL,South=NULL)

  gse<-rbind(gseother,gsesanN,gsesanS) %>%
    mutate(prey_nodc=if_else(prey_name %in% c("N Ammodytidae","S Ammodytidae"),8845010000,prey_nodc))
  gse<-as_STOMobs(gse)
 bb<-c(remains,gse)
 #sort(unique(subset(bb,pred_name=="Halichoerus grypus")[['PREY']]$prey_name))
 # print(summary(subset(bb,pred_name=="Halichoerus grypus")[['PREY']]$prey_name))
 return(bb)
}

rename_sandeel_outside<-function(x){
  # sandeel into North and south,
  # if area in (1,2,3,7,8) then prey='NSA';
  # else if area in (4,5,6,10) then prey='SSA';
 # rename to other if preys of sandeel have been estimated (by redist_unidentifed_prey_sp)outside their area

# x<-b

  control<-attr(x,'control')
  other<-control@other
other_fac<-factor(other,levels=levels(x[['PREY']]$prey_name))

  x[['PREY']]<-left_join(x[['PREY']],select(x[['PRED']],area,sample_id,fish_id),by = c("sample_id", "fish_id")) %>%
    mutate(porpoise=if_else(substr(as.character(sample_id),1,3) %in% c('HP_','Sea'),TRUE,FALSE)) %>%
     mutate(prey_name=if_else(prey_name=="S Ammodytidae" & area %in% c(1,2,3,7,8) & !porpoise,other_fac,prey_name)) %>%
    mutate(prey_name=if_else(prey_name=="N Ammodytidae" & area %in% c(4,5,6) & !porpoise,other_fac,prey_name)) %>%
    select(-area)
  return(x)
}
