load(file='b.rData',verbose=T)
b

d<-calc_population_stom(b,verbose=TRUE)
read_csv( "C:/_C_drev/Stomach_compilation/NorthSea/Stom_2023/config/remains_weighting_totall_1991.csv" )
control<-attr(b,'control')
b$PRED
summary(b$PRED)

bb<-as.data.frame(b)
summary(bb)

sc<-control@calc_sub_strata
sc
sum(bb$prey_w)
ntot<-b[['PRED']] %>% group_by( pred_name, pred_size, stratum_time,  area, stratum_area, stratum_sub_area) %>%
  summarize(ntot=sum(n_tot),mean_cpue=mean(pred_cpue,na.rm=TRUE)) %>% ungroup()
pw<-bb %>% group_by( pred_name, pred_size, stratum_time,  area, stratum_area, stratum_sub_area,prey_name,prey_size) %>%summarize(sumw=sum(prey_w)) %>% ungroup()
bbb<-left_join(ntot,pw) %>% mutate(prey_w=sumw/ntot*pred_cpue) %>%
bbb

,mean_cpue=mean(pred_cpue,na.rm=TRUE))
#####

sc<-control@calc_strata
sc

######
sc<-control@calc_total
