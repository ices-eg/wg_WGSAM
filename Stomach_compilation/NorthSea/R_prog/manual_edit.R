##postprocesssing as done in SAS, Bootsmave, length_stom_con1.sas

# title " minimum size class= &min_length ";
#
# data a;
# set aa;
# *where pred not in ('MAC','HOR');
# where pred not in ('HOR');
#
# label pred='Pred';
# label pred_l='Pred_l';
# if pred='SAI' then pred='POK';
# if pred='WHI' then pred='WHG';
# if pred='GRE' then pred='GUR';
# if pred='G_S' then pred='GSE';
# if pred='HAR' then pred='HBP';
#
# if prey='SAI' then prey='POK';
# if prey='WHI' then prey='WHG';
#
# if prey_l<&min_length then do;
# prey='OTH';
# end;
#
#
# if prey_l>pred_l and prey ne 'OTH' then do;
# prey='OTH';
# end;
#
# if prey='HER' then do;
# if prey_l<70 then prey='OTH';
# if quarter=2 and prey_l<100 then prey='OTH';
# end;
#
# if prey='WHG' then do;
# if  quarter in (1,2) and prey_l<100 then  prey='OTH';
# if  quarter=4 and prey_l=50 then  prey='OTH';
#
# end;
#
# if prey='COD' then do;
# if quarter  in (4,1,2) and prey_l<=50 then prey='OTH';
# if quarter=3 and year=1986 and pred='COD' and pred_l>=700 and prey_l<=70 then delete;
# if quarter  in (1,2) and prey_l<100 then prey='OTH';
# end;
#
# if pred='HAD' and pred_l=500 and quarter in (3) and prey='COD' then do;
# prey='OTH';
# end;
#
# if prey='HAD' and prey_l<100 and quarter in (1,2)  then do;
# prey='OTH';
# end;
#
# if prey='HAD' and prey_l<70 and quarter in (3,4)  then do;
# prey='OTH';
# end;
#
# if prey not in &sms_prey_sp then do;
# prey='OTH';
# end;
#
# if prey in ('POK') then do;
# prey='OTH';
# end;
#
# if prey in ('SAN','NSA','SSA') and pred ne 'GSE' then do;
# if ( prey_l>=250) or (prey_l<70 and quarter in (1,2)) then prey='OTH';
# end;
#
#
# if prey in ('SAN','NSA','SSA') and pred eq 'GSE' then do;
# if ( prey_l>=250) or (prey_l<70 and quarter in (1,2)) then prey='OTH';
# end;
#
#
# if prey in ('NOP')then do;
# if prey_l<70 and quarter in (1,2) then prey='OTH';  *delete 0-group in  quarter;
# end;
#
#
# if pred='HAD' and prey in ('SPR','HER','HAD','WHG') then do;
# prey='OTH';
# end;
#
# if pred='POK' then do;
# if prey in ('COD','SPR')  then do;
# prey='OTH';
# prey_l=9999;
# end;
# end;
#
# if pred in ('N_M','W_M','MAC') and year=1991 and quarter=1 then do;
# if pred_l=300 and prey in ('SAN','NSA','SSA')  then do;     * sandeel;
# prey='OTH';
# end;
# end;
#
# if pred in ('N_M','MAC') then do;
# if prey in ('HAD','WHG','COD') then do;
# prey='OTH';
# end;
# end;
#
# if pred in ('W_M','MAC') then do;
# if prey in ('HAD','WHG','COD') then do;
# prey='OTH';
# end;
# end;
#
# if pred='W_H' then do;
# if prey in ('HER','HAD') then do;
# prey='OTH';
# end;
# end;
#
# if pred='N_H' then do;
# if prey in ('HAD','NOP') then do;
# prey='OTH';
# end;
# end;
#
#
# if pred='GUR' then do;
# if pred_l<=120 then delete;
# if prey='HER' then do;
# prey='OTH';
# end;
# end;
#
# if pred='RAJ' then do;
# if pred_l<200 then delete;
# if prey in ('SPR','HER') then do;
# prey='OTH';
# end;
# end;
#
# if  (prey='OTH') then do; prey_l=9999; prey_u=9999; end;
#
#
# if pred in ('W_H') and year in (1987) then delete;
# if pred in ('N_H') and year in (1980,1988,1989,1990) then delete;
# if pred in ('GUR') and year in (1987) then delete;
# if pred in ('COD','HAD') and pred_l in (100,120) then delete;
# if pred='WHG' and pred_l=500 then delete;
# if pred='HAD' and pred_l=600  then delete;
# if stomcon=. then delete;
# if stomcon=0 and prey ne 'OTH' then delete;
# *if prey='OTH' and stomcon=1 then delete;
# run;

post_process<-function(x){
  #test x<-diet

  x_org<-x

  control<-attr(x,'control')
  min_prey_length<-control@min_prey_length
  mis_l<-control@mis_l
  other<-control@other
  mis_size_class<-control@mis_size_class
  mis_ll<-paste(mis_l,mis_l,sep='-')

  x<-as.data.frame(x)
  x<-mutate_if(x,is.factor,as.character)
  info<-read_csv(file.path(config_dir,"species_info.csv"),col_types = cols()) %>%
    select(code,SMS_species)
  x<-left_join(x,info,by=c('prey_name'='SMS_species'))

  x$to_other<-FALSE
  x$delete<-FALSE
  x$quarter<-as.numeric(substr(x$stratum_time,7,7))
  x$year<-as.numeric(substr(x$stratum_time,1,4))

  crit<-x$prey_size>x$pred_size & x$code !='OTH'; summary(crit)
  x[crit,'to_other']  <- TRUE


  crit<-x$code=='HER' & x$prey_size< '0070-0000'; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='HER' & x$prey_size< '0100-0000' & x$quarter==2; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='WHG' & x$prey_size< '0100-0000' & x$quarter %in% c(1,2); summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='WHG' & x$prey_size< '0060-0000' & x$quarter %in% c(4); summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='COD' & x$prey_size< '0050-9999' & x$quarter %in% c(4,1,2); summary(crit)
  #x[crit,]
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='COD' & x$prey_size< '0100-0000' & x$quarter %in% c(1,2); summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='COD' & x$prey_size<= '0070-0000' & x$quarter %in% c(3) & x$pred_size>='0700-0000'; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='HAD' & x$code=='COD' & x$pred_size %in% c('0500-0600','0500-0700') & x$quarter %in% c(3); summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='HAD' & x$prey_size<= '0100-0000' & x$quarter %in% c(1,2) ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='HAD' & x$prey_size<= '0070-0000' & x$quarter %in% c(3,4) ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='POK'  ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code %in% c('PLE') & x$pred_name =='COD' ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code %in% c('SAN','NSA','SSA') & x$pred_name !='GSE' & (x$prey_size>='0250-0000' | (x$prey_size<'0070-0000' & x$quarter %in% c(1,2))) ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$code=='NOP' & x$prey_size<'0070-0000' & x$quarter %in% c(1,2) ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$pred_name=='HAD' & x$code %in% c('SPR','HER','HAD','WHG') ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$pred_name=='POK' & x$code %in% c('SPR','COD') ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$pred_name=='MAC' & x$year==1991 & x$quarter==1 & x$code %in% c('SAN','NSA','SSA') ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$pred_name=='MAC' & x$code %in% c('HAD','WHG','COD') ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$pred_name=='W_H' & x$code %in% c('HAD','HER') ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$pred_name=='N_H' & x$code %in% c('HAD','NOP') ; summary(crit)
  x[crit,'to_other']  <- TRUE


  crit<-x$pred_name=='COD' & x$pred_size<'0200-0250' & x$year==1981 & x$quarter==3 ; summary(crit)
  x[crit,'delete']  <- TRUE


  crit<-x$pred_name=='GUR' & x$pred_size<'0200-0000' ; summary(crit)
  x[crit,'delete']  <- TRUE


  crit<-x$pred_name=='GUR' & x$code %in% c('HER') ; summary(crit)
  x[crit,'to_other']  <- TRUE


  crit<-x$pred_name=='RAJ' & x$pred_size<'0200-0000' ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='RAJ' & x$pred_size>'0400-0000' & x$year==1991 & x$quarter==4 ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='RAJ' & x$pred_size>'0700-0000' ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='RAJ' & x$code %in% c('HER','SPR') ; summary(crit)
  x[crit,'to_other']  <- TRUE

  crit<-x$pred_name=='W_H' & x$year %in% c(1987) ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='N_H' & x$year %in% c(1980,1988,1989,1990) ; summary(crit)
  x[crit,]
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='GUR' & x$year %in% c(1980,1981,1982,1983,1989,1990,1987) ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name %in% c("COD","HAD") & x$pred_size<='0150-0000' ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='WHG' & x$pred_size>='0500-0000' ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='HAD' & x$pred_size>='0600-0000' ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='MAC' &  x$stratum_time=='1981-Q1' & x$pred_size=='0150-0200' ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$pred_name=='MAC' &  x$stratum_time=='1991-Q1' & x$pred_size=='0400-0500' ; summary(crit)
  x[crit,'delete']  <- TRUE


  crit<-(x$prey_w==0 | is.na(x$prey_w)) & x$code!='OTH' ; summary(crit)
  x[crit,'delete']  <- TRUE

  crit<-x$n_tot<5 ; summary(crit)
  x[crit,'delete']  <- TRUE


  #x[x$to_other,]
  #x[x$delete,]

  cat('\n',sum(x$delete),' record are deleted\n',sum(x$to_other),' records are renamed into other food\n')

  xx<-x %>% group_by(stratum_area, stratum_time, pred_name, pred_size) %>% mutate(w=prey_w/sum(prey_w), id=paste(pred_name,pred_size,stratum_time,sep='_')) %>% ungroup()
  xx<-xx %>%filter(!delete & to_other) %>% group_by(id,prey_name,prey_size,to_other) %>% summarize(w=sum(w)) %>% arrange(desc(w))
  print(filter(xx,w>0.01),n=500)


  x<-filter(x,!delete)
  x[x$to_other,'prey_name']<-other
  x[x$to_other,'prey_size']<-mis_ll
  x[x$to_other,'prey_size_class']<-mis_size_class

  x$calc_l<- (as.numeric(substr(x$pred_size,1,4)) + as.numeric(substr(x$pred_size,6,9)))/2
  x<-mutate(x,pred_l_mean=if_else(is.na(pred_l_mean),calc_l,pred_l_mean))

  x<-select(x,-code,-to_other,-delete,-year,-quarter,-calc_l)
  x <-x %>% mutate_if(is.character,as.factor)
  pred<- select(x,stratum_area, stratum_time, pred_name, pred_size, pred_size_class, n_tot, n_sample,pred_l_mean, key,rep_id ) %>%
         distinct() %>% arrange(key)
  prey<-x %>% group_by( key,prey_name,prey_size, prey_size_class) %>% summarise(prey_w=sum(prey_w,na.rm=TRUE))%>% arrange(key)

  x_org[['PRED']]<-pred
  x_org[['PREY']]<-prey

  return(x_org)
}
