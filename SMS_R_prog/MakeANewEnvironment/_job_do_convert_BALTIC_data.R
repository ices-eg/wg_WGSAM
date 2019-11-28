# this script make 
#  1. write files on a spredsheet like format with data extracted from the current environment (you could then update these files with additional years data)
# 2. convert files produced under 1) to files with data on SMS format

# between 1) and 2) you are supposed to update data !
# or use
collate_automatically<-TRUE

dataSets<-c('old','new','old_and_new','newY05','newY10') [5]

# start by submitting init.R, so file paths are establibsed

#if (my.stock.dir %in% c("Baltic-2019-V01","Baltic-2019-V01_old_stom","Baltic-2019-V01_new_stom","Baltic-2019-V01_old_new_stom")) exchangeDir<-"Data_baltic/2019-data"   # directory with data  files on spread sheet format
exchangeDir<-"Data_baltic/2019-data"   # directory with data  files on spread sheet format

RexchangeDir<-"MakeANewEnvironment"     # directory with R scripts to convert files etc.


keyLabel<-dataSets
otherPredExist<-TRUE
my.years<-1974:2018
options(stringsAsFactors = FALSE)

do_plot<-FALSE
do_loess<-TRUE

#  first 1.


if (FALSE) {  # write a number of of files on a spredsheet like format with data extracted from the current environment (should be an old source run)
              # do not do that for 2019 files (but copy previously made csv files)
  source(file.path(prog.path,RexchangeDir,'from_sms_format_to_list.r'))
 
  my.code.name<-c('COD','HER','SPR')
   From_SMS_format_to_list(otherPredExist=otherPredExist,catchMultiplier=1,code.name=my.code.name,exchangeDir=file.path(root,exchangeDir),addfn=keyLabel)
}

library(tidyverse)
library(reshape2)

library(stockassessment)

if (TRUE) {  #update SMS input files
  
  read_ices<-function(fileName,varName='MATPROP',fac=1,addQuarter=FALSE){
    a<-read.ices(fileName)
    a<-a*fac
    a<-melt(a)
    colnames(a)<-c('year','age',varName)
    if (addQuarter) {
      a$dummy<-1
      quarters<-data.frame(dummy=1,quarter=1:4)
      a<-merge(a,quarters)
      a$dummy<-NULL
    }
    a<-as_tibble(a)
  }
  
  # Herring
  
  a<-read_delim(file=file.path(root,exchangeDir,'Herring',"CANUM by quarter and SD.csv"),delim=',') %>% 
    filter(quarter %in% as.character(1:4) & Subdivision != "SD 28.1/GOR" & Subdivision != "Total") %>% 
    select(year,quarter,Subdivision,age_0,age_1,age_2,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10)
  
  a<-   reshape(a,direction='long',varying=list(4:14),idvar=c("year","quarter","Subdivision"),v.names='CATCHN')
  canum<-  a %>% mutate(age=time-1,CATCHN=CATCHN*1000,time=NULL) 
  tapply(canum$CATCHN,list(canum$year,canum$age),sum,na.rm=TRUE)
  
  a<-read_delim(file=file.path(root,exchangeDir,'Herring',"WECA by quarter and SD.csv"),delim=',') %>% 
    filter(quarter %in% as.character(1:4) & Subdivision != "SD 28.1/GOR" & Subdivision != "Total") %>% 
    select(year,quarter,Subdivision,age_0,age_1,age_2,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10)
  
  a<-   reshape(a,direction='long',varying=list(4:14),idvar=c("year","quarter","Subdivision"),v.names='WCATCH')
  weca<-  a %>% mutate(age=time-1,WCATCH=WCATCH/1000,time=NULL) 
  tapply(weca$WCATCH,list(weca$year,weca$age),sum,na.rm=TRUE)
  
  
  plus_group<-8
  her<-full_join(canum,weca) %>% mutate(SOP=CATCHN*WCATCH, age=if_else(age>plus_group,plus_group,age)) %>%
      group_by( year, quarter, age) %>% summarise(sum_sop=sum(SOP,na.rm=TRUE),CATCHN=sum(CATCHN,na.rm=TRUE)) %>%
      filter(CATCHN>0) %>%
      mutate(WCATCH=sum_sop/CATCHN,sum_sop=NULL,PROP_CAT=1,cat='A',sub_area=1,species='HER')
 
  old<-as_tibble(read.csv(file=file.path(root,exchangeDir,"old_VPA_CA01_.csv"),sep=',')) %>%
    filter(species=='HER') %>% mutate(quarter=as.character(quarter))

  ftable(round(tapply(old$WCATCH,list(old$year,old$quarter,old$age),sum,na.rm=TRUE),4))
  
  keep_data <-  anti_join(x=old,y=her,by = c("species","year", "quarter", "age", "sub_area"))
  her.catch<- bind_rows(keep_data,her) %>% arrange(species, year,quarter, sub_area, cat, age ) %>%
              select(year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT)
  
  
  g<-weca %>% mutate(cohort=year-age, alder=age+as.numeric(quarter)/4-0.125)
   
  write.csv(her.catch,file.path(root,exchangeDir,'Herring',"herring_new_catch.csv"))
  
  # bio data
  her<- her.catch %>% select(year, species, quarter, sub_area ,age ,WCATCH ) %>% 
           rename(WSEA=WCATCH) %>%mutate(quarter=as.integer(quarter))
  
  cleanup()
  if (do_plot) {
    b<-filter(her,((age ==0 & quarter %in% c(3,4)) |(age ==1 & quarter %in% c(1,2,3,4)))) %>% mutate(species='Herring')
    by(b,list(b$species),function(x) {
      X11()
      print(ggplot(x,aes(year,WSEA)) +
              theme_bw() +
              geom_point() +
              geom_smooth(method = "loess") +
              #geom_vline(xintercept =2002) +
              #facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter), scale="free_y") +
              facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter)) +
              labs(x="Year", y="Weight",title=""))
    })
  }
  her13<-filter(her,age==1 & quarter %in% c(1,3)) %>% mutate(quarter=NULL) %>%
         group_by(year,species,age) %>% summarise(WSEA2=mean(WSEA)) %>% mutate(quarter=2L)
  
  her<-left_join(her,her13) %>% mutate(WSEA=if_else(is.na(WSEA2),WSEA,WSEA2)) %>%mutate(WSEA2=NULL)
 
  
  if (do_plot) {
    b<-filter(her,((age ==0 & quarter %in% c(3,4)) |(age ==1 & quarter %in% c(1,2,3,4)))) %>% mutate(species='Herring')
    by(b,list(b$species),function(x) {
    X11()
    print(ggplot(x,aes(year,WSEA)) +
            theme_bw() +
            geom_point() +
            geom_smooth(method = "loess") +
            #geom_vline(xintercept =2002) +
            #facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter), scale="free_y") +
            facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter)) +
            labs(x="Year", y="Weight",title=""))
    })
  }
  
  
  
  
  propMat<-read_ices(fileName=file.path(root,exchangeDir,'Herring',"MatProp.txt"),varName='PROPMAT',addQuarter=TRUE)
  M<-read_ices(fileName=file.path(root,exchangeDir,'Herring',"Natmor.txt"),varName='M',fac=1/4,addQuarter = TRUE)
   
  
  if (do_plot) {
    w_ass <-read_ices(fileName=file.path(root,exchangeDir,'Herring',"West.txt"),varName='WSEA',fac=1/4,addQuarter = TRUE)
        x<- subset(w_ass,quarter==1)
      X11()
      print(ggplot(x,aes(year,WSEA)) +
              theme_bw() +
              geom_point() +
              geom_smooth(method = "loess") +
              facet_wrap(~ paste0('Age:',age), scale="free_y") +
              labs(x="Year", y="Weight",title=""))
  }
    
 
  her.bio<-full_join(propMat,M) %>% mutate(species='HER',M1=0.025,PROP_M2=1,sub_area=1) %>%
               select(year,species,quarter,age,sub_area,PROPMAT,M,M1,PROP_M2)
  her.bio<-full_join(her,her.bio) %>%    filter(age <=plus_group) 
  
  group_0<-her.bio$age==0
  her.bio[ group_0,'PROPMAT']<-0
  her.bio[ group_0,'M1']<-0.025
  her.bio[ group_0,'M']<-0.05
  her.bio[ group_0,'PROP_M2']<-1
  her.bio<- her.bio %>% select(year,species,quarter,age,sub_area,WSEA,PROPMAT,M,M1,PROP_M2)

  cleanup()
  if (do_plot) {
    b<-filter(her.bio,((age ==0 & quarter %in% c(3,4)) |(age ==1 & quarter %in% c(1,2,3,4)))) %>% mutate(species='Herring')
    by(b,list(b$species),function(x) {
      X11()
      print(ggplot(x,aes(year,WSEA)) +
             theme_bw() +
             geom_point() +
             geom_smooth(method = "loess") +
             facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter), scale="free_y") +
             labs(x="Year", y="Weight",title="")
      )
    })
  }
  
  
  if (do_loess) {
    p<-subset(her.bio,(WSEA>0 &(age ==0 & quarter %in% c(3,4)) |(age ==1 )))
  
    pw<-by (p,list(p$species,p$age,p$quarter), function(x){
      lo <- loess(WSEA ~ year, subset(x))
      data.frame(year=my.years,pred=predict(lo, data.frame(year = my.years), se = FALSE))
      lo<-data.frame(species=x[1,'species'], age=x[1,'age'],quarter=x[1,'quarter'],year=my.years,predict=predict(lo,data.frame(year=my.years), se = FALSE))
      return(lo)
    })
    pw<-as_tibble(do.call('rbind',pw))
    
    her.bio<-left_join(her.bio,pw)  %>% mutate(WSEA=if_else(is.na(predict),WSEA,predict))
    # filter(her.bio,species=='HER' &age==1 & quarter==2)
    
    her.bio[her.bio$age==1 & her.bio$quarter==2 & her.bio$WSEA>0.02,'WSEA']<-0.02  # manual change 
    her.bio[her.bio$age==1 & her.bio$quarter==3 & her.bio$WSEA>0.035,'WSEA']<-0.035  # manual change 
    
    if (do_plot) {
      X11()
      b<-filter(her.bio,((age ==0 & quarter %in% c(3,4)) |(age ==1 & quarter %in% c(1,2,3,4))))
      by(b,list(b$species),function(x) {
        X11()
        print(ggplot(x,aes(year,WSEA)) +
                theme_bw() +
                geom_point() +
                #geom_smooth(method = "loess") +
                facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter)) +
                labs(x="Year", y="Weight",title=""))
      })
    }
  }
  
  write.csv(her.bio,file.path(root,exchangeDir,'Herring',"herring_new_bio.csv"))
  
  lw<-Read.length.weight.relation()
  lw<-subset(lw,Species=='Herring')
  lw$Species<-NULL
  lw$species<-'HER'
  a<-merge(her.bio,lw)
  a$mean_l<- (a$WSEA/a$a)**(1/a$b)
  #head(a)
  hl<-data.frame( year=a$year,species=a$species, quarter=a$quarter, age=a$age, sub_area=a$sub_area,mean_l=round(a$mean_l))
  write.csv(hl,file.path(root,exchangeDir,'Herring',"herring_length.csv"),row.names=FALSE)
  
  
  
  
  # sprat
  a<-  as_tibble(read.csv(file=file.path(root,exchangeDir,"Sprat","Baltic sprat CANUM by QUARTER and SD.csv"),sep=';'))
  b<-  as_tibble(read.csv(file=file.path(root,exchangeDir,"Sprat","Baltic sprat WECA by QUARTER and SD.csv"),sep=';'))
  spr<-full_join(a,b) %>% rename(year=Year,area=Subdivision,quarter=Quarter,age=Age,CATCHN=Number_millions,WCATCH=Mean_weight.grams) %>%
        mutate(CATCHN=CATCHN*1000,WCATCH=WCATCH/1000)
  
  plus_group<-as.integer(7)
  spr <- spr %>% mutate(SOP=CATCHN*WCATCH, age=if_else(age>plus_group,plus_group,age)) %>%
    group_by( year, quarter, age) %>% summarise(sum_sop=sum(SOP,na.rm=TRUE),CATCHN=sum(CATCHN,na.rm=TRUE)) %>%
    filter(CATCHN>0) %>%
    mutate(WCATCH=sum_sop/CATCHN,sum_sop=NULL,PROP_CAT=1,cat='A',sub_area=1,species='SPR')
  
  
  old<-as_tibble(read.csv(file=file.path(root,exchangeDir,"old_VPA_CA01_.csv"),sep=',')) %>%
    filter(species=='SPR') 
  
  
  keep_data <-  anti_join(x=old,y=spr,by = c("species","year", "quarter", "age", "sub_area"))
  spr.catch<- bind_rows(keep_data,spr) %>% arrange(species, year,quarter, sub_area, cat, age ) %>%
    select(year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT) %>%    filter(age <=plus_group) 
  
  
  if (do_plot) {
    w_ass <-read_ices(fileName=file.path(root,exchangeDir,'Sprat',"west.txt"),varName='WSEA',fac=1/4,addQuarter = TRUE)
    x<- subset(w_ass,quarter==1)
    X11()
    print(ggplot(x,aes(year,WSEA)) +
            theme_bw() +
            geom_point() +
            geom_smooth(method = "loess") +
            facet_wrap(~ paste0('Age:',age), scale="free_y") +
            labs(x="Year", y="Weight",title=""))
  }
  
  
  write.csv(spr.catch,file.path(root,exchangeDir,'Sprat',"sprat_new_catch.csv"))
  
  # bio data
  spr<- spr.catch %>% select(year, species, quarter, sub_area ,age ,WCATCH ) %>% 
    rename(WSEA=WCATCH) %>%mutate(quarter=as.integer(quarter))
  
  
  cleanup()
  if (do_plot) {
    b<-filter(spr,(WSEA>0 &(age ==0 & quarter %in% c(3,4)) |(age ==1 & quarter %in% c(1,2,3,4)))) %>% mutate(species='Sprat')
    by(b,list(b$species),function(x) {
      X11()
      print(ggplot(x,aes(year,WSEA)) +
              theme_bw() +
              geom_point() +
              geom_smooth(method = "loess") +
              #geom_vline(xintercept =2002) +
              facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter), scale="free_y") +
              labs(x="Year", y="Weight",title=""))
    })
  }
  
  
  
  propMat<-read_ices(fileName=file.path(root,exchangeDir,'Sprat',"MatProp.txt"),varName='PROPMAT',addQuarter=TRUE)
  M<-read_ices(fileName=file.path(root,exchangeDir,'Sprat',"MRedBITS.txt"),varName='M',fac=1/4,addQuarter = TRUE)
  
  spr.bio<-full_join(propMat,M) %>% mutate(species='SPR',M1=0.05,PROP_M2=1,sub_area=1) %>%
    select(year,species,quarter,age,sub_area,PROPMAT,M,M1,PROP_M2)
  spr.bio<-full_join(spr,spr.bio) %>%    filter(age <=plus_group) 
  
  
  cleanup()
  if (do_plot) {
    b<-filter(spr.bio,WSEA>0 &((age ==0 & quarter %in% c(3,4)) |(age ==1 & quarter %in% c(1,2,3,4))))
    aggregate(WSEA~age+quarter+species,mean,data=b)
    by(b,list(b$species),function(x) {
      X11()
      print(ggplot(x,aes(year,WSEA)) +
              theme_bw() +
              geom_point() +
              geom_smooth(method = "loess") +
  #            facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter), scale="free_y") +
                facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter)) +
              labs(x="Year", y="Weight",title=""))
    })
  }
  
  
  group_0<-spr.bio$age==0
  spr.bio[ group_0,'PROPMAT']<-0
  spr.bio[ group_0,'M1']<-0.05
  spr.bio[ group_0,'M']<-0.05
  spr.bio[ group_0,'PROP_M2']<-1
  spr.bio<- spr.bio %>% select(year,species,quarter,age,sub_area,WSEA,PROPMAT,M,M1,PROP_M2)
  
  
  if (do_loess) {
    p<-subset(spr.bio,(WSEA>0 &(age ==0 & quarter %in% c(3,4)) |(age ==1 )))
    
    pw<-by (p,list(p$species,p$age,p$quarter), function(x){
      lo <- loess(WSEA ~ year, subset(x))
      data.frame(year=my.years,pred=predict(lo, data.frame(year = my.years), se = FALSE))
      lo<-data.frame(species=x[1,'species'], age=x[1,'age'],quarter=x[1,'quarter'],year=my.years,predict=predict(lo,data.frame(year=my.years), se = FALSE))
      return(lo)
    })
    pw<-as_tibble(do.call('rbind',pw))
    spr.bio<-left_join(spr.bio,pw)  %>% mutate(WSEA=if_else(is.na(predict),WSEA,predict))
    
    if (do_plot) {
      X11()
      b<-filter(spr.bio,((age ==0 & quarter %in% c(3,4)) |(age ==1 & quarter %in% c(1,2,3,4))))
      by(b,list(b$species),function(x) {
        X11()
        print(ggplot(x,aes(year,WSEA)) +
                theme_bw() +
                geom_point() +
                #geom_smooth(method = "loess") +
                facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter)) +
                labs(x="Year", y="Weight",title=""))
      })
    }
  }
  
  spr.bio[spr.bio$age==7 & spr.bio$quarter==3 & spr.bio$year==2017,'WSEA']<-0.012   # manual change 
  spr.bio[spr.bio$age==7 & spr.bio$quarter==3 & spr.bio$year==2018,'WSEA']<-0.012  # manual change 
  
  write.csv(spr.bio,file.path(root,exchangeDir,'Sprat',"sprat_new_bio.csv"))
  
  lw<-Read.length.weight.relation()
  lw<-subset(lw,Species=='Sprat')
  lw$Species<-NULL
  lw$species<-'SPR'
  a<-merge(spr.bio,lw)
  a$mean_l<- (a$WSEA/a$a)**(1/a$b)
  head(a)
  hl<-data.frame(year=a$year, species=a$species, quarter=a$quarter, age=a$age, sub_area=a$sub_area,mean_l=round(a$mean_l))
  write.csv(hl,file.path(root,exchangeDir,'Sprat',"sprat_length.csv"),row.names=FALSE)
  
   
  # cod as other predator
  a<-read_delim(file=file.path(root,exchangeDir,'Cod',"SS3_results.csv"),delim=',') %>% 
    rename(year=Yr,quarter=Seas) %>% filter(year >=1974) %>% 
    mutate(Length=Length*10)
  infile<-  
  len_classes<-read_delim(file=file.path(root,exchangeDir,paste0("length_classes_",dataSets,".csv")),delim=',') %>% 
    filter(Species=='Gadus morhua' & year>=1974)
  
   
  b <- left_join(x = a, y = len_classes) %>% 
        filter((Length >= l1 & Length < l2)) %>% 
        mutate(l1 = NULL, l2 = NULL) %>% 
        rename(size_class = no, size = group)
  
  b<- b %>% group_by(Species,year,quarter, size_class, size) %>%
        summarise(sum_n=sum(n),sum_bio=sum(bio), sum_nl=sum(Length*n)) %>%
        mutate(WSEA=sum_bio/sum_n,mean_l=sum_nl/sum_n,N=sum_n) %>%
        mutate(sum_n=NULL, sum_bio=NULL,sum_nl=NULL)
  
  tst<-filter(b,year==1987)
  ftable(round(tapply(tst$WSEA,list(tst$year,tst$quarter,tst$size),sum),3))
  ftable(round(tapply(tst$mean_l,list(tst$year,tst$quarter,tst$size),sum),0))
  
  cod<- ungroup(b) %>% mutate(species='COD',sub_area=1,age=size_class) %>%
         select(year,species,quarter,age,sub_area,N,WSEA,mean_l)
  
  write.csv(select(cod,-mean_l),file.path(root,exchangeDir,'Cod',"cod_new_bio.csv"),row.names = FALSE)
  write.csv(select(cod,-mean_l),file.path(root,exchangeDir,paste0("VPA_Bi02_",keyLabel,".csv")),row.names = FALSE)
  write.csv(cod,file.path(root,exchangeDir,paste0("cod_w_l_",keyLabel,".csv")),row.names = FALSE)
  
  cod$mean_l<- round(cod$mean_l)
  write.csv(select(cod,-N,-WSEA,),file.path(root,exchangeDir,'Cod',"cod_length.csv"),row.names = FALSE)

  
  #consumption
  con<-read.csv(file.path(root,exchangeDir,'Cod',"Average QUARTERLY consum param.csv"))
  con$species<-'COD'
  cod<-as_tibble(cod)
  con<-as_tibble(con)
  a<-left_join(cod,con) %>% filter(year>=year.start & year<=year.stop) %>% mutate(CONSUM=a*(mean_l/10)**b /1000,SMS_area=1 )
  
  tst<-filter(a,year %in% c(1984,1990,2000))
  cat('Consumption\n');ftable(round(tapply(tst$WSEA,list(tst$year,tst$quarter,tst$age),sum),3))
  cat("Mean weight at size\n");ftable(round(tapply(tst$CONSUM,list(tst$year,tst$quarter,tst$age),sum),3))
  
  
  write.csv(select(a, year,species,quarter,age,SMS_area,CONSUM),file.path(root,exchangeDir,'Cod',"cod_consum.csv"),row.names = FALSE)
  
  rm(M)
  rm(propMat)
  }

###   HERE, YOU SHOULD UPDATE THE CSV FILES produced above manually (if needed) before YOU GO ON !!!!!!!!!!!!!!!!!!!

if (collate_automatically) {
  
  # catches
  her<-read_csv(file.path(root,exchangeDir,'Herring',"herring_new_catch.csv")) %>%
           select( year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT) %>% 
           rename(SMS_area=sub_area)
  
  spr<-read_csv(file.path(root,exchangeDir,'Sprat',"sprat_new_catch.csv")) %>%
    select( year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT) %>% 
    rename(SMS_area=sub_area)
  
  a<-bind_rows(her,spr) %>% select( year,species,quarter,SMS_area,cat,age,WCATCH,CATCHN,PROP_CAT)
  write_csv(a,file.path(root,exchangeDir,paste0('VPA_Ca01_',keyLabel,'.csv')))
  
  # BIO1
  her<-read_csv(file.path(root,exchangeDir,'Herring',"herring_new_bio.csv")) %>%
    select( year,species,quarter,sub_area,age,WSEA,PROPMAT,M,M1,PROP_M2) %>% 
    rename(SMS_area=sub_area)
  
  spr<-read_csv(file.path(root,exchangeDir,'Sprat',"sprat_new_bio.csv")) %>%
    select( year,species,quarter,sub_area,age,WSEA,PROPMAT,M,M1,PROP_M2) %>% 
    rename(SMS_area=sub_area)
  
  a<-bind_rows(her,spr) %>% select( year,species,quarter,SMS_area,age,WSEA,PROPMAT,M,M1,PROP_M2)
 
  if (FALSE) {
   a[a$year==1982 & a$quarter==4 & a$age==0 & a$species=='HER','WSEA']<-0.007
   a[a$year==1984 & a$quarter==4 & a$age==0 & a$species=='HER','WSEA']<-0.007
   a[a$year==1985 & a$quarter==4 & a$age==0 & a$species=='HER','WSEA']<-0.011
   a[a$year==1987 & a$quarter==1 & a$age==0 & a$species=='HER','WSEA']<-0.009
  }
 
  a[a$year==2002 & a$quarter==1 & a$age==7 & a$species=='SPR','WSEA']<-0.0106
  a[a$year==2002 & a$quarter==2 & a$age==7 & a$species=='SPR','WSEA']<-0.0113
  a[a$year==2002 & a$quarter==3 & a$age==7 & a$species=='SPR','WSEA']<-0.0120
  a[a$year==2002 & a$quarter==4 & a$age==7 & a$species=='SPR','WSEA']<-0.0119
  
  
  
  write_csv(a,file.path(root,exchangeDir,paste0('VPA_Bi01_',keyLabel,'.csv')))
  

  
  # BIO2
  cod<-read_csv(file.path(root,exchangeDir,'Cod',"cod_new_bio.csv"))
  write_csv(cod,file.path(root,exchangeDir,paste0('VPA_Bi02_',keyLabel,'.csv')))

  # length
  her<-read_csv(file.path(root,exchangeDir,'Herring',"Herring_length.csv")) %>% rename(SMS_area=sub_area)
  spr<-read_csv(file.path(root,exchangeDir,'Sprat',"sprat_length.csv")) %>% rename(SMS_area=sub_area)
  cod<-read_csv(file.path(root,exchangeDir,'Cod',"cod_length.csv")) %>% rename(SMS_area=sub_area)
  all<-bind_rows(bind_rows(cod,her),spr)
  write_csv(all,file.path(root,exchangeDir,paste0('mean_l_',keyLabel,'.csv')))
  
  
  # consumption
  cod <- read_csv(file.path(root,exchangeDir,'Cod',"cod_consum.csv"))
  write.csv(select(cod, year,species,quarter,age,SMS_area,CONSUM),file.path(root,exchangeDir,paste0('consum_',keyLabel,'.csv')))

} 

# Step 2.
#####################################################################################
# make basis input catch file for the run

# catches
CAT.01<-read.csv(file=file.path(root,exchangeDir,paste0('VPA_Ca01_',keyLabel,'.csv')))
CAT.01$cat<-NULL
head(CAT.01)
save(CAT.01,file=file.path(root,exchangeDir,'CAT_01.Rdata'))

# Biological data (Wsea,propmat, M and M1, proportion M2)
BIO.01<-read.csv(file=file.path(root,exchangeDir,paste0('VPA_Bi01_',keyLabel,'.csv')))

if (FALSE) by(BIO.01,list(BIO.01$species),function(x) {
  X11(h=10,w=15)
  x$time=x$year+0.125+(x$quarter-1)*0.25
  print(ggplot(x,aes(time,WSEA)) +
          theme_bw() +
          geom_point() +
          geom_smooth(method = "loess") +
          #            facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter), scale="free_y") +
          facet_wrap(~ paste0(species,' Age:',age,' Q:',quarter),scale="free_y") +
          labs(x="Time", y="Weight",title=""))
})

save(BIO.01,file=file.path(root,exchangeDir,'BIO_01.Rdata'))

# Other predators
if (otherPredExist) {
  BIO.02<-read.csv(file=file.path(root,exchangeDir,paste0('VPA_Bi02_',keyLabel,'.csv')))
  save(BIO.02,file=file.path(root,exchangeDir,'BIO_02.Rdata'))
}

# Biological data (length at age in the sea)
meanL<-read.csv(file=file.path(root,exchangeDir,paste0('mean_l_',keyLabel,'.csv')))
save(meanL,file=file.path(root,exchangeDir,'meanL.Rdata'))


# consumption)
consum<-read.csv(file=file.path(root,exchangeDir,paste0('consum_',keyLabel,'.csv')))
save(consum,file=file.path(root,exchangeDir,'consum.Rdata'))



writeLists<-function(years=my.years) {
  # BIO 01
  load(file.path(root,exchangeDir,'BIO_01.Rdata'),verbose=T) 
  BIO.01<-subset(BIO.01,year %in% years)
  write.table(BIO.01,file=file.path(root,exchangeDir,paste0('VPA_Bi01_',keyLabel,'.IN')),row.names = F,quote = T,sep=',')

  # BIO 02
  if (otherPredExist) {
    load(file.path(root,exchangeDir,'BIO_02.Rdata'),verbose=T) 
    BIO.02<-subset(BIO.02,year %in% years)
    write.table(BIO.02,file=file.path(root,exchangeDir,paste0('VPA_Bi02_',keyLabel,'.IN')),row.names = F,quote = T,sep=',')
  }
 
  # length
  load(file.path(root,exchangeDir,'meanL.Rdata'),verbose=T) 
  meanL<-subset(meanL,year %in% years)
  write.table(meanL,file=file.path(root,exchangeDir,paste0('mean_l_',keyLabel,'.dat')),row.names = F,quote = T,sep=',')

  
  # CAT 01
  load(file.path(root,exchangeDir,'CAT_01.Rdata'),verbose=T) 
  CAT.01<-subset(CAT.01,year %in% years)
  write.table(CAT.01,file=file.path(root,exchangeDir,paste0('VPA_Ca01_',keyLabel,'.IN')),row.names = F,quote = T,sep=',')
  
  # consumption
  load(file.path(root,exchangeDir,'consum.Rdata'),verbose=T) 
  consum<-subset(consum,year %in% years)
  write.table(consum,file=file.path(root,exchangeDir,paste0('consum_',keyLabel,'.dat')),row.names = F,quote = T,sep=',')
  
  
}

writeLists()



source(file.path(prog.path,RexchangeDir,'From_list_to_SMS_format.R')) # just the function


if (TRUE) {
  code.name<-c('OTH','COD','HER','SPR')
  code.name.pred<- c("COD")
  code.name.prey<- c("OTH",'COD','HER','SPR')
  min.stomach.sampled.in.stratum<-10
  
  #you have to use selected.years or year.q as stomach selection criterion
  #
  selected.years<-NULL            # use this if year.c ( defined below) is used
  selected.years<-c(1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994)
  selected.years<-1974:2014
  
  year.q<-c(
    "1977q1", "1977q2", "1977q3", "1977q4",
    "1978q1", "1978q2", "1978q3", "1978q4",
    "1979q1", "1979q2", "1979q3", "1979q4",
    "1980q1", "1980q2", "1980q3", "1980q4",
    "1981q1", "1981q2", "1981q3", "1981q4",
    "1982q1", "1982q2", "1982q3", "1982q4",
    "1983q1", "1983q2", "1983q3", "1983q4",
    "1984q1", "1984q2", "1984q3", "1984q4",
    "1985q1", "1985q2", "1985q3", "1985q4",
    "1986q1", "1986q2", "1986q3", "1986q4",
    "1987q1", "1987q2", "1987q3", "1987q4",
    "1988q1", "1988q2", "1988q3", "1988q4",
    "1989q1", "1989q2", "1989q3", "1989q4",
    "1990q1", "1990q2", "1990q3", "1990q4",
    "1991q1", "1991q2", "1991q3", "1991q4",
    "1992q1", "1992q2", "1992q3", "1992q4",
    "1993q1", "1993q2", "1993q3", "1993q4",
    "1994q1", "1994q2", "1994q3", "1994q4")
  
  # better quality stomachs!
  year.q<-c(
    "1977q2",
    "1978q1",                     "1978q4",
              "1979q2", "1979q3", "1979q4",
    
    "1981q2", "1981q3", "1981q4",
    "1982q1", "1982q2", "1982q3", "1982q4",
    "1983q1", "1983q2",           "1983q4",
    "1984q1", "1984q2",           "1984q4",
    "1985q1",
    "1986q1",
    "1987q1", "1987q2",           "1987q4",
    "1988q1", "1988q2",           "1988q4",
    "1989q1", "1989q2", "1989q3", "1989q4",
    "1990q2",           "1990q4",
    "1991q1",
    "1992q1",           "1992q3",
    "1993q1",                     "1993q4")
  
  
  # you have to set one of them to NULL
  year.q<-NULL
  #selected.years<-NULL
  
  
  # BALTIC ONLY., rescaling of number of stomachs within groups of size classes
  var.groups.size<-NULL
  if (TRUE) {
    file<-file.path(file.path(root,exchangeDir),paste0('stomcon_list_',dataSets,'.dat')) 
    stom<-read.table(file,header=TRUE,na.strings=".")
    #get range of pred.size.class
    a<-xtabs(~pred.no+pred.size.class,data=stom)
    b<-as.numeric(dimnames(a)[[2]])
    var.groups.size<-cut(b,3,labels=FALSE)
    a[1,]<- var.groups.size
    var.groups.size<-a
    
    # REMEMBER to copy N_haul_at_length_scaled.in into N_haul_at_length.in in the SMS directory
  }
 
  min.stom.groups<-10
  
  
  if (TRUE) {   ## no tails
    SMS.data.transform(list.data.path=file.path(root,exchangeDir),
                       trans.bio=T, trans.catch=T,
                       trans.meanL=T,  trans.meanL.from.weight=F,  trans.stockDist=F,
                       trans.stomach=T, make_incl_stom.in=TRUE,trans.ALK.stomach=TRUE, use.stom.meanl.ALK=FALSE, 
                       trans.other=T, trans.Consum=TRUE,
                       stom.first=1E-08, stom.mid=1E-08, stom.last=1E-08, stom.min.abs=1E-08,delete.tails=TRUE, 
                       inserted.haul.no.propor=1.0, 
                       formatted.output=T,selected.years=selected.years, year.q=year.q,outLabel=keyLabel)
  
  
    }
 
  file.copy(from=file.path(data.path,"incl_stom_master.in"),to=file.path(data.path,"incl_stom.in"),overwrite=TRUE)
  
  if (!is.null(var.groups.size)) {
    file.copy(from=file.path(data.path,"N_haul_at_length.in"),to=file.path(data.path,"N_haul_at_length_copy.in"),overwrite=TRUE)
    file.copy(from=file.path(data.path,"N_haul_at_length_scaled.in"),to=file.path(data.path,"N_haul_at_length.in"),overwrite=TRUE)
    
    
  }
  
  
  ## end Baltic
}

########################################################################################

