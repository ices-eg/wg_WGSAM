# this script make 
#  1. write files on a spreadsheet like format with data extracted from the current environment (you could then update these files with additional years data)
# 2. convert files produced under 1) to files with data on SMS format

# between 1) and 2) you are supposed to update data !
# or use
collate_automatically<-TRUE

alldataSets<-c('old','new','old_and_new',"newY05","newY10","new_Age_1")
dataSet<-alldataSets[6]


if (dataSet=="new_Age_1") first_age_all<-1 else first_age_all<-0 

# start by submitting init.R, so file paths are established

exchangeDir<-"Data_baltic/2022-data"   # directory with data  files on spread sheet format

exchangeDirData<-"Data_baltic/2022-data"   # directory with data  files on spread sheet format

RexchangeDir<-"MakeANewEnvironment"     # directory with R scripts to convert files etc.
thisYearR<-"Baltic_2022"                # directory with R scripts to specific to this year


stomDir<-file.path("C:","_C_drev","Stomach_compilation","Baltic","run_2022")

keyLabel<-dataSet
otherPredExist<-TRUE
my.years<-1974:2021
update_stomachs<-FALSE
cod_prey_as_other_food<-TRUE

options(stringsAsFactors = FALSE)

do_plot<-FALSE
do_loess<-TRUE



# source(file.path(prog.path,RexchangeDir,thisYearR,"make_new_length_classess.R")) # Update length classes with new years, has to be done only once


#  first 1.


if (FALSE) {  # write a number of of files on a spreadsheet like format with data extracted from the current environment (should be an old source run)
              # do not do that for 2019 files (but copy previously made csv files)
  source(file.path(prog.path,RexchangeDir,'from_sms_format_to_list.r'))
 
  my.code.name<-c('COD','HER','SPR')
   From_SMS_format_to_list(otherPredExist=otherPredExist,catchMultiplier=1,code.name=my.code.name,exchangeDir=file.path(root,exchangeDir),addfn=keyLabel)
}

library(tidyverse)
library(reshape2)

library(stockassessment)


if (TRUE) {  #update SMS input files
  
  
  read.ices<-function(filen){
    # Function to read ices data files
    head<-scan(filen, skip=1, n=7, quiet=TRUE)
    minY<-head[3]
    maxY<-head[4]
    minA<-head[5]
    maxA<-head[6]
    C<-as.matrix(read.table(filen, skip=5, header=F))
    C<-C[,1:(maxA-minA+1)]
    rownames(C)<-minY:maxY
    colnames(C)<-minA:maxA
    return(C)
  }
  
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
  
  a<-  as_tibble(read.csv(file=file.path(root,exchangeDir,'Herring',"CANUM by quarter and SD.csv"))) %>% 
    filter(quarter %in% as.character(1:4) & Subdivision != "SD 28.1/GOR" & Subdivision != "Total") %>% 
    dplyr::select(year,quarter,Subdivision,age_0,age_1,age_2,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10)
  
  a<-   reshape(a,direction='long',varying=list(4:14),idvar=c("year","quarter","Subdivision"),v.names='CATCHN')
  canum<-  a %>% mutate(age=time-1,CATCHN=CATCHN*1000,time=NULL) 
  tapply(canum$CATCHN,list(canum$year,canum$age),sum,na.rm=TRUE)
  
  a<-  as_tibble(read.csv(file=file.path(root,exchangeDir,'Herring',"WECA by quarter and SD.csv"))) %>% 
    filter(quarter %in% as.character(1:4) & Subdivision != "SD 28.1/GOR" & Subdivision != "Total") %>% 
    dplyr::select(year,quarter,Subdivision,age_0,age_1,age_2,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10)
  
  a<-   reshape(a,direction='long',varying=list(4:14),idvar=c("year","quarter","Subdivision"),v.names='WCATCH')
  weca<-  a %>% mutate(age=time-1,WCATCH=WCATCH/1000,time=NULL) 
  tapply(weca$WCATCH,list(weca$year,weca$age),sum,na.rm=TRUE) # make no sense!
  
  
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
              dplyr::select(year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT)
  
  
  g<-weca %>% mutate(cohort=year-age, alder=age+as.numeric(quarter)/4-0.125)
   
  write.csv(her.catch,file.path(root,exchangeDir,'Herring',"herring_new_catch.csv"))
  
  # bio data
  her<- her.catch %>% dplyr::select(year, species, quarter, sub_area ,age ,WCATCH ) %>% 
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
               dplyr::select(year,species,quarter,age,sub_area,PROPMAT,M,M1,PROP_M2)
  her.bio<-full_join(her,her.bio) %>%    filter(age <=plus_group) 
  
  group_0<-her.bio$age==0
  her.bio[ group_0,'PROPMAT']<-0
  her.bio[ group_0,'M1']<-0.025
  her.bio[ group_0,'M']<-0.05
  her.bio[ group_0,'PROP_M2']<-1
  her.bio<- her.bio %>% dplyr::select(year,species,quarter,age,sub_area,WSEA,PROPMAT,M,M1,PROP_M2)

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
  a<-  as_tibble(read.csv(file=file.path(root,exchangeDir,"Sprat","Baltic sprat CANUM by QUARTER and SD.csv"),sep=','))
  b<-  as_tibble(read.csv(file=file.path(root,exchangeDir,"Sprat","Baltic sprat WECA by QUARTER and SD.csv"),sep=','))
  spr<-full_join(a,b) %>% rename(year=Year,area=Subdivision,quarter=Quarter,age=Age,CATCHN=Number_millions,WCATCH=Mean_weight.grams) %>%
        mutate(CATCHN=CATCHN*1000,WCATCH=WCATCH/1000)
  
  
  a<-  as_tibble(read.csv(file=file.path(root,exchangeDir,"Sprat","sprat_CANUM_WECA_2019_2021.csv"),sep=',')) %>% 
    filter(quarter %in% as.character(1:4) & Subdivision %in% as.character(22:32)) %>% 
    dplyr::select(year,quarter,Subdivision,vari,age_0,age_1,age_2,age_3,age_4,age_5,age_6,age_7,age_8,age_9,age_10)
  
  x<-dplyr::select(a,year,quarter,Subdivision,vari); x[duplicated(x),]
  
  a<-   reshape(a,direction='long',varying=list(5:15),idvar=c("year","quarter","Subdivision","vari"),v.names='number')
  
  
  a<-  a %>% mutate(age=time-1,time=NULL) 
  canum<-filter(a,vari=='Numbers') %>% mutate(CATCHN=number*1000,number=NULL,vari=NULL,area=Subdivision,Subdivision=NULL) 
  
  weca<-filter(a,vari=='Weight') %>% mutate(WCATCH=number/1000,number=NULL,vari=NULL,area=Subdivision,Subdivision=NULL) 
  
  b<-full_join(canum,weca)
  spr<-rbind(spr,b) %>% mutate(age=as.integer(age),quarter=as.integer(as.numeric(quarter)))

  
  plus_group<-as.integer(7)
  spr <- spr %>% mutate(SOP=CATCHN*WCATCH, age=if_else(age>plus_group,plus_group,age)) %>%
    group_by( year, quarter, age) %>% summarise(sum_sop=sum(SOP,na.rm=TRUE),CATCHN=sum(CATCHN,na.rm=TRUE)) %>%
    filter(CATCHN>0) %>%
    mutate(WCATCH=sum_sop/CATCHN,sum_sop=NULL,PROP_CAT=1,cat='A',sub_area=1,species='SPR')
  
  
  old<-as_tibble(read.csv(file=file.path(root,exchangeDir,"old_VPA_CA01_.csv"),sep=',')) %>%
    filter(species=='SPR') 
  
  
  keep_data <-  anti_join(x=old,y=spr,by = c("species","year", "quarter", "age", "sub_area"))
  spr.catch<- bind_rows(keep_data,spr) %>% arrange(species, year,quarter, sub_area, cat, age ) %>%
    dplyr::select(year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT) %>%    filter(age <=plus_group) 
  
  
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
  spr<- spr.catch %>% dplyr::select(year, species, quarter, sub_area ,age ,WCATCH ) %>% 
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
  M<-read_ices(fileName=file.path(root,exchangeDir,'Sprat',"Msms20cm.txt"),varName='M',fac=1/4,addQuarter = TRUE)
  
  spr.bio<-full_join(propMat,M) %>% mutate(species='SPR',M1=0.05,PROP_M2=1,sub_area=1) %>%
    dplyr::select(year,species,quarter,age,sub_area,PROPMAT,M,M1,PROP_M2)
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
  spr.bio<- spr.bio %>% dplyr::select(year,species,quarter,age,sub_area,WSEA,PROPMAT,M,M1,PROP_M2)
  
  
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
  
  
  ################################################## 
  # cod as other predator
  if (FALSE) {
    #install.packages("r4ss")
    library(r4ss)
    # it's useful to create a variable for the directory with the model output
    mydir <- file.path(root,exchangeDir,'Cod','bfasFinal','bfasFinal')
    
    # read the model output and print diagnostic messages 
    rep <- SS_output(dir = mydir, 
                         verbose = TRUE,
                         printstats = TRUE)
    SS_plots(rep)
    str(rep,2)
  }
  
  #ss3<-file.path(root,exchangeDir,'Cod','Report.sso')
  ss3<-file.path(root,exchangeDir,'Cod','Report_1_space.sso') # report file where two spaces "  " are replaced by one space " "
  
  read_ss3_rep<-function(table="BIOMASS_AT_LENGTH",n=2){
   a<-read_lines(ss3) 
   start<-grep(table,a)[n]+1
   start
   empty<-unlist(lapply(a,nchar))
   empty<-which(empty==0)
   ends<-empty[which(empty>start)[1]]-1
   a<-read_delim(file=ss3,delim=' ',skip=start-1,n_max=ends-start)
   nb<-names(a)
   nb[grep("Beg/Mid",nb)]<-'Beg_Mid'
   names(a)<-nb
   
   return(a)
 
  }
  
 # BIOMASS_AT_AGE
  b<- read_ss3_rep(table="BIOMASS_AT_AGE",n=2) 
  b<-b %>% filter(Beg_Mid=="M") %>% 
    mutate(Area=NULL,Bio_Pattern=NULL,Sex=NULL,BirthSeas=NULL,Settlement=NULL,Era=NULL, Beg_Mid=NULL,Platoon=NULL,Morph=NULL,Time=NULL) %>%  rename(year=Yr,quarter=Seas) %>%
    filter(year>=1974 & year<=2021)
  b
  dim(b)
  
  ba21<-   reshape(b,direction='long',varying=list(3:18),idvar=c("year","quarter"),v.names='bio') %>%
    mutate(age=time-1,time=NULL)
  
  # NUMBERS_AT_AGE
  b<- read_ss3_rep(table="NUMBERS_AT_AGE",n=2)
  b<-b %>% filter(Beg_Mid=="M") %>% mutate(Area=NULL,Bio_Pattern=NULL,Sex=NULL,BirthSeas=NULL,Settlement=NULL,Era=NULL,
                                           Beg_Mid=NULL,Platoon=NULL,Morph=NULL,Time=NULL) %>%  rename(year=Yr,quarter=Seas) %>%
    filter(year>=1974 & year<=2021)
  b
  dim(b)
  
  na21<-   reshape(b,direction='long',varying=list(3:18),idvar=c("year","quarter"),v.names='n') %>%
    mutate(age=time-1,time=NULL)
  
   
  if (do_plot) source(file.path(prog.path.Rexchangedir,thisYearR,"SS3-SMS_BIO.R"))
  
  
  #BIOMASS_AT_LENGTH
  bio<- read_ss3_rep(table="BIOMASS_AT_LENGTH",n=2)
  bio<-bio %>% filter(Beg_Mid=="M") %>% mutate(Area=NULL,Bio_Pattern=NULL,Sex=NULL,BirthSeas=NULL,Settlement=NULL,Era=NULL,
                                          Beg_Mid=NULL,Platoon=NULL,Morph=NULL,Time=NULL) %>%  rename(year=Yr,quarter=Seas) %>%
     filter(year>=1974 & year<=2021)
 dim(bio)
  a<-   reshape(bio,direction='long',varying=list(3:48),idvar=c("year","quarter"),v.names='bio')
  l<-data.frame(time=1:46,Length=as.numeric(names(bio)[3:48])*10)
  
  BIO<-right_join(a,l) %>%mutate(time=NULL,bio=as.numeric(bio))
  round(ftable(tapply(BIO$bio,list(BIO$year,BIO$quarter),sum)))
  
  #numbers
  bio<- read_ss3_rep(table="NUMBERS_AT_LENGTH",n=2)
  bio<-bio %>% filter(Beg_Mid=="M") %>% 
       mutate(Area=NULL,Bio_Pattern=NULL,Sex=NULL,BirthSeas=NULL,Settlement=NULL,Era=NULL,Beg_Mid=NULL,Platoon=NULL,Morph=NULL,Time=NULL) %>%  
       rename(year=Yr,quarter=Seas) %>%
        filter(year>=1974 & year<=2021)
  dim(bio)
  a<-   reshape(bio,direction='long',varying=list(3:48),idvar=c("year","quarter"),v.names='n')
  l<-data.frame(time=1:46,Length=as.numeric(names(bio)[3:48])*10)
  
  N<-right_join(a,l)  %>%mutate(time=NULL,n=as.numeric(n))
  
  
  
  a<-full_join(N,BIO) %>% filter(n>0) %>% mutate(w=bio/n)
   
  tst<-filter(a,year==1974 | year==2021)
  ftable(round(tapply(tst$n,list(tst$year,tst$Length,tst$quarter),sum),1))
  ftable(round(tapply(tst$w,list(tst$year,tst$Length,tst$quarter),sum),3))
  
  
  len_classes<-read_delim(file=file.path(root,exchangeDir,paste0("length_classes_",dataSet,".csv")),delim=',') %>% 
    filter(Species=='Gadus morhua' & year>=1974)
  
  print(filter(len_classes, quarter %in% c(3) & year %in% c(1974,1975)),n=100)


  min(len_classes$no)
  max(len_classes$no)
  
  b <- left_join(x = a, y = len_classes) %>% 
        filter((Length >= l1 & Length < l2)) %>% 
        mutate(l1 = NULL, l2 = NULL) %>% 
        rename(size_class = no, size = group)
  
  
  b<- b %>% group_by(Species,year,quarter, size_class, size) %>%
        summarise(sum_n=sum(n),sum_bio=sum(bio), sum_nl=sum(Length*n)) %>%
        mutate(WSEA=sum_bio/sum_n,mean_l=sum_nl/sum_n,N=sum_n) %>%
        mutate(sum_n=NULL, sum_bio=NULL,sum_nl=NULL)
  
  print(filter(b, quarter %in% c(3) & year %in% c(1974,1975)),n=100)
  
  tst<-filter(b,year %in% c(1974,1975))
  ftable(round(tapply(tst$WSEA,list(tst$year,tst$quarter,tst$size),sum),3))
  ftable(round(tapply(tst$mean_l,list(tst$year,tst$quarter,tst$size),sum),0))
  
  cod<- ungroup(b) %>% mutate(species='COD',sub_area=1,age=size_class) %>%
         dplyr::select(year,species,quarter,age,sub_area,N,WSEA,mean_l)

  
  tst<-filter(cod,year %in% c(1974,1975))
  ftable(round(tapply(tst$WSEA,list(tst$year,tst$quarter,tst$age),sum),3))
  ftable(round(tapply(tst$mean_l,list(tst$year,tst$quarter,tst$age),sum),0))
  ftable(round(tapply(tst$N,list(tst$year,tst$quarter,tst$age),sum),0))
  
  ftable(round(tapply(tst$N*tst$WSEA,list(tst$year,tst$quarter),sum),0))
  tst<-filter(BIO,year %in% c(1974,1975))
  round(ftable(tapply(tst$bio,list(tst$year,tst$quarter),sum)))
  
  
    
  write.csv(dplyr::select(cod,-mean_l),file.path(root,exchangeDir,'Cod',"cod_new_bio.csv"),row.names = FALSE)
  write.csv(dplyr::select(cod,-mean_l),file.path(root,exchangeDir,paste0("VPA_Bi02_",keyLabel,".csv")),row.names = FALSE)
  write.csv(cod,file.path(root,exchangeDir,paste0("cod_w_l_",keyLabel,".csv")),row.names = FALSE)
  
  cod$mean_l<- round(cod$mean_l)
  write.csv(dplyr::select(cod,-N,-WSEA,),file.path(root,exchangeDir,'Cod',"cod_length.csv"),row.names = FALSE)

  
  #consumption
  con<-read.csv(file.path(root,exchangeDir,'Cod',"Average QUARTERLY consum param.csv"))
  con$species<-'COD'
  cod<-as_tibble(cod)
  con<-as_tibble(con)
  a<-left_join(cod,con) %>% filter(year>=year.start & year<=year.stop) %>% mutate(CONSUM=a*(mean_l/10)**b /1000,SMS_area=1 )
  
  tst<-filter(a,year %in% c(1984,1990,2000,2021))
  cat("Consumption\n");ftable(round(tapply(tst$CONSUM,list(tst$year,tst$quarter,tst$age),sum),3))
  
  
  write.csv(dplyr::select(a, year,species,quarter,age,SMS_area,CONSUM),file.path(root,exchangeDir,'Cod',"cod_consum_error.csv"),row.names = FALSE)
  
  ##
  
  prop<-read.csv(file.path(root,exchangeDir,'Cod',"Quarterly_consumption_proportions.csv"))
  prop<-melt(prop,id=1:4)
  prop$quarter<-as.numeric(substr(prop$variable,2,2)); 
  prop<-mutate(prop,variable=NULL,species='COD')
  head(prop)
  head(a)
  dim(prop);dim(a)
  ap<-left_join(dplyr::select(a,c(-a,-b)),prop) %>% filter(mean_l >=l.start & mean_l<=l.stop) %>%
    mutate(l.start=NULL,l.stop=NULL, CONSUM2=CONSUM*4*value)
  dim(ap)
  head(ap)
  
  
  #plot(ap$CONSUM,ap$CONSUM2,col=ap$quarter,pch=as.character(ap$quarter),xlab='old ration (kg/quarter)',ylab='new ration (kg/quarter)')
  #abline(a=0,b=1,col='black')
  ap$CONSUM<-ap$CONSUM2
  write.csv(dplyr::select(ap, year,species,quarter,age,SMS_area,CONSUM),file.path(root,exchangeDir,'Cod',"cod_consum.csv"),row.names = FALSE)
  
  
  
  ####
  rm(M)
  rm(propMat)
}

###   HERE, YOU SHOULD UPDATE THE CSV FILES produced above manually (if needed) before YOU GO ON !!!!!!!!!!!!!!!!!!!

if (collate_automatically) {
  
  # catches
  her<-read_csv(file.path(root,exchangeDir,'Herring',"herring_new_catch.csv")) %>%
           dplyr::select( year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT) %>% 
           rename(SMS_area=sub_area)
  
  spr<-read_csv(file.path(root,exchangeDir,'Sprat',"sprat_new_catch.csv")) %>%
    dplyr::select( year,species,quarter,sub_area,cat,age,WCATCH,CATCHN,PROP_CAT) %>% 
    rename(SMS_area=sub_area)
  
  a<-bind_rows(her,spr) %>% dplyr::select( year,species,quarter,SMS_area,cat,age,WCATCH,CATCHN,PROP_CAT)  %>% filter(age>=first_age_all)
    
  write_csv(a,file.path(root,exchangeDir,paste0('VPA_Ca01_',keyLabel,'.csv')))
  
  # BIO1
  her<-read_csv(file.path(root,exchangeDir,'Herring',"herring_new_bio.csv")) %>%
    dplyr::select( year,species,quarter,sub_area,age,WSEA,PROPMAT,M,M1,PROP_M2) %>% 
    rename(SMS_area=sub_area)
  
  spr<-read_csv(file.path(root,exchangeDir,'Sprat',"sprat_new_bio.csv")) %>%
    dplyr::select( year,species,quarter,sub_area,age,WSEA,PROPMAT,M,M1,PROP_M2) %>% 
    rename(SMS_area=sub_area)
  
  a<-bind_rows(her,spr) %>% dplyr::select( year,species,quarter,SMS_area,age,WSEA,PROPMAT,M,M1,PROP_M2)  %>% filter(age>=first_age_all)
 
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
  all<-bind_rows(bind_rows(cod,her),spr)  %>% filter(age>=first_age_all)
  write_csv(all,file.path(root,exchangeDir,paste0('mean_l_',keyLabel,'.csv')))
  
  
  # consumption
  cod <- read_csv(file.path(root,exchangeDir,'Cod',"cod_consum.csv"))
  write.csv(dplyr::select(cod, year,species,quarter,age,SMS_area,CONSUM),file.path(root,exchangeDir,paste0('consum_',keyLabel,'.csv')))

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

#######################
#make ALK 

#read length at ages in (mm)
a<-read_csv(file.path(root,exchangeDir,paste0('mean_l_',keyLabel,'.csv')))
a %>% filter(species=='HER' & year==1974 & quarter==1)


if (FALSE) {
  tst<-filter(a,age<3 &species !='COD')
  round(ftable(tapply(tst$mean_l,list(tst$age,tst$quarter,tst$species),mean)),1)
  round(ftable(tapply(tst$mean_l,list(tst$age,tst$quarter,tst$species),min)),1)
  round(ftable(tapply(tst$mean_l,list(tst$age,tst$quarter,tst$species),max)),1)
}


# filter(a,species=='COD' & year==1979 & quarter==2)
# filter(a,species=='COD' & year==1979 & age==0)

# mean of CV of mean length at age by year and quarter derived from H:\Pandora\R_key_run_2019
#  AGE                  0    1    2    3    4    5    6    7    8
#Clupea harengus   1    NA 0.14 0.09 0.11 0.13 0.13 0.13 0.13 0.12
#                  4  0.12 0.10 0.14 0.16 0.16 0.15 0.13 0.13 0.11
#Gadus morhua      1    NA 0.32 0.24 0.21 0.18 0.17 0.17 0.16 0.19
#                  4  0.34 0.25 0.22 0.18 0.18 0.17 0.18 0.18 0.18
#Sprattus sprattus 1    NA 0.12 0.08 0.09 0.08 0.08 0.07 0.08 0.08
#                  4  0.10 0.08 0.08 0.08 0.07 0.07 0.06 0.07 0.07

# smoothed values (used) of the table above
cod<- data.frame(species='COD',age=0:8,cv=c(0.34,0.25,0.23,0.20,rep(0.18,5))) # cod is not used !
her<- data.frame(species='HER',age=0:8,cv=rep(0.13,9))  
spr <-data.frame(species='SPR',age=0:7,cv=c(0.1,0.1,rep(0.08,5),0.08))  



sdR<-rbind(cod,her,spr)


intersect(names(a), names(sdR))
a<-merge(a,sdR)
head(a)
summary(a)
# length in cm
ll<-rbind(
  data.frame(species='COD',ll=c(seq(5,150,1))),
  data.frame(species='HER',ll=c(seq(5, 45,1))),
  data.frame(species='SPR',ll=c(seq(5, 24,1)))
)

head(ll)
intersect(names(a), names(ll))
al<-merge(a,ll) %>% mutate(mean_l=mean_l/10) %>% mutate(sd=mean_l*cv)
dim(al)

head(arrange(al,species,year,quarter,age,mean_l,ll))

# please note that ll must by sequences by 1
# al<-al %>% group_by(species,year,quarter,age,mean_l) %>% mutate(prob=dnorm(ll, mean = mean_l, sd = sd))
al$prob<-dnorm(al$ll, mean = al$mean_l, sd = al$sd)

head(arrange(al,species,year,quarter,age,mean_l,ll))


# check should be 1 (except for the very small (young) fish)
al %>% group_by(species,year, quarter,age) %>% summarise(check=sum(prob)) 

al<-rename(al,"code"="species")

# put pred_size on 
l<-read_delim(file=file.path(root,exchangeDir,paste0("length_classes_",dataSet,".csv")),delim=',')%>% rename(size_class=no,size=group)
sp_info<- readr::read_csv(file= file.path(stomDir,"config",paste0("species_info_",dataSet,".csv")),col_types = readr::cols()) %>% dplyr::select(code,species,number)
l<-right_join(x=l,y=sp_info,by=c('Species'='species'))
l
head(al)
al$ll<-al$ll*10 #into mm

minimum_alk<-0.0005
if (FALSE) { #test
  tst<-left_join(al,l,by = c("code", "year", "quarter"))%>% 
    filter(ll >= l1 & ll <l2 & prob>0) 
  
  filter(tst,code=='COD'&quarter==1 &year==1974)
  filter(tst,code=='SPR' &quarter==1 &year==1974)
  filter(tst,code=='SPR' & age==0 & quarter==3 & year==1974)
  filter(tst,code=='HER')
}

alk<- left_join(al,l,by = c("code", "year", "quarter")) %>% 
  filter(ll >= l1 & ll <l2 & prob>0)  %>% 
  dplyr::select( -l1, -l2) %>% 
  group_by(year,quarter,code,number,Species,size,size_class,age) %>% 
  summarise(ALK=sum(prob),ALK.MeanL=round(sum(prob*(ll+5)/ALK))) %>%
  filter(ALK>=minimum_alk) %>%
  mutate(SMS_area='1')

if (FALSE) {
  alk
  
  filter(alk,code=='COD'& quarter==1 &year==1974)
  filter(alk,code=='SPR' & quarter==1 &year==1974)
  filter(alk,code=='HER')
}

# check should be 1 (except for the very small (young) fish)
alk %>% group_by(Species,year, quarter,age) %>% summarise(check=sum(ALK)) 

#rescale ALK
alk <- alk %>% group_by(Species,year, quarter,age) %>% mutate(ALK=ALK/sum(ALK)) 

if (FALSE) {
  df<-filter(alk,year==1974)%>% mutate(size=substr(size,2,4),age=paste('Age:',age),quarter=paste0("Q:",quarter))
  
  by(df,df$Species,function(x) {
    X11()
    p<-ggplot(data=x, aes(x=size, y=ALK)) +labs(title=x[1,"Species"])+
    geom_bar(stat="identity", fill="steelblue")+facet_grid(rows=vars(age),cols=vars(quarter))+theme_minimal()
  print(p)
  })

}

write.table(alk,file=file.path(root,exchangeDir,paste0('ALK_raw_',keyLabel,'.dat')),row.names = F,quote = T,sep=',')

if (FALSE) {
  ftable(xtabs(~quarter+code+size,data=subset(alk,code=='HER')))
  ftable(xtabs(~quarter+code+size,data=subset(alk,code=='SPR')))
  xtabs(~year+quarter,data=filter(alk,code=='HER' & size=="0050-0069"))
}


######### stomachs
# made by "_job_to_do_the_whole_shit.R" in C:\_C_drev\Stomach_compilation\Baltic\run_2022\R_prog
if (update_stomachs) source( 'C:/_C_drev/Stomach_compilation/Baltic/run_2022/R_prog/_job_to_do_the_whole_shit.R')

library(FishStomachs)
load(file=file.path(root,exchangeDir,paste0('diet_raw_',keyLabel,'.Rdata')),verbose=TRUE)
control<-attr(d,'control')
max_l<-control@max_l
other<-control@other

s<-read_csv(file=file.path(root,exchangeDir,paste0('STOM_raw_',keyLabel,'.dat'))) %>%
mutate(SMS_area=as.integer(as.factor(SMS_area)))

s %>% dplyr::select( SMS_area,  year, quarter, pred,  pred.no, pred.size, pred.size.class, pred.mean.length) %>% distinct()

s %>% dplyr::select( SMS_area,  year, quarter, prey,prey.no,prey.size,prey.size.class) %>% distinct() %>%
  dplyr::arrange( SMS_area,  year, quarter, prey.no,prey.size.class)

sort(unique(s$pred.size))


#subset(s,year==1974 & quarter==2)
#subset(s,year==1974 & quarter==2 & pred.size=='0250')


alk<-read_csv(file=file.path(root,exchangeDir,paste0('ALK_raw_',keyLabel,'.dat'))) %>% 
  mutate(SMS_area=as.integer(as.factor(SMS_area)))
alk
alk<- alk %>% mutate(ALK=ALK*100)
alk<-filter(alk,age> SMS.control@first.age | (age==SMS.control@first.age & quarter>=SMS.control@rec.season))
filter(alk,year==1974 & quarter==2 & code=='COD' & size=='0250-0299')
if (cod_prey_as_other_food) {
  alk<- filter(alk,Species !="Gadus morhua") 
}



if (FALSE) {
  cleanup()
  df<-filter(alk,year==1974)%>% mutate(size=substr(size,2,4),age=paste('Age:',age),quarter=paste0("Q:",quarter))
  
  by(df,df$Species,function(x) {
    X11()
    p<-ggplot(data=x, aes(x=size, y=ALK)) +labs(title=x[1,"Species"])+
      geom_bar(stat="identity", fill="steelblue")+facet_grid(rows=vars(age),cols=vars(quarter))+theme_minimal()
    print(p)
  })
  
}

#tail(filter(alk,year==1975,quarter==3))

# what is actually needed for stomach observations ?
used_area_year_quarter_combinations<-dplyr::select(s,SMS_area,year,quarter) %>% distinct()


alk<-right_join(alk,used_area_year_quarter_combinations)

unique(dplyr::select(s,pred,pred.size,pred.size.class,prey.size,prey,prey.size.class,prey.size.range))
unique(dplyr::select(alk,Species,code,size,size_class))

# check that all length classes in the diet set can be found in ALK
aa<-filter(s,prey !=other) %>% transmute(pred=pred,pred.size=pred.size,pred.size.class=pred.size.class,year=year,quarter=quarter,Species=prey,size=prey.size.range,size_class=prey.size.class,stomcon=stomcon) %>% 
     distinct() 
bb<-left_join(aa,alk, by = c("year", "quarter", "Species", "size_class"))  
misALK<-filter(bb,is.na(ALK))
aa %>% dplyr::select(year, quarter,Species,size,size_class ) %>% distinct()

print(misALK)
by(misALK,misALK$Species,function(x){
ftable(xtabs(~quarter+size.x,dat=x))
})
  print(filter(misALK,Species=='Clupea harengus' & size.x=="0070-0084"),n=100)

# just ignore (delete) these records with a small (0.05) contents
s_ignor<-filter(misALK,stomcon<=0.05) %>% 
  transmute(pred=pred,pred.size=pred.size,year=year,quarter=quarter,prey=Species,prey.size.range=size.x) %>% distinct()
s_ignor
dim(s)
s<-anti_join(s,s_ignor)
dim(s)
s<-s %>% group_by(SMS_area,year,quarter,pred,pred.size) %>% mutate(stomcon=stomcon/sum(stomcon)) %>%ungroup()
dim(s)

#delete all records for the given predator 
s_del<-filter(misALK,stomcon>0.05) %>% 
  transmute(pred=pred,pred.size=pred.size,year=year,quarter=quarter) %>% distinct()
s_del
dim(s)
s<-anti_join(s,s_del)  #ignore stomcon does not sum up to 1
dim(s)



first_age<-first_age_all
if (first_age_all==0) first_quarter_first_age<-3 else first_quarter_first_age<-1;


outALK<-ungroup(alk) %>% dplyr::select(SMS_area=SMS_area,
                                year=year,
                                quarter=quarter,
                                prey=code,
                                prey.no=number,
                                prey.age=age,
                                prey.size.class=size_class,
                                prey.size=size,
                                ALK=ALK,
                                ALK.MeanL=ALK.MeanL) %>%
  group_by(SMS_area,
           year, 
           quarter,
           prey.no,
           prey.size.class,
           prey.age) %>%ungroup()


write.table(outALK,file=file.path(root,exchangeDir,paste0("ALK_stom_list_",dataSet,".dat")),row.names = FALSE,quote = FALSE)

####

sp_info<- readr::read_csv(file= file.path(stomDir,"config",paste0("species_info_",dataSet,".csv")),col_types = readr::cols()) %>% dplyr::select(pred_code=code,species)
sp_info
s<-right_join(x=s,y=sp_info,by=c('pred'='species'))
s

sp_info<-sp_info %>%rename(prey_code=pred_code)
sp_info
s<-right_join(x=s,y=sp_info,by=c('prey'='species'))
s
# filter(s,year==1974,quarter==2)

out<-s %>% dplyr::select(SMS_area,
                  year,
                  quarter,
                  pred=pred_code,
                  pred.no,
                  pred.size,
                  pred.size.class,
                  pred.mean.length,
                  prey=prey_code,
                  prey.no,
                  prey.size,
                  prey.size.class,
                  prey.mean.length,
                  stomcon,
                  type,
                  mean.weight,
                  haul.no,
                  haul.prey.no) %>%
  group_by(SMS_area,
           year, 
           quarter,
           pred.no,
           pred.size,
           prey.no,
           prey.size) %>%
   filter(!is.na(pred.no))%>% ungroup()

# tail(out)
# sort(unique(out$pred.size))
# filter(out,year==1979 & quarter==2)
# filter(out,year==1974 & quarter==1)

#outOK<-out; out<-outOK

# check if there are "wholes" in the predator, predator size
check<-out %>% dplyr::select( SMS_area,  year, quarter, pred,  pred.no, pred.size, pred.size.class, pred.mean.length) %>% 
  distinct() %>% 
  group_by( SMS_area,year,quarter,pred,  pred.no) %>% summarize(mins=min( pred.size.class), maxs=max(pred.size.class), ns=dplyr::n())%>%
  mutate(OK= (maxs-mins+1)==ns) %>% filter(!OK)
check

check<-left_join(dplyr::select(check,SMS_area,year,quarter,pred,  pred.no,mins,maxs),out) %>%
        dplyr::select(SMS_area,  year, quarter, pred,  pred.no, pred.size, pred.size.class, pred.mean.length,mins,maxs) %>%
        distinct() %>%
          arrange(SMS_area,year,quarter,pred,  pred.no)
check

ch2<-check %>%  group_by( SMS_area,year,quarter,pred,pred.no) %>% tidyr::expand(pred.size.class=full_seq(pred.size.class, 1))

ch3<-full_join(check,ch2) %>%  arrange(SMS_area,year,quarter,pred,  pred.no,pred.size.class)
dels<-filter(ch3,is.na(pred.size)) %>% rename(del.size=pred.size.class) %>% mutate(pred.size=NULL,pred.mean.length=NULL,mins=NULL,maxs=NULL)
# filter(dels,year==1979,quarter==2)
dels<-dels %>% group_by(SMS_area,year,quarter,pred,pred.no) %>% summarize(del.size=min(del.size)) %>%ungroup()
filter(dels,year==1979,quarter==2)

out<-left_join(out,dels)
filter(out,!is.na(del.size))
filter(out,year==1974 & quarter==1)

out<-out %>% filter(is.na(del.size) | (!is.na(del.size) & pred.size.class>del.size))


filter(out,year==1979 & quarter==2)
filter(out,year==1974 & quarter==2)
#ftable(xtabs(~year+quarter+pred.size ,data=out))


# check AGAIN if there are "wholes" in the predator, predator size
out %>% dplyr::select( SMS_area,  year, quarter, pred,  pred.no, pred.size, pred.size.class, pred.mean.length) %>% 
  distinct() %>%
  group_by( SMS_area,year,quarter,pred,  pred.no) %>% summarize(mins=min( pred.size.class), maxs=max(pred.size.class), ns=dplyr::n())%>%
  mutate(OK= (maxs-mins+1)==ns) %>% filter(!OK)

out$del.size<-NULL


# check if there are "wholes" in the predator, predator size, prey, prey size
check<-  out %>% dplyr::select( SMS_area,  year, quarter,pred.no, pred, pred.size.class, prey.no,prey,prey.size.class) %>% 
  distinct() %>%
  group_by(SMS_area,  year, quarter,pred.no, pred, pred.size.class, prey.no,prey) %>% 
     summarize(mins=min( prey.size.class), maxs=max(prey.size.class), ns=dplyr::n())  %>%
  mutate(OK= (maxs-mins+1)==ns) %>% filter(!OK) %>%ungroup()
check  # there should be no observations


sort(unique(out$pred.size))

#filter(out,year==1979,quarter==2)
write.table(out,file=file.path(root,exchangeDir,paste0("stomcon_list_",dataSet,".dat")),row.names = FALSE,quote = FALSE)

source(file.path(prog.path,RexchangeDir,'From_list_to_SMS_format.R')) # just the function

xtabs(~year+quarter,data=out)

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
  #please note that year-quarter combinations have already been selected in the stomach compilation
  
  # BALTIC ONLY., rescaling of number of stomachs within groups of size classes
  var.groups.size<-NULL
  if (TRUE) {
    file<-file.path(file.path(root,exchangeDir),paste0('stomcon_list_',dataSet,'.dat')) 
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

fout<-c(
  file.path(data.path,'stomcon_at_length.in'),            # 1
  file.path(data.path,'stomlen_at_length.in'),            # 2
  file.path(data.path,'stomweight_at_length.in'),         # 3
  file.path(data.path,'N_haul_at_length.in'),             # 4
  file.path(data.path,'N_haul_at_length_scaled.in'),      # 5
  file.path(data.path,'stomnumber_at_length.in'),         # 6
  file.path(data.path,'stomtype_at_length.in'),           # 7
  file.path(data.path,'stom_pred_length_at_sizecl.in')    # 8
)
fout

for (i in fout) {
  cat(i,'   ')
  scan(file=i,comment.char = "#")
  
}


if (FALSE) {
    # 2019 key-run
  a<-scan(file=file.path("C:","_C_drev","SMS-git","Baltic-2019-keyRun",'N_haul_at_length.in'),comment.char = "#"); max(a)
  a<-scan(file=file.path("C:","_C_drev","SMS-git","Baltic-2019-keyRun",'N_haul_at_length_copy.in'),comment.char = "#"); max(a)
  
    # 2022
  a<-scan(file=file.path("C:","_C_drev","SMS-git","Baltic-2022",'N_haul_at_length.in'),comment.char = "#"); max(a)
  a<-scan(file=file.path("C:","_C_drev","SMS-git","Baltic-2022",'N_haul_at_length_copy.in'),comment.char = "#"); max(a)
}

