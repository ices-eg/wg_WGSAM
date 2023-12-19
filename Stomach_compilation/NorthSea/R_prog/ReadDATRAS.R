library(DATRAS)

one_year<-1991

if (readNewSurvData) {

  dd<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_NS_IBTS-1990-1999.zip",strict = FALSE)  # use strict=F for a useful ageing information

  p1<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-1987_1999.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p2<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-2014.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p3<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-2015.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p4<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-2016.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p5<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-2017.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p6<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-2018.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p7<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-2019.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p8<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_BTS-2020.zip",strict = FALSE)  # use strict=F for a useful ageing information
  p10<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_SNS_2002-2009.zip",strict = FALSE)##  # use strict=F for a useful ageing information
  p11<-readExchangeDir(path = DatrasDir, pattern ="Exchange Data_SNS_2010-2019.zip",strict = FALSE)  # use strict=F for a useful ageing information

  pp<-c(p1,p2,p3,p4,p5,p6,p7,p8,p10,p11)
  pp<-subset(pp,Species=="Pleuronectes platessa")

   dd<-c(dd,pp)
  dd<-subset(dd,HaulVal=="V")
  #dd<-subset(dd,Year %in% 1991:1997)

  info<-read_csv(file.path(config_dir,"species_info.csv"),col_types = cols())
  sp<-info$species

  dd<-subset(dd,Species %in% sp)
  dd<-subset(dd,Roundfish %in% as.character(1:7))

  save(dd,info, file = file.path(Rdata,"DATRAS_raw00.Rdata"))
  #  load(file.path(Rdata,"DATRAS_raw00.Rdata"),verbose=TRUE)


  xtabs(~Year+Species,data=dd[['CA']])

  if (one_year>0) {  # you have to have at least two years for the structure to work
   dd[['HH']]<-dd[['HH']] %>% mutate(Year=if_else(as.character(Year)>=as.character(one_year),one_year,one_year-1)) %>% mutate(Year=as.factor(Year))
   dd[['HL']]<-dd[['HL']] %>% mutate(Year=if_else(as.character(Year)>=as.character(one_year),one_year,one_year-1))  %>% mutate(Year=as.factor(Year))
   dd[['CA']]<-dd[['CA']] %>% mutate(Year=if_else(as.character(Year)>=as.character(one_year),one_year,one_year-1))  %>% mutate(Year=as.factor(Year))
  }

  xtabs(~Year+Species,data=dd[['CA']])

  sort(unique( dd[['HH']]$Year))
  # change of variable names !!?
 # dd[['CA']]$NoAtALK<- dd[['CA']]$NoAtLngt
  #dd[['CA']]$NoAtLngt<-NULL
  unique(dd[['HL']]$Species)

  info[info$code=='PLE','prey_sp'] <-TRUE
  my.species<-filter(info,prey_sp)
  my.species<-filter(my.species,!(code %in% c("NSA","SSA")))

  my.species<- filter(my.species,code %in% c("WHG","PLE"))

  dd<-subset(dd,Species %in% my.species$SMS_species )

  #dd<-subset(dd,Year %in% c(1991,1992))
  #  dd.ysplit = split(dd,list(dd$Year,dd$Quarter))
  dd.ysplit = split(dd,list(dd$Quarter))

  #put ages on
  source(file.path(R_prog,"ages.R"))
  if (FALSE){
    tst<-subset(dd.ysplit[[1]],Species=="Merlangius merlangus")
    a<-putAgesOn(tst,ages=1:3,testOut=FALSE,doPlot=FALSE)
    head(a)
  }

  tst<-subset(dd.ysplit[[3]],Species=="Pleuronectes platessa")
  a<-putAgesOn(tst,ages=1:3,testOut=FALSE,doPlot=TRUE)
  head(a)

  doALK2<-function(x) {
    #test x<-dd.ysplit[[1]]
    q<-as.character(x[['HH']][1,'Quarter'])
    a<-by(my.species,list(my.species$species),function(my.sp) {
       xsp<-subset(x,Species==my.sp$species)
       if (q<="2") minAge<-1 else minAge<-my.sp$first_age
       maxAge<-my.sp$last_age
       a<-putAgesOn(xsp,ages=minAge:maxAge,testOut=FALSE,doPlot=FALSE,usepredALK=TRUE)
       a$species<-as.character( my.sp$code)
       print(head(a))
       return(a)
     })
    return(do.call("rbind",a))
  }


  ndd<-lapply(dd.ysplit,doALK2)
  # combine them into one
  dd <- do.call("rbind",ndd)
  save(dd, file = file.path(Rdata,"DATRAS_ages.Rdata"))
  #
} else load(file.path(Rdata,"DATRAS_ages.Rdata"),verbose=TRUE)

sort(unique(dd$species))
dd
summary(dd)
sort(unique(dd$Year))
xtabs(~Year,data=dd)
