#do plaice alk

if (FALSE) {
  # make proportions by size group for each age (ALK) from IBTS data
  readNewSurvData<-FALSE
  source(file.path(R_prog,"ReadDATRAS.R"))

  dd<-filter(as_tibble(dd),nl>0) %>% mutate(Year=NULL,age=as.character(Age),Age=NULL) %>%
    mutate(quarter=as.integer(as.character(Quarter)),Quarter=NULL,l=as.numeric(as.character(sizeGroup))*10, sizeGroup=NULL)%>%
    rename(prey=species)


  ifile<-paste0("length_classes_config_",sz,".csv")
  ofile<-paste0("length_classes_",sz,".csv")

  len_classes_in<-make_length_classess(inp_dir=config_dir,inp_file=ifile,out_file=ofile,write_output=FALSE,max_l=control@max_l,minus_one= 0)

  s<-select(len_classes_in,-Species)

  filter(s,quarter==1)
  head(dd)
  s

  a<-full_join(s,dd ,by = "quarter")
  a
  b<-filter(a,(l>=l1 & l<l2) | is.na(l))

  b

  b2<-    b %>%  group_by(year,quarter,prey,age,no,group) %>% summarise(wl=sum((l+5)*nl),   nl=sum(nl)) %>%mutate(meanl=wl/nl,wl=NULL) %>% ungroup()
  b3<-   b2 %>%  group_by(year,quarter,prey,age)      %>% mutate(ALK=nl/sum(nl)) %>% ungroup()

  b2$nl<-NULL

  b2<-right_join(b2,b3)%>% arrange(no,year,quarter,age,group)
  b2
  summary(b2)
  b3<-filter(b2,year %in%  c(1981,1990))
  by(b3,list(b3$prey,b3$year),function(x){
    x<-droplevels(x)
    ftable(round(tapply(x$ALK,list(x$quarter,x$age,x$group),sum),2))
  })

  by(b3,list(b3$prey,b3$year),function(x){
    x<-droplevels(x)
    ftable(round(tapply(x$meanl,list(x$quarter,x$age,x$group),sum),2))
  })

  b3$nl<-NULL
  head(b3)

  b4<-b3 %>% filter(prey=='PLE') %>%
    mutate(l1=as.integer(substr(group,1,4)),l2=as.integer(substr(group,6,9)),Age=as.integer(age),age=NULL,SMS_area=1L,spno=26) %>%
    rename(new_alk=ALK,sizecl=group,sms_l=no,species=prey,length=meanl) %>%
    select( SMS_area,  year ,quarter,  spno, species,   Age, sizecl,    length, sms_l, new_alk,    l1,    l2)
  b4
  b4<-b4 %>% filter(new_alk>0.01) %>% group_by( SMS_area,year, quarter,spno, species,Age) %>%
    mutate(new_alk=new_alk/sum(new_alk))
  sort(unique(b4$year))
  b85 <-b4 %>% filter(year==1981) %>% mutate(year=1985L)
  b02 <-b4 %>% filter(year==1990) %>% mutate(year=2002L)
  b10 <-b4 %>% filter(year==1990) %>% mutate(year=2010L)

  ple_alk<-rbind(b85,b02,b10)

  just<-ple_alk %>% filter(( Age==0 & quarter==3) | ( Age==1 & quarter==1)) %>%
    mutate( Age=0L, quarter=4L) %>%
    group_by(SMS_area, year, quarter,  spno, species,   Age, sizecl,sms_l,   l1 ,   l2) %>%
    summarize(length=weighted.mean(length,new_alk), new_alk=sum(new_alk))  %>%
     group_by(SMS_area, year, quarter,  spno, species,   Age) %>%
       mutate(new_alk=new_alk/sum(new_alk))


  ple_alk<-rbind(filter(ple_alk,!(Age==0 & quarter==4)),just) %>% mutate(new_alk=new_alk*100)

  save(ple_alk,file=file.path(Rdata,'ple_ALK.Rdata'))

  ######################################################

} else  load(file=file.path(Rdata,'ple_ALK.Rdata'),verbose=TRUE)
