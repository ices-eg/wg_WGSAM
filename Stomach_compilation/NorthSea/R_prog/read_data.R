stomach_dir<-file.path(stom_dir,"old-stomachs")

in_file<-
  c(
    "EXCGREDATSTO.995",
    "EXCHORDATSTO.995",
    "EXCmacDATSTO_815.dat",
    "ExcSaidatsto_843.dat",
    "Exccoddatsto_815.dat",
    "Exccoddatsto_8587.dat",
    "Exccoddatsto_915.dat",
    "Exchaddatsto_815.dat",
    "Exchaddatsto_915.dat",
    "Excmacdatsto_915.dat",
    "Excsaidatsto_8083.dat",
    "Excsaidatsto_863.dat",
    "Excsaidatsto_873.dat",
    "Excsaidatsto_915.dat",
    "Excwhidatsto_815.dat",
    "Excwhidatsto_8587.dat",
    "Excwhidatsto_915.dat",
 #     "IMARESdataset.dat", findes kun på nyt ICES format læses ind senere
    "RADIATA_915.dat")




if (convert_data) convert_year_of_the_stomach(dir=stomach_dir,in_file=in_file,write_file=TRUE,verbose=TRUE)
#summary(a[[1]]);str(a[[1]])

#read data in again from exchange format
control<-new("STOMcontrol",
             name='North Sea 2023 Key run',
             stomach_dir=stomach_dir,
             years=as.integer(1980:2013),
             predators=c("Amblyraja radiata",
                         "Eutrigla gurnardus",
                         "Trachurus trachurus",
                         "Phocoena phocoena",
                         "Merluccius merluccius",
                         "Gadus morhua",
                         "Merlangius merlangus",
                         "Melanogrammus aeglefinus",
                         "Pollachius virens",
                         "Scomber scombrus"),
             dataSets=paste0("adjusted_",in_file))




print(control,show=c('general','data_used'))

st<-read_exchange_data(control, delete_errors=TRUE, allow_alias_names = FALSE,  keep_just_mandatory_fields = FALSE)

st
st<-change_data(st,correct_nstom=TRUE,correct_predl=TRUE)

summary(st)

if (FALSE) {
  cod_tst<-subset(st,pred_name=="Gadus morhua" & year==1981& quarter==2 & pred_ll==700)
  cod_tst
  view(cod_tst$PRED)
  filter(cod_tst$PRED,rectangle=='48F3')
}
#mac<-subset(st,pred_name=="Scomber scombrus" &year==1991 & quarter==3)
#mac[['PRED']]
#unique(paste(mac[['PRED']]$ship,mac[['PRED']]$station))

###########################
#read additional data on ICES new format
#NOT USED; AS THERE ARE NO PREY LENGHTS   stomach_dir<-file.path(stom_data_dir,"NorthSea","new-stomachs","EUMare_IMARESdata_update_20150604"); in_file <-c("all_stom.csv")


##### seals
stomach_dir<-file.path(stom_dir,"mammals");
new_seal_data<-TRUE
if (new_seal_data)  in_file<-c("seal_diet_from_Vanessa_nov_2023.csv") else in_file <-c("seal_diet.csv")

if (!new_seal_data) {
  s<-read_csv(file.path(stomach_dir,in_file),col_types = cols())
  summary(s)

  s<-s %>% mutate(
    dataset='seal data',
    record_type='PS',
    country='All',
    area='1',
    lat=55,
    lon=0,
    month=(quarter-1)*3+1,
    day=1,
    haul=quarter,
    station=quarter,
    sample_id=paste('Seal',year,quarter,sep='_'),
    ship='xxx',
    rectangle='40F0',
    pred_name='Halichoerus grypus',
    pred_nodc=8111111111,
    pred_l=1100,
    pred_ll=1000,
    pred_lu=1200,
    CPUE=1,
    fish_id=paste(year,quarter,sep='_'),
    n_food=10,
    n_regur=0,
    n_empty=0,
    n_skel=0,
    n_tot=n_food+n_empty+n_regur+n_skel,
    digest=1,
    prey_name=prey, prey=NULL,
    prey_w=preyw,preyw=NULL,
    #prey_n=nprey, nprey=NULL,
    prey_n=NA, nprey=NULL,
    prey_l=NA,
    prey_w_meth='r',
    prey_ll=lowpreyl*10, lowpreyl=NULL,
    prey_lu=higpreyl*10, higpreyl=NULL)

  s[s$prey_name=='OTH','prey_ll']<-NA
  s[s$prey_name=='OTH','prey_lu']<-NA

  write_csv(s,file=file.path(stomach_dir,paste0('adjusted_',in_file)))

  control<-new("STOMcontrol",
             name='Seals',
             predators= "Halichoerus grypus",
             years=as.integer(c(1985,2002)),
             stomach_dir=stomach_dir,
             dataSets=paste0('adjusted_',in_file))
} else {
  control<-new("STOMcontrol",
               name='Seals',
               predators= "Halichoerus grypus",
               years=as.integer(c(1985,2002,2010)),
               stomach_dir=stomach_dir,
               dataSets=in_file)

}

seal<-read_exchange_data(control, delete_errors=FALSE, allow_alias_names = TRUE,  keep_just_mandatory_fields = FALSE)


if (gse_north_south_sandeel) {
  if (FALSE) {
    san<-read_xlsx(path=file.path(stomach_dir,"Proportion_san_north_south.xlsx"),sheet="Sheet2")
    png(file.path(output_dir,labRdata,paste0('GSE_Proportion_North_south.png')),width=700,height=700,pointsize=18)
     plot(san$year,san$NorthSea,ylim=c(0.10,0.55),xlab='Year',ylab='proportion southern Sandeel')

    san2<-read_xlsx(path=file.path(stomach_dir,"Proportion_san_north_south.xlsx"),sheet="Sheet3")
    lines(san2$year,san2$South,col='red',type='p',pch=2)

    ls<-loess(South~year,data=san2)
    pls<-predict(ls,new=san$year)

    lines(san$year,pls,col='red')
   cleanup()
   north_south_san_GSE<-data.frame(year=san$year,proportion_south=pls)
   oldP<-head(north_south_san_GSE,1)$proportion_south
   for (y in (1974:1983)) north_south_san_GSE<-rbind(north_south_san_GSE,data.frame(year=y,proportion_south=oldP))
  }
 san2<-read_xlsx(path=file.path(stomach_dir,"Proportion_san_north_south.xlsx"),sheet="Sheet3")
 north_south_GSE_quarter <-filter(san2,quarter<=4) %>% select(year,quarter,North,South)
}
#########################

##### H porpoise
stomach_dir<-file.path(stom_dir,"mammals");
in_file <-c("porpoise_diet_2017_AR.csv")

s<-read_csv(file.path(stomach_dir,in_file),col_types = cols())

if (adjust_harborPorpoise) {
  a<-s

  aa<-xtabs(preyw~prey+paste(year,quarter,sep='_'),data=a)
  #aa
  asum<-colSums(aa)
  aa<-sweep(aa, 2, asum, FUN = '/')
  round(rbind(aa,all=colSums(aa)),4)

  small<-a$lowpreyl<=4
  a[small,'prey']<-'OTH'
  a[small,'lowpreyl']<-1
  a[small,'higpreyl']<-2
  a[small,'nprey']<-0

  a<- a %>% group_by(year, quarter, prey,  lowpreyl, higpreyl) %>% summarize(preyw=sum(preyw), nprey=sum(nprey)) %>% ungroup()

  #filter(a,prey=='OTH')

  o<-read_xlsx(path=file.path(porpoise,"Otholiths_porpoise.xlsx"),sheet="data")  %>%
    filter(regression=="Y=a+bx" & !is.na(code)) %>% select(code,   a , b ) %>% rename(prey=code) %>%
    mutate(prey=if_else(prey %in% c('NSA','SSA'),'SAN',prey)) %>% unique()

  ao<-left_join(a,o, by = "prey") %>%  filter(lowpreyl>0) %>%
    mutate(fishLength=(lowpreyl+higpreyl)/2,OL=(fishLength-a)/b, OmassProxy=OL^3, residenceTime=sqrt(OmassProxy),weightingFactor=1/residenceTime)

  #filter(ao,prey=='OTH')
  #filter(ao,is.na(OL))
  #filter(ao,is.na(weightingFactor ))
  # guess weighting factor for other food
  oth<-filter(ao,!is.na(OL)) %>%group_by(year,quarter) %>%
    summarize(othW=weighted.mean(weightingFactor,preyw)) %>%
    ungroup() %>% mutate(prey='OTH')


  aao<-left_join(ao,oth,by = c("year", "quarter", "prey")) %>% mutate(weightingFactor=if_else(is.na(othW),weightingFactor,othW),othW=NULL)
  #filter(aao,prey=='OTH')
  #aao

  #arrange(aao,weightingFactor)
  #arrange(aao,desc(weightingFactor))
  #summary(aao)


  b<-aao %>% group_by(year,quarter,prey) %>%
    summarize(w_preyw=sum(preyw*weightingFactor),  preyw=sum(preyw))  %>%
    group_by(year,quarter) %>%
    mutate(preyw=preyw/sum(preyw), w_preyw=w_preyw/sum(w_preyw))

  bb<-pivot_longer(b,cols=4:5) %>% mutate(method=if_else(name=="w_preyw",'adjusted','observed')) %>% mutate(time=paste0(year,' Q',quarter))


  theme_ben <- function(base_size = 14) {
    theme_bw(base_size = base_size) %+replace%
      theme(
        # changed theme options
      )
  }

  png(filename=file.path(NorthSea,'otholiths.png'),width=1800,height=900,units='px',bg='white')

  ggplot(data=bb, aes(x=prey, y=value, fill=method)) +
    geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(cols=vars(time))+ labs(x='Prey',y='Proportion')+
    theme_ben(base_size = 30)

  x<-dev.off()

  s<-aao%>% mutate(preyw=preyw*weightingFactor,nprey=nprey*weightingFactor) %>%
    select( year, quarter, prey,  lowpreyl, higpreyl, preyw, nprey)
}

#####

s<-s %>% mutate(
  dataset='porpoise data',
  record_type='PS',
  country='All',
  area='1',
  lat=55,
  lon=0,
  month=(quarter-1)*3+1,
  day=1,
  haul=quarter,
  station=quarter,
  sample_id=paste('HP',year,quarter,sep='_'),
  ship='xxx',
  rectangle='40F0',
  pred_name='Phocoena phocoena',
  pred_nodc=8111111112,
  pred_l=1100,
  pred_ll=1000,
  pred_lu=1200,
  CPUE=1,
  fish_id=paste(year,quarter,sep='_'),
  n_food=10,
  n_regur=0,
  n_empty=0,
  n_skel=0,
  n_tot=n_food+n_empty+n_regur+n_skel,
  digest=1,
  prey_name=prey, prey=NULL,
  prey_w=preyw,preyw=NULL,
  #prey_n=nprey, nprey=NULL,
  prey_n=NA, nprey=NULL,
  prey_l=NA,
  prey_w_meth='r',
  prey_ll=lowpreyl*10, lowpreyl=NULL,
  prey_lu=higpreyl*10, higpreyl=NULL)

s[s$prey_name=='OTH','prey_ll']<-NA
s[s$prey_name=='OTH','prey_lu']<-NA

write_csv(s,file=file.path(stomach_dir,paste0('adjusted_',in_file)))

control<-new("STOMcontrol",
             name='Porpoise',
             predators= "Phocoena phocoena",
             years=as.integer(c(1985,1995,2005)),
             stomach_dir=stomach_dir,
             dataSets=paste0('adjusted_',in_file))

por<-read_exchange_data(control, delete_errors=FALSE, allow_alias_names = TRUE,  keep_just_mandatory_fields = FALSE)
por

#########################
#combine seal and porpoise
sp<-c(seal,por)
sp
infos<-read_csv(file.path(config_dir,"species_info.csv"),col_types = cols()) %>%
  select(code,SMS_species)

sp[['PREY']]<-left_join(sp[['PREY']],infos,by=c("prey_name"="code")) %>%
  mutate(prey_name2=SMS_species,SMS_species=NULL)
sp[['PREY']]<-sp[['PREY']] %>% mutate(prey_name2=ifelse(prey_name=='SAN',"Ammodytidae",prey_name2)) %>%
  mutate(prey_name=factor(prey_name2),prey_name2=NULL)
NODC_latin <- system.file("extdata","NODC_latin.csv", package = "FishStomachs")
b <- as_tibble(read.csv(file = NODC_latin, stringsAsFactors = FALSE)) %>% select(species,NODC) %>%distinct()

# put NODC on prey
sp[['PREY']]<-left_join(x=sp[['PREY']],y=b,by = c("prey_name" = "species")) %>% rename(prey_nodc=NODC) %>%
  mutate(prey_nodc=if_else(is.na(prey_nodc),9999999999,prey_nodc))

#####################

stomach_dir<-file.path(stom_dir,"old-stomachs")
in_file <-c("StomachDataSet20101122.csv")

a<-read_csv(file.path(stomach_dir,in_file),guess_max = 200000,col_types = cols())
names(a)
#sort(unique(a$"Date/Time"))

b<-a %>% rename(
  dataset="File Name",
  est_lat_lon="Estimated Lat/Lon",station= "Station/Haul" ,sample_id="Sample Number",fish_id="ICES StomachID",
  rectangle="ICES Rectangle",samp_meth="Sampling Method",pred_name=Predator,pred_nodc="Predator NODC Code" ,
  pred_l= "Predator (mean) Lengh",pred_w="Predator (mean) Weight",pred_age="Predator (mean) Age",
  pred_ll="Predator Lower Length Bound", pred_lu="Predator Upper Length Bound",pred_cpue="CPUE",
  n_food="Number Stomachs With Food",n_regur="Number Stomachs Regurgitated",n_skel="Number Stomachs With Skeletal Remains",
  n_empty= "Number Stomachs Empty",n_tot= "Number Stomachs",digest_in="Digestion Stage",
  prey_name="Prey Species Name",prey_nodc="Prey NODC Code",prey_w="Prey Weight",
  prey_ll="Prey Lower Length Bound",prey_lu="Prey Upper Length Bound",prey_n="Prey Number") %>%
  mutate("ICES Internal ID"=NULL,haul='X',"Date/Time"=NULL,prey_w_meth='r',area='XX',prey_l=prey_ll,sample_id=paste(pred_name,Country,Year,Quarter,fish_id,sep='_')) %>%
  filter(dataset=='IMARESdataset')
sort(unique(b$digest_in))
b<-b %>%  mutate(digest=if_else(digest_in=='Pristine',0L,
                        if_else(digest_in=='Afected by digestion',1L,
                                if_else(digest_in=='Skeletal remains',2L,9L))))
xtabs(~digest_in+digest,data=b)
b<-select(b,-digest_in)
names(b)
write_csv(b, file=file.path(stomach_dir,paste0("adjusted_",in_file)))
file.path(stomach_dir,paste0("adjusted_",in_file))

#read data in again from exchange form
control<-new("STOMcontrol",
             name='IMARES data',
             stomach_dir=stomach_dir,
             predators= as.character(NA),
             years=as.integer(NA),
             dataSets=paste0("adjusted_",in_file))

imares<-read_exchange_data(control, delete_errors=FALSE, allow_alias_names = TRUE,  keep_just_mandatory_fields = FALSE)

im2<-subset(imares,dataset=='IMARESdataset',pred_name %in% my_pred_sp)
summary(im2)
im2<-subset(im2,!(year %in% c(1981,1984,1986))) #already in oldformat data
im2<-subset(im2,!((year %in% c(1989,1991)) & (pred_name=="Merluccius merluccius"))) #already in oldformat data
im2<-subset(im2,!((year %in% c(1991)) & (pred_name=="Amblyraja radiata"))) #already in oldformat data

summary(im2)
im2<-change_data(im2,correct_nstom=TRUE,correct_predl=FALSE)


#####################
# put it all together

ns<-c(st,sp,im2)
summary(ns)

subset(ns,(pred_name=="Gadus morhua" & pred_ll==1200 ))


# move year
#  SAS if Predator_NODC_Code=8791030901 and year<=1983 then year=1981;  * Saithe stomachs;
ns[['PRED']]<-ns[['PRED']] %>% mutate(year=if_else(pred_name=="Pollachius virens" & year<=1983,1981L,year))


ns[['PRED']]<-ns[['PRED']] %>% mutate(year=if_else(pred_name=="Gadus morhua" & year==1990L,1991L,year))
ns[['PRED']]<-ns[['PRED']] %>% mutate(year=if_else(pred_name=="Gadus morhua" & year==1989L,1987L,year))

# delete
# %let cod_select=%str(if pred=8791030402 and (pred_l<100 or pred_l=1200 or (pred_l<120 and quarter in ('1','2')) or (quarter='4' and pred_l in (60,70))) then delete;);
ns<-subset(ns,!(pred_name=="Gadus morhua" & (pred_ll<100 | pred_l==1200 | (pred_ll<120 & quarter %in% c(1,2)) | quarter==4 & pred_ll %in% c(60,70))))

# %let had_select=%str(if pred=8791031301           and (pred_l<100 or pred_l=0700 or (pred_l<100 and quarter='1') or (quarter='2' and pred_l<120) or (pred_l=50)) then delete;);
ns<-subset(ns,!(pred_name=="Melanogrammus aeglefinus" & (pred_ll<100 | pred_l==700 | (pred_ll<100 & quarter %in% c(1)) | (quarter==2 & pred_ll<120) | (pred_l==50) )))

# %let sai_select=%str(if pred=8791030901 and pred_l<300 then delete;);
ns<-subset(ns,!(pred_name=="Pollachius virens" & pred_ll<300 ))

# %let whg_select=%str(if pred=8791031801       and (pred_l<100 or quarter in ('1','2') and pred_l <80) then delete;);
ns<-subset(ns,!(pred_name=="Merlangius merlangus" & (pred_ll<100 | quarter %in% c(1,2) & pred_l<80)))

# SAS
# if pred=8850030302 and year=1981 then do; *use 1981 size classes for Mackerel as well;
#   if pred_l=350 then do; pred_l=300; sample=sample*10; end;
#   if pred_u=350 then do; pred_u=400; end;
#   mac='OK';
# end;
ns[['PRED']]<-  ns[['PRED']] %>% mutate(pred_ll=if_else(pred_name=="Scomber scombrus" & year==1981 & pred_ll==350L,300L,pred_ll))
ns[['PRED']]<-  ns[['PRED']] %>% mutate(pred_lu=if_else(pred_name=="Scomber scombrus" & year==1981 & pred_lu==350L,400L,pred_lu))



ns<-subset(ns,!(is.na(pred_lu) | is.na(pred_ll)))

ns[['PRED']][is.na(ns[['PRED']]$pred_cpue),'pred_cpue'] <- ns[['PRED']][is.na(ns[['PRED']]$pred_cpue),'n_tot']
ns<-subset(ns,pred_lu>=100)

# OK PRED

# PREY
#summary(ns[['PREY']])

ns[['PREY']]<-ns[['PREY']] %>% mutate(digest=if_else(is.na(digest),9L,digest))
ns[['PREY']] <- ns[['PREY']] %>% mutate(prey_ll=if_else(prey_ll== -1 & prey_lu==0,0L,prey_ll),prey_w_meth= factor('r'))

dd<-is.na(ns[['PREY']]$prey_w)
ns[['PREY']][dd,'prey_w']<-0
# ns[['PREY']][dd,'prey_l']<-9999L
# ns[['PREY']][dd,'prey_ll']<-9999L
# ns[['PREY']][dd,'prey_lu']<-9999L
ns[['PREY']][dd,'prey_n']<-NA
summary(ns[['PREY']])

# add roundfish area

rf<-read_csv(file.path(config_dir,"rectangles_to_roundfish.csv"),col_types = cols()) %>%
    distinct() %>% mutate(roundfish=as.integer(roundfish),rectangle=factor(rectangle,levels=levels(ns[['PRED']]$rectangle)))

ns[['PRED']]<-  left_join(ns[['PRED']],rf,by = "rectangle") %>%mutate(area=roundfish,roundfish=NULL,pred_l=as.integer(pred_l))




ns[['PREY']]<-ns[['PREY']] %>% mutate(prey_l=as.integer(prey_l),prey_ll=as.integer(prey_ll),prey_lu=as.integer(prey_lu))

ns[['PREY']]<- ns[['PREY']] %>% mutate(prey_l=if_else(prey_l==9999L,as.integer(NA),prey_l),
                        prey_lu=if_else(prey_lu==9999L,as.integer(NA),prey_lu),
                        prey_ll=if_else(prey_ll==9999L,as.integer(NA),prey_ll))


tst<-subset(ns,pred_name %in% c("Gadus morhua","Pollachius virens"), year==1991)

write_exchange_data(tst,file.path(test_dir,'cod_pok_stom_1991.csv'))

tst<-subset(tst,pred_name %in% c("Gadus morhua"), year==1991)
write_exchange_data(tst,file.path(test_dir,'cod_stom_1991.csv'))



#names(ns[['PRED']])

delete_vars=c("country","ship","month","day","time","lat","lon","est_lat_lon","temp","pred_nodc","pred_w","depth","haul","station","samp_meth","pred_age")
ns2<-change_data(ns,delete_vars)

ns<-subset(ns,pred_l<2000)

save(ns,north_south_GSE_quarter,file=file.path(Rdata,'ns.Rdata'))

