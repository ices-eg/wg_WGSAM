
##### seals
stomach_dir<-file.path(stom_dir,"mammals");
nstomach_dir<-file.path(stomach_dir,"seal_2023");


ss<-read_xlsx(path=file.path(nstomach_dir,"1985 Hg consumption for SMS 2023-10-03.xlsx"),sheet="1985 North Sea SMS") %>% rename(prey=species.name)
ss
by(ss,ss$quarter,function(x){
    a<-xtabs(cons.t~prey+region,data=x)
    round(rbind(a,colSums(a)),2)
})



s85<-read_xlsx(path=file.path(nstomach_dir,"1985 Hg consumption for SMS 2023-10-03.xlsx"),sheet="Fish_lengths 1985 NS SMS") %>%
    select(region,year,quarter,species=sp.name,l=`fish length`)
sort(unique(s85$species))
sort(unique(s85$year))

#### NOTE all allocated to year 1985
s85$year<-1985


sps<-  matrix(c(
                	"COD",	"Gadus morhua",	'cod',"Cod",
                	"WHG",	"Merlangius merlangus", 'whiting',"Whiting",
                	"HAD",	"Melanogrammus aeglefinus", "haddock","Haddock",
                	"POK",	"Pollachius virens","saithe","Saithe",
                	"MAC",	"Scomber scombrus","mackerel","Mackerel",
                	"HER",	"Clupea harengus","herring","Herring",
                	"NSA",	"Ammodytidae","sandeel",'N.sandeel',
                	"NOP",	"Trisopterus esmarkii","norway pout","Nor.pout",
                	"SPR",	"Sprattus sprattus","sprat",'Sprat',
                	"PLE",  "Pleuronectes platessa","plaice","Plaice",
                	"SOL",	"Solea solea","sole","Sole"),
              byrow=T,ncol=4)


colnames(sps)<-c('SMS','latin','species',"Species")
sps<-as.data.frame(sps)
sps
s85<-left_join(s85,sps)
s85
filter(s85,is.na(latin))  #errors ?

s85<-left_join(s85,Read.length.weight.relation(dir=data.path))
s85
filter(s85,is.na(a))  #errors


# length weight relation W=a * Power(L,b)
# L in mm, W in kg
# Source Coull, K.K et al. Length/weight relationships for 88 species
# of fish Encountered in the North East Atlantic
# Scottish Fisheries Research Report, 43, 1989
s85<-s85 %>% mutate(w=a*((l*10)^b) )  #in kg
s85
filter(s85,l>40)
sort(unique(s85$SMS))
s85[s85$SMS=='NSA','SMS']<-"SAN"

s85<-s85 %>% select(region,year,quarter,l,latin,prey=SMS,Species,w) %>%dplyr::mutate_if(is.character,as.factor)
s85<-s85 %>%mutate(lowpreyl=round(trunc(l)),higpreyl=round(trunc(l)+1))
summary(s85)

#Includes more years than 1985, threats it like one single year
ss<-s85 %>% group_by(quarter,region,latin,prey,lowpreyl,higpreyl) %>% summarize(menaW=mean(w),meanL=mean(l),w=sum(w)) %>% ungroup()
ss

by(ss,ss$quarter,function(x){
  a<-xtabs(w~prey+region,data=x)
  round(rbind(a,colSums(a)),2)
})

#relative stomach content
ss2<-ss %>% group_by(region,quarter,prey) %>% summarize(w=sum(w)) %>% group_by(region,quarter) %>% mutate(W_percent=w/sum(w)*100)

by(ss2,ss2$quarter,function(x){
  a<-xtabs(W_percent~prey+region,data=x)
  round(rbind(a,colSums(a)),2)
})


+###################  old


##### seals
stomach_dir<-file.path(stom_dir,"mammals");
in_file <-c("seal_diet.csv")

s<-read_csv(file.path(stomach_dir,in_file),col_types = cols())
head(s)
sort(unique(s$prey))
summary(s)


by(s,s$year,function(x){
  a<-xtabs(preyw~prey+quarter,data=x)
  round(rbind(a,total=colSums(a)))
})


s2<-s %>% group_by(year,quarter,prey) %>% summarize(preyw=sum(preyw)) %>%
  group_by(year,quarter) %>% mutate(preyw=preyw/sum(preyw))


by(s2,s2$year,function(x){
  a<-xtabs(preyw~prey+quarter,data=x)
  round(rbind(a,total=colSums(a))*100,1)
})

#####################

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

seal<-read_exchange_data(control, delete_errors=FALSE, allow_alias_names = TRUE,  keep_just_mandatory_fields = FALSE)

