library("readxl")

##############################

o<-read_xlsx(path=file.path(porpoise,"Otholiths_porpoise.xlsx"),sheet="data")
o<-o %>% filter(PorpoisePrey!='XX') %>% select(PorpoisePrey, Yvar,  Xvar,  regression,     a , b , Lwa,Lwb )%>%
      rename(species=PorpoisePrey) %>% mutate(species=tolower(species))
o

# test OL(otolith length of a 20 cm fish)

length<-20
filter(o,regression=="Y=a+bx") %>% unique() %>% mutate(fishLength=length,OL=(length-a)/b, OmassProxy=OL^3, residenceTime=sqrt(OmassProxy),weightingFactor=1/residenceTime)

s<-read_xlsx(path=file.path(porpoise,"HP_diet_corrections_2023.xlsx"),sheet="Diet_proportions") %>%
    mutate(species=tolower(species),species2=if_else(species %in% c('cod','herring','norwaypout','sandeel','sprat','whiting'),species,'XotherFood'))

s
xtabs(diet~year+country,data=s)
aa<-xtabs(diet~species+paste(year,country),data=s)

# I assume it is weight proportions
agem<-round(rbind(aa,all=colSums(aa)),4)
agem

aa<-xtabs(diet~species2+paste(year,country),data=s)
agem2<-round(rbind(aa,all=colSums(aa)),4)
agem2

aa<-xtabs(diet~species2+paste(year,country),data=subset(s,species2 !="XotherFood"))
agem3<-round(rbind(aa,all=colSums(aa)),4)
agem3
aa<-sweep(aa, 2, colSums(aa), FUN = '/')
agem4<-round(rbind(aa,all=colSums(aa)),4)
agem4

##

left_join(s,o,by = "species")

l<-read_xlsx(path=file.path(porpoise,"HP_diet_corrections_2023.xlsx"),sheet="Lengthdistributions") %>%
  pivot_longer(cols=4:9) %>%
  mutate(species=tolower(name),name=NULL)
l

xtabs(value~year+country,data=l)
aa<-xtabs(value~species+paste(year,country,paste='_'),data=l)
aa
rbind(aa,all=colSums(aa))
ftable(xtabs(value~species+cut(lowl,5)+paste(year,country,sep='_'),data=l))


bb<-left_join(l,o,by = "species") %>% filter(lowl>3) %>%
  mutate(fishLength=(lowl+higl)/2,fishWeight_g=Lwa*(fishLength*100)^Lwb,OL=(fishLength-a)/b, OmassProxy=OL^3, residenceTime=sqrt(OmassProxy),weightingFactor=1/residenceTime) %>%
  filter(value>0) %>% select(-Yvar, -Xvar,-regression,  -a,-b,-Lwa,-Lwb)
bb


# no adjustment
a<-xtabs(fishWeight_g~species+paste(year,country,sep='_'),data=bb)
asum<-colSums(a)

aaa<-sweep(a, 2, asum, FUN = '/')
round(rbind(aaa,all=colSums(aaa)),4)
#for comparison
agem4

################ it does not work (all above);

### use the old (but still valid data set from 2017?)

a<-read_csv(file.path(stom_dir,"mammals","porpoise_diet_2017_AR.csv"))
a

aa<-xtabs(preyw~prey+paste(year,quarter,sep='_'),data=a)
aa
asum<-colSums(aa)
aa<-sweep(aa, 2, asum, FUN = '/')
round(rbind(aa,all=colSums(aa)),4)

small<-a$lowpreyl<=4
a[small,'prey']<-'OTH'
a[small,'lowpreyl']<-1
a[small,'higpreyl']<-2
a[small,'nprey']<-0

a<- a %>% group_by(year, quarter, prey,  lowpreyl, higpreyl) %>% summarize(preyw=sum(preyw), nprey=sum(nprey)) %>% ungroup()

a
filter(a,prey=='OTH')

o<-read_xlsx(path=file.path(porpoise,"Otholiths_porpoise.xlsx"),sheet="data")  %>%
  filter(regression=="Y=a+bx" & !is.na(code)) %>% select(code,   a , b ) %>% rename(prey=code) %>%
  mutate(prey=if_else(prey %in% c('NSA','SSA'),'SAN',prey)) %>% unique()
o

ao<-left_join(a,o, by = "prey") %>%  filter(lowpreyl>0) %>%
  mutate(fishLength=(lowpreyl+higpreyl)/2,OL=(fishLength-a)/b, OmassProxy=OL^3, residenceTime=sqrt(OmassProxy),weightingFactor=1/residenceTime)

ao
filter(ao,prey=='OTH')
summary(ao)

filter(ao,is.na(OL))
filter(ao,is.na(weightingFactor ))
# guess weighting factor for other food
oth<-filter(ao,!is.na(OL)) %>%group_by(year,quarter) %>% summarize(othW=weighted.mean(weightingFactor,preyw)) %>% ungroup() %>% mutate(prey='OTH')
oth

aao<-left_join(ao,oth) %>% mutate(weightingFactor=if_else(is.na(othW),weightingFactor,othW),othW=NULL)
filter(aao,prey=='OTH')
aao

arrange(aao,weightingFactor)
arrange(aao,desc(weightingFactor))
summary(aao)

b<-aao %>% group_by(year,quarter,prey) %>%
  summarize(w_preyw=sum(preyw*weightingFactor),  preyw=sum(preyw))  %>%
  group_by(year,quarter) %>%
  mutate(preyw=preyw/sum(preyw), w_preyw=w_preyw/sum(w_preyw))

bb<-pivot_longer(b,cols=4:5) %>% mutate(method=if_else(name=="w_preyw",'adjusted','observed')) %>% mutate(time=paste0(year,' Q',quarter))

ggplot(data=bb, aes(x=prey, y=value, fill=method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(cols=vars(time))+ labs(x='Prey',y='Proportion')+
  theme_bw()
