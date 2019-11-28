
#2012 key-run  data
b<-Read.summary.data(dir=file.path(root,"EB_2012_key_run"),read.init.function=FALSE)
head(b)

b<-subset(b,Species=='Cod',select=c(Species,Year,Quarter,Age,BIO,N,west))
b$source<-'2012 key run'
b<-as_tibble(b)
b
summary(b)


a<-as_tibble(read.csv(file='SS3_results_at_age.csv'))
a
summary(a)
a<-filter(a,Yr>=1974 & Yr<=2018 & n>0) %>% rename(Quarter=Seas,Year=Yr,BIO=bio,N=n,west=sw) %>% 
  mutate(Age=if_else(Age>8,8L,Age)) %>% 
  group_by(Year,Quarter,Age) %>% summarise(BIO=sum(BIO,na.rm=TRUE),N=sum(N,na.rm=TRUE),nb=sum(N*west,na.rm=T)) %>%
   mutate(Species='Cod',source='SS3',west=nb/N,nb<-NULL)

a
a<-bind_rows(a,b) %>%mutate(N=N/1000,BIO=BIO/1000) %>%filter(Quarter==1 & Age>=1)


bio<-a %>% group_by(Year,source) %>% summarise(BIO=sum(BIO))

X11(h=8,w=10)
ggplot(bio, aes(x=Year, y=BIO, group=source)) +
  geom_line(aes(color=source),size=2)+
  geom_point(aes(color=source))+
  theme(legend.position="right")+
  labs(x="Year", y="Biomass (1000 tonnes) Q1 ",title="Biomass Age 1 and older")


bio<-filter(a,Age>=5) %>% group_by(Year,source) %>% summarise(BIO=sum(BIO))
X11(h=8,w=10)
ggplot(bio, aes(x=Year, y=BIO, group=source)) +
  geom_line(aes(color=source),size=2)+
  geom_point(aes(color=source))+
  theme(legend.position="right")+
  labs(x="Year", y="Biomass (1000 tonnes) Q1 ",title="Biomass Age 5 and older")


X11(h=8,w=10)
ggplot(a, aes(x=Year, y=N, group=source)) +
  geom_line(aes(color=source),size=1)+
  geom_point(aes(color=source))+
  theme(legend.position="right")+
facet_wrap(~ paste("Age:",Age), scale="free_y") +
    labs(x="Year", y="N (million) Q1 ",title="Stock Numbers")

X11(h=8,w=10)
ggplot(a, aes(x=Year, y=BIO, group=source)) +
  geom_line(aes(color=source),size=1)+
  geom_point(aes(color=source))+
  theme(legend.position="right")+
  facet_wrap(~ paste("Age:",Age)) +
  labs(x="Year", y="Biomass (1000 tonnes) Q1 ",title="BIomass")

