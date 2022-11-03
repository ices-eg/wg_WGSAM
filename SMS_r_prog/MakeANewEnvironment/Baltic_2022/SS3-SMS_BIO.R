
#2012 key-run  data
b<-Read.summary.data(dir=file.path(root,"Baltic-2012-keyRun-results"),read.init.function=FALSE)
head(b)

b<-subset(b,Species=='Cod',select=c(Species,Year,Quarter,Age,BIO,N,west))
b$Source<-'2012 key run'
b12<-as_tibble(b)


#2019 SS3
# library(devtools)
# devtools::install_github("r4ss/r4ss")

library(r4ss)

myreplist <- SS_output(dir='C:/_C_drev/SMS-git/Data_Baltic/2019-data/Cod/BFAS_Final')
# make a bunch of plots
# SS_plots(myreplist)

str(myreplist,1,list.len=200)

fa=0;la=15
conv<-function(x,fa=0,la=15,v.names='cn') {
  x<-subset(x,select=c("Yr","Seas","Fleet",as.character(fa:la)))
  colnames(x)<-c("Yr","Seas","Fleet",paste("age",as.character(fa:la),sep='_'))
  
  x<-reshape(x,direction='long',varying=list(4:(4+la-fa)),v.names=v.names)
  x$Age<-x$time-1
  x<-subset(x,select=c("Yr","Seas",'Fleet','Age',v.names))
  return(x)
}

# biomas at age
ba<-myreplist$batage
cba<-colnames(ba)
cba<-gsub("Beg/Mid","BM",cba)
colnames(ba)<-cba
ba<-subset(ba,BM=='B' & Yr>=1974 & Yr<=2019 & Era=='TIME')
ba$Fleet<- -9
ba19<-conv(x=ba,v.names='bio')
ba19<-as_tibble(ba19) %>% rename("Year"="Yr","Quarter"="Seas","BIO"="bio") %>%mutate(Fleet=NULL,Source='SS3 2019',Species='Cod')
ba19

# N at age
na<-myreplist$natage
cna<-colnames(na)
cna<-gsub("Beg/Mid","BM",cna)
colnames(na)<-cna
na<-subset(na,BM=='B' & Yr>=1974 & Yr<=2019 & Era=='TIME')
na$Fleet<- -9
na19<-conv(x=na,v.names='n')
na19<-as_tibble(na19) %>% rename("Year"="Yr","Quarter"="Seas","N"="n") %>%mutate(Fleet=NULL,Source='SS3 2019',Species='Cod')
na19


nb19<-full_join(na19,ba19)


na21$Source<-'SS3 2022'
na21<- na21 %>% rename("Year"="year","Quarter"="quarter","N"="n","Age"="age")


ba21$Source<-'SS3 2022'
ba21<- ba21 %>% rename("Year"="year","Quarter"="quarter","BIO"="bio","Age"="age")

nb21<-full_join(na21,ba21)
nb21<-nb21 %>% mutate( Species='Cod')

nb19
nb21

a<-rbind(nb19,nb21) %>% mutate(west=BIO/N)

a
b12
b<-rbind(b12,a)
####


bb<-b %>% filter(Quarter==1 & Age>0) %>% mutate(Age=if_else(Age>8,8,Age)) %>% 
  group_by(Source,Year,Quarter,Age) %>% summarise(BIO=sum(BIO,na.rm=TRUE),N=sum(N,na.rm=TRUE)) %>%
  mutate(west=BIO/N) %>%mutate(N=N/1000,BIO=BIO/1000)

bb

X11(h=8,w=10)
ggplot(bb, aes(x=Year, y=N, group=Source)) +
  geom_line(aes(color=Source),size=1)+
  geom_point(aes(color=Source))+
  theme(legend.position="right")+
facet_wrap(~ paste("Age:",Age), scale="free_y") +
    labs(x="Year", y="N (million) Q1 ",title="Stock Numbers")

X11(h=8,w=10)
ggplot(bb, aes(x=Year, y=BIO, group=Source)) +
  geom_line(aes(color=Source),size=1)+
  geom_point(aes(color=Source))+
  theme(legend.position="right")+
  facet_wrap(~ paste("Age:",Age)) +
  labs(x="Year", y="Biomass (1000 tonnes) Q1 ",title="Biomass")



bb1<-b %>% filter(Quarter==1 & Age>0)  %>% 
  group_by(Source,Year) %>% summarise(BIO=sum(BIO,na.rm=TRUE)/1000) %>% mutate(tit='Age 1 and older') 

bb1

bb5<-b %>% filter(Quarter==1 & Age>=5)  %>% 
  group_by(Source,Year) %>% summarise(BIO=sum(BIO,na.rm=TRUE)/1000) %>% mutate(tit='Age 5 and older') 

bb<-rbind(bb1,bb5)

X11(h=6,w=10)
ggplot(bb, aes(x=Year, y=BIO, group=Source)) +
  geom_line(aes(color=Source),size=1)+
  geom_point(aes(color=Source))+
  theme(legend.position="right")+
  facet_wrap(~ tit) +
  labs(x="Year", y="Biomass (1000 tonnes) Q1 ",title="Biomass")


