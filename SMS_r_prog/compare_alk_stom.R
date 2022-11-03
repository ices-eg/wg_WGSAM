a<-Read.ALK.stom(dir=data.path,read.init.function=FALSE) %>% mutate(proportion.adjusted=NULL) %>%
  rename(len22=Length,prop22=proportion,nbar22=Nbar) %>% as_tibble()

 
b<-Read.ALK.stom(dir=file.path(root,"Baltic-2019-keyRun")) %>% mutate(proportion.adjusted=NULL) %>%
  rename(len19=Length,prop19=proportion,nbar19=Nbar) %>% as_tibble()


ab<-left_join(a,b) %>%  mutate(NbarRat=nbar22/nbar19,propRat=prop22/prop19,lenRat=len22/len19)
ab 


filter(ab,is.na(NbarRat))  # missing value in one of the two sets
filter(ab,is.na(propRat))  # missing value in one of the two sets
a<-filter(ab,NbarRat<0.8 |NbarRat>1.2)
a
xtabs(~Species,data=a)


a<-filter(ab,propRat<0.8 |propRat>1.2)
a
xtabs(~Species,data=a)
xtabs(~Species+LengthGroup,data=a)


a<-arrange(ab,desc(abs(1-lenRat)))
tail(a)
head(a)
a<-filter(ab,lenRat<0.95 |lenRat>1.05)
a
xtabs(~Species,data=a)
xtabs(~Species+LengthGroup,data=a)


sp<-'Herring'
tst<-filter(ab,Species==sp) ;plot(x=tst$prop22,y=tst$prop19, main=sp);abline(a=0,b=1,col='red')
tst<-filter(ab,Species==sp) ;plot(x=tst$len22,y=tst$len19, main=sp);abline(a=0,b=1,col='red')
sp<-'Sprat'
tst<-filter(ab,Species==sp) ;plot(x=tst$prop22,y=tst$prop19, main=sp)
tst<-filter(ab,Species==sp) ;plot(x=tst$len22,y=tst$len19, main=sp);abline(a=0,b=1,col='red')

tst<-filter(ab,propRat<0.8 |propRat>1.2) %>% arrange(propRat)
tail(tst,20)
head(tst,10)
summary(ab)


############################################


a<-as_tibble(Read.stomach.data.start(dir=data.path,read.init.function=FALSE)) %>% #filter(stom.used.all==1) %>%
  dplyr::select(Predator,Prey,Year,Quarter,Predator.length.class ,Predator.length, Prey.length.class,  N.haul, stomcon) %>%
  rename(n.haul22=N.haul,stomcon22=stomcon)


b<-as_tibble(Read.stomach.data.start(dir=file.path(root,"Baltic-2019-keyRun"),read.init.function=FALSE)) %>% #filter(stom.used.all==1) %>%
  dplyr::select(Predator,Prey,Year,Quarter,Predator.length.class ,Predator.length, Prey.length.class,  N.haul, stomcon) %>%
  rename(n.haul19=N.haul,stomcon19=stomcon)


ab<-left_join(a,b) %>%  mutate(NhaulRat=n.haul22/n.haul19) %>%
  arrange(Year,Quarter,Predator,Predator.length,Prey,Prey.length.class)
ab 


filter(ab,is.na(NbarRat))  # missing value in one of the two sets
filter(ab,NbarRat<0.8 |NbarRat>1.2)

sp<-'Herring'
tst<-filter(ab,Species==sp) ;plot(x=tst$prop22,y=tst$prop19, main=sp)
sp<-'Sprat'
tst<-filter(ab,Species==sp) ;plot(x=tst$prop22,y=tst$prop19, main=sp)

tst<-filter(ab,propRat<0.8 |propRat>1.2) %>% arrange(propRat)
tail(tst,10)
head(tst,10)
summary(ab)

