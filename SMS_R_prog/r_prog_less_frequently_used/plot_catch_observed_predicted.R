aa<-Read.summary.data()
head(aa)
a<-aa
a$cw<-a$C.hat*a$weca; a$type<-'predicted'; a<-subset(a,select=c(Species,Year,type,cw))

b<-aa
b$cw<-b$C.obs*b$weca; 
b$type<-'observed' ;
b<-subset(b,select=c(Species,Year,type,cw))

a<-as_tibble(bind_rows(a,b)) 
a<-a %>% group_by(Species,Year,type) %>% summarise(cw=sum(cw,na.rm=TRUE)/1000) %>%filter(Species !='Cod')



X11(h=8,w=10)
ggplot(a, aes(x=Year, y=cw, group=type)) +
  geom_line(aes(color=type),size=1)+
  geom_point(aes(color=type))+
  theme(legend.position="right")+
  facet_wrap(~ paste(Species), scale="free_y") +
  labs(x="Year", y="Yield (1000 tonnes)  ",title="Yield")


aa<- filter(a,type=='observed') %>% rename(cw.obs=cw)%>% mutate(type=NULL)
bb<- filter(a,type=='predicted') %>% rename(cw.hat=cw)%>% mutate(type=NULL)

a<-left_join(aa,bb) %>% mutate(obs_pred=cw.obs/cw.hat)
a

X11(h=8,w=10)
ggplot(a, aes(x=Year, y=obs_pred)) +
  geom_line(size=1)+
  #geom_hline(yintercept=1,color='red',size=1)
  theme(legend.position="right")+
  facet_wrap(~ paste(Species), scale="free_y") +
  labs(x="Year", y="ratio  ",title="observed yiled divided by model yield")

 