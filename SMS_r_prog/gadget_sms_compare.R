
g<-read_csv(file=file.path( root,"gadget","mort_Gadget.csv"))
g$source<-'Gadget'
g<-as_tibble(g)

a<-Read.summary.data()
a<-subset(a,select=c(Year,Species,Quarter,Age,M1,M2))
a$source<-'SMS'
head(a)

a<-as_tibble(a) %>% filter(Species %in% c('Herring','Sprat')) %>% group_by(Year,Species,Age,source) %>%summarise(M1=sum(M1),M2=sum(M2))

b<-bind_rows(a,g)


X11(h=8,w=10)
bb<-subset(b,Species=='Herring' & Age<=8)
print(ggplot(bb, aes(x=Year, y=M1+M2, group=source)) +
  geom_line(aes(color=source),size=1)+
  #geom_point(aes(color=source))+
  theme(legend.position="right")+
  facet_wrap(~ paste("Age:",Age)) +
  labs(x="Year", y="M1+M2 ",title="M1+M2 of herring")
)

X11(h=8,w=10)
bb<-subset(b,Species=='Sprat' & Age<=8)
print(ggplot(bb, aes(x=Year, y=M2, group=source)) +
  geom_line(aes(color=source),size=1)+
  #geom_point(aes(color=source))+
  theme(legend.position="right")+
  facet_wrap(~ paste("Age:",Age)) +
  labs(x="Year", y="M2 ",title="M2 of Sprat")
)

X11(h=8,w=10)
bb<-subset(b,Species=='Sprat' & Age<=8)
print(ggplot(bb, aes(x=Year, y=M1+M2, group=source)) +
  geom_line(aes(color=source),size=1)+
  #geom_point(aes(color=source))+
  theme(legend.position="right")+
  facet_wrap(~ paste("Age:",Age)) +
  labs(x="Year", y="M1+M2 ",title="M1+M2 of Sprat")
)
