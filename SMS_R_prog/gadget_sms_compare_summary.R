
g<-read_tsv(file=file.path( root,"gadget","summary_Gadget.txt"),guess_max = 100000)
g<-filter(g,Species !='cod')

rec<-filter(g,Age==1 & Quarter==1) %>% mutate(value=Number/1000,source="Gadget",var='Recruits') %>% select(Year,Species,value,var,source)  # there are no stock number for age 0?
g$one<-1

Fbar<- filter(g, (Species=='her' & Age %in% (3:6)) | (Species=='spr' & Age %in% (3:5))) %>%
       group_by(Year,Species) %>% summarise(Fbar=sum(F_q),no=sum(one)) %>% mutate(value=Fbar/no*4,no=NULL,Fbar=NULL,source="Gadget",var='Fbar')

SSB<-filter(g,Quarter==1) %>%  group_by(Year,Species) %>% summarise(value=sum(SSB,na.rm=TRUE)/1000) %>% mutate(source="Gadget",var='SSB')

gadget<-bind_rows(bind_rows(rec,Fbar),SSB)  
 

g<-read_delim(file=file.path( root,"gadget","summary_SMS.out"),delim=' ',guess_max = 100000)
g<-mutate(g,Species=c('cod','her','spr')[Species.n])

rec<-filter(g,Age==1 & Quarter==1) %>% mutate(value=N,source="SMS",var='Recruits') %>% select(Year,Species,value,var,source)  # there are no stock number for age 0?
g$one<-1

Fbar<- filter(g, (Species=='her' & Age %in% (3:6)) | (Species=='spr' & Age %in% (3:5))) %>%
  group_by(Year,Species) %>% summarise(Fbar=sum(F),no=sum(one)) %>% mutate(value=Fbar/no*4,no=NULL,Fbar=NULL,source="SMS",var='Fbar')

SSB<-filter(g,Quarter==1) %>%  group_by(Year,Species) %>% summarise(value=sum(SSB,na.rm=TRUE)) %>% mutate(source="SMS",var='SSB')

sms<-bind_rows(bind_rows(rec,Fbar),SSB)  



g<-read_tsv(file=file.path( root,"gadget","summary_table_ICES_ASSES.out"),guess_max = 100000)
g<-mutate(g,Species=c('cod','her','spr')[Species.n])

rec<-g %>% mutate(value=Rec,source="ICES",var='Recruits') %>% select(Year,Species,value,var,source) 

Fbar<-g %>% mutate(value=mean.F,source="ICES",var='Fbar') %>% select(Year,Species,value,var,source) 


SSB<-g %>% mutate(value=SSB,source="ICES",var='SSB') %>% select(Year,Species,value,var,source) 


ICES<-bind_rows(bind_rows(rec,Fbar),SSB)  



a<-bind_rows(gadget,sms)
a<-bind_rows(a,ICES) %>% filter(Year<=2018)



X11(h=8,w=10)
b<-filter(a,Species=='her')
print(ggplot(b, aes(x=Year, y=value, group=source)) +
  geom_line(aes(color=source),size=1)+
  #geom_point(aes(color=source))+
  theme(legend.position="right")+
  facet_grid(rows=vars(var),scales='free_y') +
  labs(x="Year", y="value ",title="Herring")
)


X11(h=8,w=10)
b<-filter(a,Species=='spr')
print(ggplot(b, aes(x=Year, y=value, group=source)) +
        geom_line(aes(color=source),size=1)+
        #geom_point(aes(color=source))+
        theme(legend.position="right")+
        facet_grid(rows=vars(var),scales='free_y') +
        labs(x="Year", y="value ",title="Sprat")
)
