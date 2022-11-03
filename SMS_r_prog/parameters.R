
# read SMS.std and present results

a<-Read.SMS.std(excludeNotUsed=FALSE)
subset(a,used=='not_used')


#remove output variables
remo<-c('M2_sd','hist_SSB','avg_F')

a<-Read.SMS.std(excludeNotUsed=TRUE,remove=remo)


# sort parameter uncertanties
a$absCV.round<-abs(a$CV.round)
b<-a[order(abs(a$CV.round)),]
head(b)
tail(b,10)

#view(b)

b<-subset(b,select=c(-area,-predator,-prey, -fleet,-index, -used))

head(b,50)
tail(b,50)


b<-b[order(abs(b$CV.round),b$species),]
tail(b,70)
subset(b,par=='F_q_ini')
b$CV.Pct<-round(b$CV.round)


rownames(b)<-NULL
b<-subset(b,select=c( name,par,value,std, CV.Pct, species, year, quarter, age))
b$species.name<-' 0 predation'
b[b$species>0,"species.name"]<-paste(formatC(b[b$species>0,"species"],width=2),sp.names[b[b$species>0,"species"]])

head(b)
tail(b,10)

sort(unique(b$par))

if (file.exists( 'par_names.csv')) p<-read.csv('par_names.csv') else p<-read.csv('../HTML_source/par_names.csv')

b<-merge(b,p,by='par')
b<-droplevels(b)
a<-xtabs(~par_text+group,data=b)
a<-rbind(a,all=colSums(a))
a<-cbind(a,all=rowSums(a))
a


xtab(a, caption=paste("Table ", "Parameter overview. Number of estimated parameters by group of data"), cornername='Parameter',
     file=file.path(data.path,'_Parameter_overview.html'), dec=rep(0,dim(a)[[2]]), width='"100%"')



a<-xtabs(~par_text+species.name,data=b)
a<-rbind(a,all=colSums(a))
a<-cbind(a,all=rowSums(a))
a

xtab(a, caption=paste("Table ", "Parameter overview. Number of estimated parameters by species"), cornername='Parameter',
     file=file.path(data.path,'_Parameter_species_overview.html'), dec=rep(0,dim(a)[[2]]), width='"100%"')

#plot of year effect in F
a<-subset(b,par=="F_y_ini")
a<-subset(a,std<10000)
library(ggplot2)

p<- ggplot(a, aes(x=year, y=value)) + 
  geom_bar(stat="identity", color="red", position=position_dodge()) +
  #geom_line() +  geom_point()+
  labs(y = "F year effect")+
  facet_wrap(~ species.name, ncol=2,scales='free_y')+
  geom_errorbar(aes(ymin=value-std, ymax=value+std), width=.2,
                position=position_dodge(.9))
ggsave("uncertanty_F_year_effect.png",width = 17, height = 22, units = "cm")


#plot of recruits
a<-subset(b,par=="log_rec")
#a<-subset(a,std<10000)
#bb<-Read.Rec_scale.out(dir=data.path,read.init.function=FALSE);bb$Species<-NULL
#a<-merge(a,bb,by.x='species',by.y='Species.n')  
#a$value<-a$value+a$rec_scale

p<- ggplot(a, aes(x=year, y=value)) + 
  #geom_bar(stat="identity", color="red", position=position_dodge()) +
  geom_line() +  geom_point()+
  labs(y = "log(recruitment) +- 2*sd")+
  facet_wrap(~ species.name, ncol=2,scales='free_y')+
  geom_errorbar(aes(ymin=value-2*std, ymax=value+2*std), width=.2,
                position=position_dodge(.9))
ggsave("uncertanty_recruit.png",width = 17, height = 22, units = "cm")


bb<-data.frame(CV.abs=abs(b$CV.Pct),n=1:dim(b)[[1]])
bb<-subset(bb,CV.abs<50)
X11()
plot(x=bb$n,y=bb$CV.abs,ylab='CV %',xlab='')

bb<-subset(bb,CV.abs<100)
plot(x=bb$n,y=bb$CV.abs,ylab='CV %',xlab='')


#plot of N first year
a<-subset(b,par=="log_rec_older" | (par=="log_rec" & year==SMS.control@first.year))
a[a$age==-1,'age']<-SMS.control@first.age
#a$agec<-formatC(a$age,flag="0",digits=0,format="d",width=2)
p<- ggplot(a, aes(x=age, y=value)) + 
  #geom_bar(stat="identity", color="red", position=position_dodge()) +
  geom_line() +  geom_point()+
  labs(y = "log(Stock numbers) +- 2*sd",x='Age')+
  facet_wrap(~ species.name, ncol=2,scales='free_y')+

scale_x_continuous(limits = c(SMS.control@first.age, SMS.control@max.age.all),breaks = scales::breaks_width(2))+
  geom_errorbar(aes(ymin=value-2*std, ymax=value+2*std), width=.2,
                position=position_dodge(.9))
ggsave("uncertanty_N_first_year.png",width = 17, height = 22, units = "cm")


bb<-data.frame(CV.abs=abs(b$CV.Pct),n=1:dim(b)[[1]])
bb<-subset(bb,CV.abs<50)
X11()
plot(x=bb$n,y=bb$CV.abs,ylab='CV %',xlab='')

bb<-subset(bb,CV.abs<100)
plot(x=bb$n,y=bb$CV.abs,ylab='CV %',xlab='')


############## predation parameters
a<-Read.SMS.std(excludeNotUsed=TRUE,remove=remo)
sort(unique(a$name))
names<-a$name

withIndex<-c(grep("Stom_var",names),grep("var_size_ratio_ini",names),grep("init_pref_size_ratio",names))
b<-rbind(subset(a,name %in% c("vulnera","init_stl_other_suit_slope","init_season_overlap")),a[withIndex,])
b<-b[order(b$index),]
b$name<-as.character(b$name)
b$CV.round<-abs(b$CV.round)
names<-b$name
unique(names)

subset(b,name=='vulnera')
head(b)
found<-grep("Stom_var",names)
b[found,]
b[found,'predator']<-readr::parse_number(b[found,'name'])
b[found,'prey']<- -1
b[found,'name']<-'Stom_var'
unique(b$name)
sort(unique(b$predator))
b[b$predator>0,'Predator']<-paste(formatC(b[b$predator>0,'predator'],width=2),sp.names[b[b$predator>0,'predator']],sep='_')
sort(unique(b$prey))
b[b$prey>0,'Prey']<-paste(formatC(b[b$prey>0,'prey'],width=2),sp.names[b[b$prey>0,'prey']],sep='_')

bb<-subset(b,name=='vulnera')
a<-round(tapply(bb$CV.round,list(bb$Predator,bb$Prey),sum))
print(a, na.print = "")

xtab(a, caption=paste("Table ", "Parameter overview. CV of predator - prey vulnerability parameter"), cornername='Predator',
     file=file.path(data.path,'_Parameter_overview_vulnerab_CV.html'), dec=rep(0,dim(a)[[2]]), width='"100%"')


other<-unique(subset(bb,select=c(Predator)))
other$value<-1
other$std<-0
other$Prey<-'0_Other food'
bb<-subset(bb,select=c(Predator,Prey,value,std)) 
bb<-rbind(bb,other)

a<-round(tapply(bb$value,list(bb$Predator,bb$Prey),sum),2)
print(a, na.print = "")

xtab(a, caption=paste("Table ", "Parameter overview. Predator - prey vulnerability parameter"), cornername='Predator',
     file=file.path(data.path,'_Parameter_overview_vulnerab.html'), dec=rep(2,dim(a)[[2]]), width='"100%"')


p<- ggplot(bb, aes(x=Prey, y=value)) + 
  geom_bar(stat="identity", color="red", position=position_dodge()) +
 # geom_line() +  geom_point()+
  labs(y = "Vulnerability +- 1*sd")+
  facet_wrap(~ Predator, ncol=2,scales='free_y')+
  theme(axis.text.x = element_text(angle=90))+
  geom_errorbar(aes(ymin=value-1*std, ymax=value+1*std), width=.2,
                position=position_dodge(.9))
ggsave("uncertanty_vulnerability.png",width = 17, height = 22, units = "cm")


for (pp in sort(unique(bb$Predator)) ) {
  p<- ggplot(subset(bb,Predator==pp), aes(x=Prey, y=value)) + 
    geom_bar(stat="identity", color="red", position=position_dodge()) +
    # geom_line() +  geom_point()+
    labs(y = "Vulnerability +- 1*sd")+
   # facet_wrap(~ Predator, ncol=2,scales='free_y')+
    ggtitle(strsplit(pp,'_')[[1]][2])+
    theme(axis.text.x = element_text(angle=90))+
    geom_errorbar(aes(ymin=value-1*std, ymax=value+1*std), width=.2,position=position_dodge(.9))
  ggsave(paste0("uncertanty_vulnerability_",pp,".png"),width = 10, height = 7, units = "cm")
}

p<- ggplot(bb, aes(x=Prey, y=value)) + 
  geom_bar(stat="identity", color="red", position=position_dodge()) +
 # geom_line() +  geom_point()+
  labs(y = "Vulnerability")+
  facet_wrap(~ Predator, ncol=2,scales='free_y')+
  theme(axis.text.x = element_text(angle=90))
ggsave("vulnerability.png",width = 17, height = 22, units = "cm")


bb<-subset(b,name=='init_stl_other_suit_slope')
a<-round(t(t(tapply(bb$CV.round,list(bb$Predator),sum))))
print(a, na.print = "")

bb<-subset(b,name=='init_season_overlap')
print(round(t(t(tapply(bb$CV.round,list(bb$Predator),sum)))), na.print = "")


### catchability

a<-Read.SMS.std(excludeNotUsed=TRUE,remove=remo)
a<-subset(a,name=='qq_ini')

b<-readFleetInfo() 
a<-merge(x=a,y=b, all.y=TRUE)
a<-subset(a,select=c(species,age,fleet,fleetName,value,std))
a<-a[order(a$species,a$fleet,a$age),]
#b<-aggregate(cbind(mv2=value)~species+fleet,data=a,mean,na.rm=TRUE)
#a<-merge(a,b)
head(a)
for (i in (2:dim(a)[[1]])) {if (is.na(a[i,"value"])) a[i,"value"]<-a[i-1,"value"]}
a$Species<-sp.names[a$species]

for (pp in sort(unique(a$species)) ) {
  p<- ggplot(subset(a,species==pp), aes(x=age, y=value)) + 
  #geom_bar(stat="identity", color="red", position=position_dodge()) +
  geom_line() +  geom_point()+
  labs(y = "Catchability+- 2*sd",x='Age')+
  facet_wrap(~ fleetName, ncol=2,scales='free_y')+
  
  scale_x_continuous(breaks = scales::breaks_width(1),minor_breaks = NULL)+
  #  scale_x_discrete()+
  geom_errorbar(aes(ymin=value-2*std, ymax=value+2*std), width=.2,
                position=position_dodge(.9))
  ggsave(paste0("uncertanty_catchability_",sp.names[pp],".png"),width = 14, height = 13, units = "cm")
}

## survey variance
a<-Read.SMS.std(excludeNotUsed=TRUE,remove=remo)
a<-subset(a,name=='qq_s2_ini')

b<-readFleetInfo() 
a<-merge(x=a,y=b, all.y=TRUE)
a<-subset(a,select=c(species,age,fleet,fleetName,value,std))
a<-a[order(a$species,a$fleet,a$age),]

head(a)
for (i in (2:dim(a)[[1]])) {if (is.na(a[i,"value"])) a[i,"value"]<-a[i-1,"value"]}
a$Species<-sp.names[a$species]

for (pp in sort(unique(a$species)) ) {
  p<- ggplot(subset(a,species==pp), aes(x=age, y=value)) + 
    #geom_bar(stat="identity", color="red", position=position_dodge()) +
    geom_line() +  geom_point()+
    labs(y = "Observation variance+- 2*sd",x='Age')+
    facet_wrap(~ fleetName, ncol=2,scales='free_y')+
    
    scale_x_continuous(breaks = scales::breaks_width(1),minor_breaks = NULL)+
    #  scale_x_discrete()+
    geom_errorbar(aes(ymin=value-2*std, ymax=value+2*std), width=.2)
  ggsave(paste0("uncertanty_surv_variance_",sp.names[pp],".png"),width = 14, height = 13, units = "cm")
}

