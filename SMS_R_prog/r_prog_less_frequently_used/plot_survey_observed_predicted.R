a<-Read.catch.survey.residuals()
a<-as_tibble(a)
a<-filter(a,data=='survey')

fn<-Read.fleet.names()
fn
fl<-NULL
for (fleet in (1:ncol(fn))) for (s in (first.VPA:nsp)) fl<-rbind(fl,data.frame(fleet=fleet,Species.n=s,fleet_name=fn[s-first.VPA+1,fleet]))
fl<-as_tibble(fl)%>% mutate(fleet_name=as.character(fleet_name))
a<-left_join(a,fl)

a


by(a,list(a$Species,a$fleet),function(aa){
  #newplot(dev='png',nox=1,noy=1,filename=paste0(aa[1,'fleet_name']),Portrait=F,pointsize=25);

  p<-ggplot(data=aa, aes(x=Year, y=observed))+
     geom_point(col='red',size=2) +
     geom_line(aes(x=Year, y=model),col='blue',size=1)+
    facet_wrap(~ paste("Age:",Age), scale="free_y") + 
    labs(x="Year", y="Stock N  ",title=aa[1,'fleet_name'])
  
  ggsave(filename=paste0(aa[1,'fleet_name'],'.png'), plot=p, height=18, width=18, units="cm", dpi=300)

  aa<-aa %>%mutate(observed=log(observed),model=log(model))
  p<- ggplot(data=aa, aes(x=Year, y=observed))+
    geom_point(col='red',size=2) +
    geom_line(aes(x=Year, y=model),col='blue',size=1)+
    facet_wrap(~ paste("Age:",Age), scale="free_y") + 
    labs(x="Year", y="log(Stock N)  ",title=aa[1,'fleet_name'])
  ggsave(filename=paste0(aa[1,'fleet_name'],'_log.png'), plot=p, height=18, width=18, units="cm", dpi=300)
  
})
cleanup()



