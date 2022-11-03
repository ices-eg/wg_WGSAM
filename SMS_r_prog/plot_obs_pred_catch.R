
dat<-Read.summary.data(extend=FALSE,read.init.function=F) %>% filter(Yield>=0)
dat<-dat %>% mutate(observed=Yield,predicted=C.hat*weca) %>% group_by(Species.n,Species,Year) %>% 
  summarize(observed=sum(observed,na.rm=TRUE),predicted=sum(predicted,na.rm=T)) %>% ungroup() %>%
  pivot_longer( cols = c("observed", "predicted"),names_to="Yield")

dat
ggplot(dat, aes(x=Year, y=value/1000,shape=Yield,color=Yield)) + 
   geom_point()+ geom_line()+facet_grid(vars(Species),scales="free_y")+
   ylab("Yield (1000 tonnes)")
  


