plotExploitationPattern<-function(indir=data.path,outdir=data.path) {
  a<-Read.summary.data(dir=data.path,infile='summary.out',extend=FALSE,read.init.function=TRUE)
  
  s<-SMS.control@catch.sep.year 
  
  ss<-NULL
  for (i in (1:length(s))) {
    for (j in (1:length(s[[i]]))) ss<-rbind(ss,data.frame(Species=sp.names[i+first.VPA-1], Year=s[[i]][j]))
  }
  ss$y2<-c(tail(ss$Year,-1)-1,SMS.control@last.year.model)
  
  ss[!duplicated(ss$Species,fromLast = TRUE),'y2']<-SMS.control@last.year.model
  ss<-merge(ss,data.frame(Species=sp.names[first.VPA:nsp],combined=SMS.control@combined.catches))
  
  ff<-data.frame(SMS.control@avg.F.ages)
  ff$Species<-sp.names[first.VPA:nsp]
  ss<-merge(ss,ff)
  head(ss)
  a<-merge(ss,subset(a,select=c(Species,Species.n,Year,Quarter,Age,F)))
  a<-subset(a,combined==0 | Quarter==1)
  a2<-subset(a,Age>=first.age &Age<=last.age)
  
  aa<-aggregate(F~Species+Year+Quarter,mean,data=a)
  aa$meanF<-aa$F; aa$F<-NULL
  
  a<-merge(a,aa)
  a$exploit<-a$F/a$meanF
  a$years<-paste(a$Year,a$y2,sep='-')
  
  a$Quarter<-ifelse(a$combined==1,'annual',paste('Quarter',a$Quarter))
  a$fages<-paste(a$first.age,a$last.age,sep='-')
  a<-subset(a,select=c(Species,Species.n,years,Quarter,fages,Age,exploit),!is.na(exploit))
  
  
  for (pp in sort(unique(a$Species.n)) ) {
    aaa<-subset(a,Species.n==pp)
    fage<-aaa[1,'fages']
    p<- ggplot(aaa, aes(x=Age, y=exploit)) + 
      #geom_bar(stat="identity", color="red", position=position_dodge()) +
      geom_line(aes(linetype = years)) +  geom_point()+
      xlab('Age')+
      ylab("F scaled to Fbar") +
      facet_wrap(~ Quarter, ncol=2,scales='free_y')+
      
      scale_x_continuous(breaks = scales::breaks_width(1),minor_breaks = NULL)
      #  scale_x_discrete()+
  
    ggsave(file.path(outdir,paste0("expl_",sp.names[pp],".png")),width = 14, height = 13, units = "cm")
  }
}

#plotExploitationPattern(indir=data.path,outdir=data.path)
