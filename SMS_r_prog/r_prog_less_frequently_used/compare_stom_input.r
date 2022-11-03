
stomDirs<-c(file.path(root,'Baltic-2022-V01_same_stomachs_as_in_2019'),file.path(root,'Baltic-2022-keyrun'))
stomLabes<-c('2019 key run','2022 key run')

stom<-Read.stomach.data.start(dir=stomDirs[1])
a<-subset(stom,select=c( Predator.no,Predator,Prey.no,Prey,Quarter,Year, Predator.length, Predator.length.mean, Predator.size, Prey.length.class, Prey.length.mean,  Prey.size, N.haul,stomcon.input))

stom<-Read.stomach.data.start(dir=stomDirs[2])
b<-subset(stom,select=c( Predator.no,Predator,Prey.no,Prey,Quarter,Year, Predator.length, Predator.length.mean, Predator.size, Prey.length.class, Prey.length.mean,  Prey.size, N.haul,stomcon.input ))


#differences predator information
aa<-subset(a,select=c( Predator.no,Predator, Quarter, Year, Predator.length )) %>% mutate(source.a=TRUE) %>% unique()
bb<-subset(b,select=c( Predator.no,Predator, Quarter, Year, Predator.length )) %>% mutate(source.b=TRUE) %>% unique()

ab<-full_join(aa,bb)
filter(ab, source.a & is.na(source.b))
filter(ab, source.b & is.na(source.a))
#filter(ab, is.na(source.a)| is.na(source.b))

in_both<-inner_join(aa,bb) %>% dplyr::select( Predator.no,,Predator,Quarter,Year,Predator.length) %>% unique()

a<-right_join(a,in_both) %>% mutate(source.a=TRUE)
b<-right_join(b,in_both)%>% mutate(source.b=TRUE)


#differences prey species
aa<-subset(a,select=c(Predator, Quarter, Year, Predator.length,Prey,source.a )) %>% unique()
bb<-subset(b,select=c(Predator, Quarter, Year, Predator.length,Prey,source.b  ))  %>% unique()

ab<-full_join(aa,bb) %>% arrange(  Predator,Year, Quarter, Predator.length, Prey )
head(ab)

filter(ab, source.a & is.na(source.b))
filter(ab, source.b & is.na(source.a))


#differences prey information
aa<-subset(a,select=c( Predator.no,Predator, Quarter, Year, Predator.length,Prey.no,Prey,Prey.length.class,source.a,stomcon.input)) %>% 
     rename(stom_a=stomcon.input) %>% unique()
bb<-subset(b,select=c( Predator.no,Predator, Quarter, Year, Predator.length,Prey.no,Prey,Prey.length.class,source.b,stomcon.input)) %>% 
  rename(stom_b=stomcon.input) %>% unique()

ab<-full_join(aa,bb) %>% arrange( Predator.no,  Predator,Year, Quarter, Predator.length, Prey.no,Prey ,Prey.length.class)
head(ab)
dim(ab)


# compare  relative stomach content by predator quarter and year from two sources
compare_stoms<-function(stom) {
  cleanup()
  
  stom<-ab
  dev<-"png"
  #dev<-"screen"
  nox<-2
  noy<-3
  
  i<-0
  b<- tapply(stom$stom_a,list(stom$Prey.no),sum)
  all.prey.col<-sort(as.numeric(dimnames(b)[[1]]),decreasing = TRUE)
  all.names<-rep('aaa',length(all.prey.col))
  for (s in (1:length(all.prey.col))) all.names[s]<-sp.other.names[all.prey.col[s]+1]
  
  oldY<-0
  if (length(all.names<5)) my.cex <- 1.5 else my.cex<-1
  
  if (TRUE) by(stom,list(stom$Quarter,stom$Year,stom$Predator.no),function(x) {
    newY<<-x[1,'Year']
    b<- tapply(x$stom_a,list(x$Prey.no,x$Predator.length),sum,na.rm=TRUE)
    b[is.na(b)]<-0
    
    c<- tapply(x$stom_b,list(x$Prey.no,x$Predator.length),sum,na.rm=TRUE)
    c[is.na(c)]<-0
    
    b<-rbind(b,c)
    prey.no<-as.numeric(dimnames(b)[[1]])
    prey.names<-rep('aaa',length(prey.no))
    for (s in (1:length(prey.names))) prey.names[s]<-sp.other.names[prey.no[s]+1]
    length.names<-dimnames(b)[[2]]
    if (oldY != newY) {
      if (dev=='png') cleanup()
      newplot(dev,nox,noy,Portrait=F,filename=paste('comp_stmch_',x[1,]$Predator,x[1,]$Year,sep='-'),dir=data.path);
      par(mar=c(3,5,3,2))
      if (dev=="wmf" ) par(mar=c(2,4,2,2))
      
      # text(x=0.0,y=0.07,"lower: observed",pos=4)
      plot.new();
      title(main=list(paste0("upper:",stomLabes[1],"\nlower:",stomLabes[2]),cex=my.cex*1.5))
      legend("center",all.names,fill=all.prey.col,cex=my.cex)
      oldY<<-newY   
    }
    barplot(b,names=length.names,col=prey.no)
    title(main=paste(x[1,]$Year,x[1,]$Quarter," Pred:",x[1,]$Predator))
    #title(main=paste(x[1,]$Year,x[1,]$Quarter))
    
    abline(h=1,lwd=2)
  })
  cleanup()
}

compare_stoms(stom=ab)

aa<-filter(ab, source.a & is.na(source.b));aa;dim(aa)
bb<-filter(ab, source.b & is.na(source.a));bb;dim(bb)
X11()

ggplot(ab, aes(x=stom_a, y=stom_b,group=Prey,shape=Prey, color=Prey)) + geom_point()+
  facet_grid(Prey ~ .)+ xlab('2019 keyrun')+ylab('2022 keyrun')

ggplot(ab, aes(x=stom_a, y=stom_b,group=Prey,shape=Prey, color=Prey)) + geom_point()+
  facet_wrap(Prey.length.class ~ ., ncol=3)+geom_abline(slope=1,intercept=0,color='black')+
  xlab('2019 keyrun')+ylab('2022 keyrun')

ggplot(ab, aes(x=stom_a, y=stom_b,group=Prey,shape=Prey, color=Prey)) + geom_point()+
  facet_wrap(Prey.length.class ~ ., ncol=3)+geom_abline(slope=1,intercept=0,color='black')+
  xlab('2019 keyrun')+ylab('2022 keyrun')

ggplot(ab, aes(x=stom_a, y=stom_b,group=Prey,shape=Prey, color=Prey)) + geom_point()+
  facet_grid(Prey.length.class ~ Prey)+geom_abline(slope=1,intercept=0,color='black')+
  xlab('2019 keyrun')+ylab('2022 keyrun')

ggplot(filter(ab,Prey !='Other'), aes(x=stom_a, y=stom_b,group=Prey,shape=Prey, color=Prey)) + geom_point()+
  facet_grid(Prey.length.class ~ Prey)+geom_abline(slope=1,intercept=0,color='black')+
  xlab('2019 keyrun')+ylab('2022 keyrun')

ggplot(filter(ab,Prey=='Herring'), aes(x=stom_a, y=stom_b)) + geom_point()+
  facet_wrap(Prey.length.class ~ ., ncol=3)+geom_abline(slope=1,intercept=0,color='black')+ geom_smooth(method = "lm", se = FALSE)+
  xlab('2019 keyrun')+ylab('2022 keyrun')+ggtitle('Herring')
ggplot(filter(ab,Prey=='Sprat'), aes(x=stom_a, y=stom_b)) + geom_point()+
  facet_wrap(Prey.length.class ~ ., ncol=3)+geom_abline(slope=1,intercept=0,color='black')+ geom_smooth(method = "lm", se = FALSE)+
  xlab('2019 keyrun')+ylab('2022 keyrun')+ggtitle('Sprat')


# sum within preys
ab<-ab %>% group_by(Predator,Year, Quarter, Predator.length, Prey) %>% summarize(stom_a=sum(stom_a,na.rm=TRUE),stom_b=sum(stom_b,na.rm=TRUE))
newplot(dev='png',nox=1,noy=1,Portrait=TRUE,filename='stom_diffrence',dir=data.path,w8=8,w11=10,pointsize=12,doGgplot=TRUE)
ggplot(ab, aes(x=stom_a, y=stom_b,group=Prey,shape=Prey, color=Prey)) + geom_point()+
  facet_grid(Prey ~ .)+geom_abline(slope=1,intercept=0,color='red')+
  xlab('2019 keyrun')+ylab('2022 keyrun')
cleanup()

X11()

ggplot(ab, aes(x=stom_a, y=stom_b,group=Prey,shape=Prey, color=Prey)) + geom_point()+
  facet_grid(Prey ~ .)+geom_abline(slope=1,intercept=0,color='red')+
  xlab('2019 keyrun')+ylab('2022 keyrun')


