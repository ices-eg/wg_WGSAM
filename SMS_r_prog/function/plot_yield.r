plot_yield<-function(
  Portrait=T,                 # graphical output orientation
  first.year= -1974,          #first year on plot, negative value means value defined by data
  last.year= 2300,             #last year on plot
  my.dev=c('screen','wmf', 'png', 'pdf')[3],
  out_dir=data.path,
  my.colors=c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )) {

  cleanup()
  palette("default")

    my.colors<-my.colors[1:4]
  palette(my.colors)
  
  dat<-Read.summary.data(extend=FALSE,read.init.function=F) %>% filter(Yield>0)
  yq<-aggregate(Yield~Species.n+Species+Year+Quarter,data=dat,FUN=sum,na.rm=TRUE)
 
tmp<-by(yq,list(yq$Species.n),function(x) {
  filename<-paste0('Yield_',x[1,"Species"])
  newplot(dev=my.dev,nox=2,noy=1,w8=12,w11=7,Portrait=TRUE,pointsize=12,filename=filename,dir=out_dir);
  
  tot<-tapply(x$Yield,list(x$Quarter,x$Year),sum)/1000
  tot[is.na(tot)]<-0
  barplot(tot,main=x[1,"Species"],
          col=my.colors,
          ylab='Yield (1000 t)',
          legend =rownames(tot),args.legend=list(x="topright",ncol=2,title='Quarter'))
  
  atot<-tot/rep(colSums(tot),each=dim(tot)[1])
  barplot(atot, col=my.colors,ylab='Yield proportion')
  
  if (my.dev %in% c('png','print')) cleanup()
  ###
})
return()
}

#plot_yield()
