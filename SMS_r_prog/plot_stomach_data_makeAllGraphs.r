
## prog ini 1
# read data from ASCII file and do some graphical initialisations
# you have to do this after every new SMS run
#################################################################
my.colors<-c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )

#my.colors<-my.colors[1:length(new.names)]
palette(my.colors)

#rm(stom)
if (!(exists("stomach.dir"))) stomach.dir<-data.path
if (!(exists("stomach.dir.old.new"))) stomach.dir.old.new<-data.path
if (!(exists("stom.resid.dir"))) stom.resid.dir<-data.path
if (!(exists("stom.resid.dir.obsEst"))) stom.resid.dir.obsEst<-data.path
if (!(exists("stom.resid.dir.stan_resid"))) stom.resid.dir.stan_resid<-data.path
if (!(exists("stom.resid.dir.raw_resid"))) stom.resid.dir.raw_resid<-data.path
if (!(exists("stom.resid.dir.pl_resid"))) stom.resid.dir.pl_resid<-data.path
if (!(exists("stom.resid.dir.like"))) stom.resid.dir.like<-data.path
if (!(exists("stom.resid.dir.comp_resid"))) stom.resid.dir.comp_resid<-data.path

if (!(exists("stomach.size.dir"))) stomach.size.dir<-data.path

if (!exists("make_compositional_residuals")) make_compositional_residuals<-TRUE


stom<-Read.stomach.data(read.init.function=T) %>% rename(Area=SMS.area)

#stom<-subset(stom,Predator=='G.gurnards')

a<-aggregate(list(Pred.avail=stom$Prey.avail.part),
              list(Area=stom$Area,Year=stom$Year,Quarter=stom$Quarter,Predator=stom$Predator,Predator.length.class=stom$Predator.length.class),sum)
stom<-merge(stom,a)

stom$stomcon.hat.part<-stom$Prey.avail.part/stom$Pred.avail 
stom$Residual.part<-stom$stom.input-stom$stomcon.hat.part

area.names<-Read.area.names()
no.areas<-length(area.names)
size.selection<-SMS.control@size.selection
stomach.variance<-SMS.control@stomach.variance
################################################################

# Peter Lewy's Dirichlet residuals
stom$pl.theta<-stom$N.samples/stom$Diri.sum.p
stom$pl.E<-stom$N.samples*stom$Diri.p/stom$Diri.sum.p
stom$pl.Var<-stom$Diri.p*stom$pl.theta^2
stom$pl.sd<-sqrt(stom$pl.Var)
stom$pl.res<-((stom$N.samples*stom$stomcon)-stom$pl.E)/stom$pl.sd
stom$pl.y<-pgamma(stom$N.samples*stom$stomcon,shape=stom$Diri.p,scale=stom$pl.theta)

# Already include as SMS output
if (FALSE) { #stom$mv.var==stom$Stom.var and   stom$mv.stan.res==stom$stan.residual
  stom$mv.var<-(stom$stomcon.hat*(1-stom$stomcon.hat))/(stom$Diri.sum.p+1)
  stom$mv.var-stom$Stom.var
  stom$mv.stan.res<-stom$Residual/sqrt(stom$Stom.var)
  stom$mv.stan.res-stom$stan.residual
}

# identify stomachs with only 1 food (species) item
a<-stom %>% dplyr::select(Area,Year,Quarter,Predator.no,Predator.length.class,Prey.no,Diri.p,stomcon) 

filter(a,Year==1993 & Quarter=="Q2" & Predator.length.class==11)
# obs with only 1 food item are excluded
one<- a %>% group_by(Area,Year,Quarter,Predator.no,Predator.length.class,Prey.no) %>% summarise(preycon=sum(stomcon)) %>% 
  filter(preycon==1) %>% ungroup() %>% mutate(preycon=NULL,Composistional=FALSE,Prey.no=NULL) 

stom<-left_join(stom,one) %>% 
  mutate(Composistional=if_else(is.na(Composistional),TRUE,FALSE), ratio=Predator.size/Prey.size,split=paste(Area,Year,Quarter,Predator.no,Predator.length.class))  %>% 
  filter(stom.used.all==1) %>%  
  arrange(Area,Year,Quarter,Predator.no,Predator.length.class,Prey.no)

write.csv(stom, file = file.path(data.path,'ASCII_stom_data.csv'),row.names = FALSE)
names(stom)



tr<-trellis.par.get( "background")
# old value  tr$col="#909090"
tr$col="white"
trellis.par.set("background", tr)

dev<-"print"
dev<-"screen"
nox<-4
noy<-3


##############################################################
## prog rel 1
# Observed relative stomach contents by predator, year and quarter

cleanup()

dev<-"print"
#dev<-"screen"
dev<-"png"
nox<-2
noy<-3

i<-0
b<- tapply(stom$stomcon,list(stom$Prey.no),sum)
all.prey.col<-sort(as.numeric(dimnames(b)[[1]]),decreasing = TRUE)
all.names<-rep('aaa',length(all.prey.col))
for (s in (1:length(all.prey.col))) all.names[s]<-sp.other.names[all.prey.col[s]+1]
all.names

if (length(all.names<5)) my.cex <- 1.5 else my.cex<-1

#stom<-droplevels(subset(stom,Predator %in% c("W. mackerel","N. mackerel")))

stom<-subset(stom,stom.used.like==1)  # just used stomachs
 
# Fill the page with plots
if (FALSE) a<-by(stom,list(stom$Quarter,stom$Year,stom$Predator.no),function(x) {
   if (dim(x)[[1]]>0) {
    b<- tapply(x$stomcon,list(x$Prey.no,x$Predator.length),sum) 
    b[is.na(b)]<-0
    prey.names<-as.numeric(dimnames(b)[[1]])
    length.names<-dimnames(b)[[2]]
    #if (x[1,]$Quarter=="Q1") {
    if ((i %% (nox*noy-1))==0) {
      newplot(dev,nox,noy,Portrait=F,filename=paste(x[1,]$Year,x[1,]$Quarter,x[1,]$Predator,sep='-'),dir=stomach.dir);
      par(mar=c(3,5,3,2)) 
      plot.new(); legend(x=0,y=1,all.names,fill=all.prey.col,cex=my.cex)
    }    
    i<<-i+1
    barplot(b,names=length.names,col=prey.names)
    title(main=paste(x[1,]$Year,x[1,]$Quarter," Pred:",x[1,]$Predator))
   }
})
if (dev=='png') cleanup()



# plot by year with max 4 quarters

a<-by(stom,list(stom$Year,stom$Predator.no),function(x) {
  if (dim(x)[[1]]>0) {
    if (dev=='png') cleanup()
    newplot(dev,nox,noy,Portrait=F,filename=paste('stom',x[1,]$Predator,'LABEL',x[1,]$Year,sep='-'),dir=stomach.dir);
    par(mar=c(3,5,3,2)) 
    plot.new(); legend(x=0,y=1,all.names,fill=all.prey.col,cex=my.cex)
    
    by(x,list(x$Quarter), function(xx){
      b<- tapply(xx$stomcon,list(xx$Prey.no,xx$Predator.length),sum) 
      b[is.na(b)]<-0
      prey.names<-as.numeric(dimnames(b)[[1]])
      length.names<-dimnames(b)[[2]]
      barplot(b,names=length.names,col=prey.names)
      title(main=paste(xx[1,]$Year,xx[1,]$Quarter," Pred:",xx[1,]$Predator))
      
    })
  }
})
if (dev=='png') cleanup()



##################################################################################
# compare old and new stomachs
compare_stom_all<-function(stomDirs=file.path(root,dirs.keyrun), stomLabes=labels.keyrun,out.dir=data.path) {
  stom<-Read.stomach.data.start(dir=stomDirs[1])
  a<-subset(stom,select=c( Predator.no,Predator,Prey.no,Prey,Quarter,Year, Predator.length, Predator.length.mean, Predator.size, Prey.length.class, Prey.length.mean,  Prey.size, N.haul,stomcon.input))
  
  stom<-Read.stomach.data.start(dir=stomDirs[2])
  b<-subset(stom,select=c( Predator.no,Predator,Prey.no,Prey,Quarter,Year, Predator.length, Predator.length.mean, Predator.size, Prey.length.class, Prey.length.mean,  Prey.size, N.haul,stomcon.input ))
  
  
  #differences predator information
  aa<-subset(a,select=c( Predator.no,Predator, Quarter, Year, Predator.length )) %>% mutate(source.a=TRUE) %>% unique()
  bb<-subset(b,select=c( Predator.no,Predator, Quarter, Year, Predator.length )) %>% mutate(source.b=TRUE) %>% unique()
  
  if (FALSE) {
    ab<-full_join(aa,bb)
    filter(ab, source.a & is.na(source.b))
    filter(ab, source.b & is.na(source.a))
  }
  
  in_both<-inner_join(aa,bb) %>% dplyr::select( Predator.no,,Predator,Quarter,Year,Predator.length) %>% unique()
  
  a<-right_join(a,in_both) %>% mutate(source.a=TRUE)
  b<-right_join(b,in_both)%>% mutate(source.b=TRUE)
  
  
  #differences prey species
  aa<-subset(a,select=c(Predator, Quarter, Year, Predator.length,Prey,source.a )) %>% unique()
  bb<-subset(b,select=c(Predator, Quarter, Year, Predator.length,Prey,source.b  ))  %>% unique()
  
  ab<-full_join(aa,bb) %>% arrange(  Predator,Year, Quarter, Predator.length, Prey )
  
  if (FALSE) {
    filter(ab, source.a & is.na(source.b))
    filter(ab, source.b & is.na(source.a))
  }
  
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
        newplot(dev,nox,noy,Portrait=F,filename=paste('comp_stmch_',x[1,]$Predator,x[1,]$Year,sep='-'),dir=out.dir);
        par(mar=c(3,5,3,2))
        if (dev=="wmf" ) par(mar=c(2,4,2,2))
        
        # text(x=0.0,y=0.07,"lower: observed",pos=4)
        plot.new();
        title(main=list(paste0("\nupper:",stomLabes[1],"\nlower:",stomLabes[2]),cex=my.cex*1.25))
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
}

compare_stom_all(out.dir=stomach.dir.old.new)


##################################################################################
## prog rel 4
# Observed and predicted relative stomach content by predator quarter and year

cleanup()


dev<-"png"
#dev<-"screen"
nox<-2
noy<-3

i<-0
b<- tapply(stom$stomcon,list(stom$Prey.no),sum)
all.prey.col<-sort(as.numeric(dimnames(b)[[1]]),decreasing = TRUE)
all.names<-rep('aaa',length(all.prey.col))
for (s in (1:length(all.prey.col))) all.names[s]<-sp.other.names[all.prey.col[s]+1]

oldY<-0

if (TRUE) by(stom,list(stom$Quarter,stom$Year,stom$Predator.no),function(x) {
   newY<<-x[1,'Year']
    b<- tapply(x$stomcon,list(x$Prey.no,x$Predator.length),sum)
    b[is.na(b)]<-0

    c<- tapply(x$stomcon.hat,list(x$Prey.no,x$Predator.length),sum)
    c[is.na(c)]<-0

    b<-rbind(b,c)
    prey.no<-as.numeric(dimnames(b)[[1]])
    prey.names<-rep('aaa',length(prey.no))
    for (s in (1:length(prey.names))) prey.names[s]<-sp.other.names[prey.no[s]+1]
    length.names<-dimnames(b)[[2]]
    if (oldY != newY) {
       if (dev=='png') cleanup()
      newplot(dev,nox,noy,Portrait=F,filename=paste('obs_est_st_',x[1,]$Predator,x[1,]$Year,sep='-'),dir=stom.resid.dir.obsEst);
       par(mar=c(3,5,3,2))
      if (dev=="wmf" ) par(mar=c(2,4,2,2))
      
     # text(x=0.0,y=0.07,"lower: observed",pos=4)
      plot.new();
      title(main="upper: expected\nlower: observed")
      legend("center",all.names,fill=all.prey.col,cex=my.cex)
      oldY<<-newY   
    }
    barplot(b,names=length.names,col=prey.no)
    title(main=paste(x[1,]$Year,x[1,]$Quarter," Pred:",x[1,]$Predator))
    #title(main=paste(x[1,]$Year,x[1,]$Quarter))

    abline(h=1,lwd=2)
})
cleanup()

if (make_compositional_residuals) {
  # Anders & Vanessa residuals
  if (FALSE) {
    TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
    Sys.unsetenv("GITHUB_PAT")
    devtools::install_github("fishfollower/compResidual/compResidual", INSTALL_opts=c("--no-multiarch"))
  }
  library(compResidual)
  a<- stom %>% filter(Composistional) %>% dplyr::select(split,Prey.no,Diri.p,stomcon)
  last_and_omitted_prey_no<-0   

  qq<-by(a,a$split,function(x){
    # test x<-subset(a,split==a[14,'split'])
    cat( x[1,"split"],'\n') 
    x[x$Prey.no==last_and_omitted_prey_no,'Prey.no']<-99
    x<- x %>% arrange(Prey.no)
    aa<- dplyr::select(x,-stomcon) %>% pivot_wider(values_from=Diri.p,names_from=Prey.no) 
    l<-dim(aa)[[2]]
    alpha<-t(as.matrix(aa[1,2:l],ncol=1))
    o<- dplyr::select(x,-Diri.p) %>% pivot_wider(values_from=stomcon,names_from=Prey.no)  
    obs<-t(as.matrix(o[1,2:l],ncol=1))
    obs<-obs/sum(obs)
    res<-resDir(obs,alpha)
    x<-dplyr::select(x,split,Prey.no) 
    x<-data.frame(x,cres=c(as.numeric(res),NA))
    x[x$Prey.no==99,'Prey.no']<-last_and_omitted_prey_no
    return(x)
  })
  qq[[6]]
  b<-do.call(rbind,qq)
  
  
  # add variables to residual set
  a<- stom %>% dplyr::select(split,Area,Year,Quarter,Predator.no,Predator,Predator.length.class,Predator.length,N.samples,Size.model,Prey.no,Prey,Diri.like,Diri.sum.p,Diri.p,stomcon,stomcon.hat,pl.res,Residual,stan.residual)
  ab<-left_join(a,b) %>% mutate(split=NULL)
  head(ab)
} else {
  ab<- stom %>% dplyr::select(split,Area,Year,Quarter,Predator.no,Predator,Predator.length.class,Predator.length,N.samples,Size.model,Prey.no,Prey,Diri.like,Diri.sum.p,Diri.p,stomcon,stomcon.hat,pl.res,Residual,stan.residual) %>% mutate(cres=0)
}


predators<-ab %>% dplyr::select(Predator,Predator.no) %>% unique() %>% arrange(Predator.no) %>%mutate(Predator.no=NULL) %>% as.vector
predators<-predators[[1]]


if (FALSE) {
  X11() # just checking
  nsamp<- stom %>% dplyr::select(Year,Quarter,Predator.no,Predator.length.class,N.samples,Diri.like) %>% unique()
  hist(nsamp$N.samples)
  X11()
  hist(nsamp$Diri.like)
}

if (FALSE) {
  if (make_compositional_residuals) head(ab %>% arrange(desc(abs(cres))),n=20) %>% dplyr::select(-Size.model,-Area,-Predator.length,-Predator.no,-Prey.no) %>% as_tibble()
  head(ab %>% arrange(desc(abs(pl.res))),n=20) %>% dplyr::select(-Size.model,-Area,-Predator.length,-Predator.no,-Prey.no)%>% as_tibble()
  head(ab %>% arrange(desc(abs(stan.residual))),n=20) %>% dplyr::select(-Size.model,-Area,-Predator.length,-Predator.no,-Prey.no)%>% as_tibble()
 head(ab %>% arrange(desc(Diri.like)),n=20) %>% dplyr::select(-Size.model,-Area,-Predator.length,-Predator.no,-Prey.no)%>% as_tibble()
}  

lapply(predators,function(pred) {
 
  x<-filter(ab,Predator==pred)
  
  newplot(dev="png",nox=2,noy=1,w8=5,w11=8,filename=paste0("QQ_resid_1_",pred),dir=stom.resid.dir.raw_resid);     
  hist(x$Residual,xlab="Residual",main=paste(pred,'\nraw residuals'))
  qqnorm(x$Residual)
  qqline(x$Residual)
  
  newplot(dev="png",nox=2,noy=1,w8=5,w11=8,filename=paste0("QQ_resid_2_",pred),dir=stom.resid.dir.stan_resid);     
  hist(x$stan.residual,xlab="Residual",main=paste(pred,'\nstandardized residuals'))
  qqnorm(x$stan.residual)
  qqline(x$stan.residual)

  newplot(dev="png",nox=2,noy=1,w8=5,w11=8,filename=paste0("QQ_resid_3_",pred),dir=stom.resid.dir.pl_resid);      
  hist(x$pl.res,xlab="Residual",main=paste(pred,'\nPeter Lewy'))
  qqnorm(x$pl.res)
  qqline(x$pl.res)
  
  if (make_compositional_residuals) {
    newplot(dev="png",nox=2,noy=1,w8=5,w11=8,filename=paste0("QQ_resid_4_",pred),dir=stom.resid.dir.comp_resid);     
    hist(x$cres,xlab="Residual",main=paste(pred,'\none step ahead quantile residuals '))
    qqnorm(x$cres)
    qqline(x$cres)
  }
  cleanup()
})



###############################################################
# Bubble residual plot
#
stomBuble<-function(my.pred='Cod',my.area=1,my_dir=data.path,my.dev=my.dev.used,filename='a',time.order="quarter-year",pred.order="pred-size",pred.prey.order="prey-pred",scale=2,Res.type=1,use.likelihood=FALSE,make.table=FALSE,...) {
  # res type
  # 1= observed-predicted
  # 2= standardised 
  # 3= Peter Lewy's suggestion
  # 4= compositional Anders & Vanessa
  
  stom3<-(subset(ab,Predator %in% my.pred & Size.model %in% c(0,1,4,11) & Area==my.area,
                 select=c(Predator,Quarter,Year,Predator.length.class ,Predator.length,Prey,Prey.no,N.samples,Diri.like,stan.residual,Residual,pl.res,cres)))
  
  if (use.likelihood) {
    stom3<-unique(subset(stom3,select=c(-stan.residual,-Residual,-Prey,-Prey.no)))
    stom3$Residual<-stom3$Diri.like
  } else if (Res.type==2) stom3$Residual<-stom3$stan.residual else if (Res.type==3) stom3$Residual<-stom3$pl.res else if (Res.type==4) stom3$Residual<-stom3$cres
  
  
  if (time.order== "quarter-year") stom3$time<-paste(stom3$Quarter,stom3$Year,sep=' ')
  if (time.order== "year-quarter") stom3$time<-paste(stom3$Year,stom3$Quarter,sep=' ')
  if (use.likelihood) stom3$plPrey<-'' else stom3$plPrey<-paste(stom3$Prey.no,") ",stom3$Prey,sep='')
  if (pred.order=="pred-size") stom3$plPred<-paste(stom3$Predator,stom3$Predator.length/10,sep='-')
  if (pred.order=="size-pred") stom3$plPred<-paste(stom3$Predator.length/10,stom3$Predator,sep='-')
  
  s<-droplevels(subset(stom3,select=c(time,plPrey,plPred,Predator,Predator.length,Residual)))
  if (pred.prey.order=="pred-prey") a<-tapply(s$Residual,list(s$plPred,s$plPrey,s$time),sum,na.rm=T)
  if (pred.prey.order=="prey-pred") a<-tapply(s$Residual,list(s$plPrey,s$plPred,s$time),sum,na.rm=T)
  
  if (make.table) ftable(round(a,3))
  bb<-data.frame(ftable(a))
  names(bb)<-c('Prey','Pred','Period','Value')
  ylabs<-sort(unique(paste(bb$Prey,bb$Pred)))
  xlabs<-sort(unique(bb$Period))
  b<-as.matrix(ftable(a))
  b<-arr2df(b)
  names(b)<-c('y','x','v')
  b<-subset(b,!is.na(v))
  
  bp3<-function(x,y,v, scale=3,xlabs,ylabs,col1='seagreen3',col2='red',...){
    plot(x,y,cex=sqrt(abs(v))*scale, col=col1, pch=ifelse(v<0,16,1), axes=F,ylab='',xlab='',main=my.pred) #main=area.names[my.area]
    axis(2,at=1:length(ylabs),labels=ylabs,las=1,cex.axis=0.7)
    axis(1,at=1:length(xlabs),labels=xlabs,las=2,cex.axis=0.7)
    box()
    points(x,y,cex=sqrt(abs(v))*scale, col=col2, pch=1)
  }
  
  newplot(dev=my.dev,nox=1,noy=1,filename=filename,dir=my_dir);
  par(mar=c(4,10,1,2)) #c(bottom, left, top, right)
  bp3(x=as.numeric(b$x),y=b$y,v=b$v, scale=scale,xlabs=xlabs,ylabs=ylabs)
}



cleanup()




my.dev.used<-'png'

plot_stom_resid<-function(predator,resid=1,scale=1.3,dir) {
  for (are in (1:no.areas)) stomBuble(my.area=are,my_dir=dir,my.pred=predator,time.order="quarter-year",pred.order="pred-size",scale=scale,Res.type=resid,filename=paste0('ar',are,'_',predator,'_qy_stom_resid_',resid))
  for (are in (1:no.areas)) stomBuble(my.area=are,my_dir=dir,my.pred=predator,time.order="year-quarter",pred.order="pred-size",scale=scale,Res.type=resid,filename=paste0('ar',are,'_',predator,'_yq_stom_resid_',resid))
  cleanup()
}

plot_stom_likelihood<-function(predator,scale=1.3,dir) {
  for (are in (1:no.areas))  stomBuble(my.area=are,my_dir=dir,my.pred=predator,my.dev='png',time.order="year-quarter",pred.order="pred-size",scale=1,use.likelihood=T,filename=paste0('ar',are,'_',predator,'_yq_stom_likelihood'))
  for (are in (1:no.areas)) stomBuble(my.area=are,my_dir=dir,my.pred=predator,my.dev='png',time.order="quarter-year",pred.order="pred-size",scale=1,use.likelihood=T,filename=paste0('ar',are,'_',predator,'_qy_stom_likelihood'))
  cleanup()
}

lapply(predators,function(pred) {
  plot_stom_resid(predator=pred,resid=1,scale=3,dir=stom.resid.dir.raw_resid) 
  plot_stom_resid(predator=pred,resid=2,scale=1.3,dir=stom.resid.dir.stan_resid) 
  plot_stom_resid(predator=pred,resid=3,scale=1.3,dir=stom.resid.dir.pl_resid) 
  if (make_compositional_residuals) plot_stom_resid(predator=pred,resid=4,scale=1.3,dir=stom.resid.dir.comp_resid) 
  plot_stom_likelihood(predator=pred,scale=1.3,dir=stom.resid.dir.like)
  cleanup()
})



##############################################################
# Box-plot of residuals by predator, prey and quarter
# prog box 1


Res.type<-1
b<-ab

if (Res.type==1) b$box.res<-b$Residual else if (Res.type==2) b$box.res<-b$stan.residual else if (Res.type==3) b$box.res<-b$pl.res else if (Res.type==4) b$box.res<-b$cres

b<-filter(b,!is.na(box.res))
cleanup()

res.ylim<-c(-0.05,0.05)
res.ylim<-c(-0.05,0.05)
res.ylim<-c(-0.1,0.1)

my.pred<-c("Cod")
my.col<-"red"
nox<-3; noy<-4;
noxy<-nox*noy
i<-0
i<-nox*noy
b$yq<-paste(b$Year,b$Quarter)
b$Length<-round(b$Predator.length/10)
dev<-'png'
xx<-by(b,list(b$Predator,b$Prey,b$Area),function(x) {
  if (i==noxy) {print('newplot'); newplot(dev,nox,noy,filename=paste0('aa_stom_box_',x[1,'Predator']),dir=stom.resid.dir.raw_resid); i<<-0 ;  par(mar=c(3,2,3,2)) }#c(bottom, left, top, right)
  if (x[1,"Prey"]=="Other" ) fac<-1 else fac<-1
  
  boxplot(Residual~yq,data=x,ylim=6*fac*res.ylim)
  abline(h=0,col=my.col)
  #  title(main=paste(area.names[x[1,'Area']],x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
  title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
  i<<-i+1
  
  boxplot(Residual~Year,data=x,ylim=6*fac*res.ylim)
  abline(h=0,col=my.col)
  title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
  i<<-i+1
  
  boxplot(Residual~Quarter,data=x,ylim=3*fac*res.ylim)
  abline(h=0,col=my.col)
  title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
  i<<-i+1
  
  boxplot(Residual~Length,data=x,ylim=3*fac*res.ylim)
  abline(h=0,col=my.col)
  title(main=paste(x[1,]$Predator,"eating",x[1,]$Prey,sep=" "))
  i<<-i+1
  
})


cleanup()

######################################
plot_all_figs<-FALSE


stom2<-subset(stom,Size.model>0 & stom$Prey.no != 0)

if (plot_all_figs) {
  nox<-4; noy<-4;
  #cleanup()
  dev<-'screen'
  newplot(dev="screen",filename='stom_box',nox,noy,Portrait=F);
  #newplot(dev="png",filename='stom_box',nox,noy,Portrait=F);
  par(mar=c(3,2,3,2)) #c(bottom, left, top, right)
  i<-0
  
  if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
    by(stom2,list(stom2$Predator),function(x) {
      if (i==noxy) {newplot(dev,nox,noy); i<<-0 }  
      hist(log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(pred-size / prey-size) ')
      abline(h=0, lty=3) 
      title(main=paste("Pred:",x[1,]$Predator,sep=""))
      i<<-i+1
    }
    )}
}
##################################


stom3<-subset(stom,Size.model>0 & Prey.no>0 & N.haul>=10 & Predator !="NS Mackerel" & Predator !="W Mackerel" )
#stom3<-subset(stom,Size.model>0 & Prey.no>0 & N.haul>10  )
#stom3<-subset(stom3, !((Predator=='Cod' & Predator.length.mean<400)| (Predator=='Whiting' & Predator.length.mean<200)  | (Predator=='Saithe' & Predator.length.mean<400)))

# start with a high fac for calc of mean and var
fac<-50  
a<-aggregate(list(stom=stom3$stomcon),list(Predator=stom3$Predator,PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*50)/50),sum)

by(a,list(a$Predator),function(x) weighted.mean(x$PredPrey, x$stom))


if (plot_all_figs) barchart(stom~as.factor(trunc(PredPrey*2)/2)|Predator, data=a, 
                            xlab='log(predator weight/prey weight)',ylab='proportion',
                            strip = strip.custom( bg='white'),par.strip.text=list(cex=0.8, lines=2),
                            layout = c(1, 3) ,col='grey')



####################################
# observation by predator-prey size ratio
## prog size 1b

cleanup()
nox<-2; noy<-2;
newplot(dev,nox,noy);
i<-0


stom3<-subset(stom,  (Size.model==1 | Size.model==11) &  Prey.no>0 )


# just checking
a<-aggregate(list(stom=stom3$stomcon,stom.input=stom3$stom.input,stom.hat=stom3$stomcon.hat,stom.hat.part=stom3$stomcon.hat.part),
             list(Year=stom3$Year,Quarter=stom3$Quarter,Predator=stom3$Predator,Pred_l=stom3$Predator.length),sum)

# use the individual data points (not the liklihood data) when size mode is 11                       
stom3[stom3$Size.model==11,]$stomcon<-    stom3[stom3$Size.model==11,]$stom.input
stom3[stom3$Size.model==11,]$stomcon.hat<-stom3[stom3$Size.model==11,]$stomcon.hat.part

stom3<-droplevels(stom3)

# start with a high fac for calc of mean and var
fac<-50  
a<-aggregate(list(stom=stom3$stomcon*5000,stom.hat=stom3$stomcon.hat*5000)
             ,list(Predator=stom3$Predator,PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*fac)/fac),sum)

# by(a,list(a$Predator),function(x) weighted.mean(x$PredPrey, x$stom))

bb<-data.frame(size=rep(a$PredPrey,trunc(a$stom)),Predator=rep(a$Predator,trunc(a$stom)))
k1<-aggregate(list(mean=bb$size),list(Predator=bb$Predator),mean)
k2<-aggregate(list(var=bb$size),list(Predator=bb$Predator),var)
stat<-merge(k1,k2)

bb<-data.frame(size=rep(a$PredPrey,trunc(a$stom.hat)),Predator=rep(a$Predator,trunc(a$stom.hat)))
k1<-aggregate(list(mean.hat=bb$size),list(Predator=bb$Predator),mean)
k2<-aggregate(list(var.hat=bb$size),list(Predator=bb$Predator),var)
stat.hat<-merge(k1,k2)

stat<-merge(stat,stat.hat) 
stat

# decrease fac for a  nice plot
fac<-2
a<-aggregate(list(stom=stom3$stomcon, stomNo=stom3$stomcon/stom3$Prey.weight,stom.hat=stom3$stomcon.hat),list(Predator=stom3$Predator,
                                                                                                              PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*fac)/fac),sum)                                     
b<-aggregate(list(sumstom=a$stom, sumstomNo=a$stomNo, sumstom.hat=a$stom.hat),list(Predator=a$Predator),sum)
a<-merge(a,b)
a<-merge(a,stat)
a$stom<-a$stom/a$sumstom
a$stomNo<-a$stomNo/a$sumstomNo
a$stom.hat<-a$stom.hat/a$sumstom.hat

## observed
a$headt<-paste(a$Predator,", mean=",formatC(a$mean,digits=2,format='f')," variance=",formatC(a$var,digits=2,format='f'),sep='')
if (plot_all_figs) {
  cleanup()
  trellis.device(device = "windows",
                 color = T, width=18, height=18,pointsize = 12,
                 new = TRUE, retain = FALSE)
  
  # for the paper
  barchart(stom~as.factor(PredPrey)|headt, data=a, 
           groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(1,1) ,col='grey')
  
}

b1<-a
b1$type<-paste(a$headt,'\nObserved',sep='');

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')
if (plot_all_figs) {
  trellis.device(device = "windows", 
                 color = T, width=9, height=9,pointsize = 22,
                 new = TRUE, retain = FALSE)
  
  # for the paper
  barchart(stom.hat~as.factor(PredPrey)|headt, data=a, 
           groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
           xlab='log(predator weight/prey weight)',ylab='proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=0.8, lines=2),
           scales = list(x = list(rot = 45, cex=0.8), y= list(alternating = 1,cex=0.8)),
           layout = c(1, 1) ,col='grey')
}

b2<-a

b2$type<-paste(a$headt,'\nPredicted',sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)

if (plot_all_figs) {
  trellis.device(device = "windows", 
                 color = T, width=17, height=16,pointsize = 12,
                 new = TRUE, retain = FALSE)
  
  barchart(stom~as.factor(PredPrey)|type, data=aa, 
           groups = Predator, stack = TRUE,      # this line is not needed, but it gives a nicer offset on the Y-axis
           
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(2, 1) ,col='grey')
}

#####################
# same as above, but with contributions from prey species

fac<-2
a<-aggregate(list(stom=stom3$stomcon, stomNo=stom3$stomcon/stom3$Prey.weight,stom.hat=stom3$stomcon.hat),list(Predator=stom3$Predator,
                                                                                                              Prey=stom3$Prey,
                                                                                                              PredPrey=trunc(log(stom3$Predator.size/stom3$Prey.size)*fac)/fac),sum)
b<-aggregate(list(sumstom=a$stom, sumstomNo=a$stomNo, sumstom.hat=a$stom.hat),list(Predator=a$Predator),sum)
a<-merge(a,b)
a<-merge(a,stat)
a$stom<-a$stom/a$sumstom
a$stomNo<-a$stomNo/a$sumstomNo
a$stom.hat<-a$stom.hat/a$sumstom.hat

## observed
a$headt<-paste(a$Predator,", mean=",formatC(a$mean,digits=2,format='f')," variance=",formatC(a$var,digits=2,format='f'),sep='')

#cleanup()
trellis.device(device = "windows",
               color = T, width=17, height=17,pointsize = 12,
               new = TRUE, retain = FALSE )

b<- tapply(stom3$stomcon,list(stom3$Prey),sum)
col<-1:6
lab<-dimnames(b)[[1]]

#cleanup()
if (plot_all_figs) {
  trellis.device(device = "windows",
                 color = T, width=7, height=7,pointsize = 2,
                 new = TRUE, retain = FALSE)
  
  
  barchart(stom~as.factor(PredPrey)|headt, data=a,
           groups = Prey, stack = TRUE,  col=2:3,
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=2:3),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(1, 1) )
}

b1<-a
#b1$type<-paste(a$headt,'\nObserved',sep='');
b1$type<-paste('Observed\n',a$head,sep='');

## predicted
a$headt<-paste(a$Predator," mean=",formatC(a$mean.hat,digits=2,format='f')," var=",formatC(a$var.hat,digits=2,format='f'),sep='')
if (plot_all_figs) {
  trellis.device(device = "windows",
                 color = T, width=7, height=7,pointsize = 2,
                 new = TRUE, retain = FALSE)
  
  barchart(stom.hat~as.factor(PredPrey)|headt, data=a,
           groups = Prey, stack = TRUE,  col=2:3,
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=2:3),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(1, 1) )
}

b2<-a

b2$type<-paste('Predicted\n',a$headt,sep='');

# both on the same plot
b2$stom<-b2$stom.hat
aa<-rbind(b1,b2)
aa<-droplevels(aa)

if (plot_all_figs) {
  trellis.device(device = "windows", 
                 color = T, width=9, height=7,pointsize = 12,
                 new = TRUE, retain = FALSE)
  
  
  barchart(stom~as.factor(PredPrey)|type, data=aa, 
           groups = Prey, stack = TRUE,  col=2:3,    # this line is not needed, but it gives a nicer offset on the Y-axis
           xlab='log(predator weight/prey weight)',ylab='Proportion',
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=2),
           key = list(text = list(label = lab,col=1), rectangles = TRUE, space = "right", col=2:3),
           scales = list(x = list(rot = 45, cex=1), y= list(alternating = 1,cex=1)),
           layout = c(2, 1) )
  
  
}

ggplot(aa, aes(x=PredPrey, y=stom, fill=Prey)) +
  geom_bar(stat="identity")+theme_minimal()+
  facet_wrap(~type,  ncol=1)+xlab('log(predator weight/prey weight)')+ylab('Proportion')
newplot(dev='png',nox=1,noy=1,Portrait=F,filename=paste('size_all','Cod',sep='-'),dir=stomach.size.dir,doGgplot=TRUE);

aa$type2<-if_else(grepl('Observed',aa$type),'Observed','Predicted')
ggplot(aa, aes(x=PredPrey, y=stom, fill=Prey)) +
  geom_bar(stat="identity")+theme_minimal()+
  facet_grid(rows=vars(type2),cols=vars(Prey))+xlab('log(predator weight/prey weight)')+ylab('Proportion')
newplot(dev='png',nox=1,noy=1,Portrait=F,filename=paste('size_prey','Cod',sep='-'),dir=stomach.size.dir,doGgplot=TRUE);

cleanup()




#######################################



palette("default")
