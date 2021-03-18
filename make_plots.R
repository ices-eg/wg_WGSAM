# file including functions for various plots


plot_summary_new<-function(res,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),years=c(0,5000),species='Cod',splitLine=FALSE,incl.reference.points=FALSE,nox=2,noy=3) {

  a<-as_tibble(subset(res$out$detail_sum,Year>=years[1] & Year<=years[2] & Species %in% species))
  a2<-subset(histCondensed,Year>=years[1] & Year<=years[2] & Species %in% species)
  a<-bind_rows(a,a2)%>% mutate(Year=as.integer(Year)) %>% arrange(Year) 
  
  b<-subset(res$out$detail_M2,Year>=years[1] & Year<=years[2] & Species %in% species)
  b2<-subset(histAnnoM,Year>=years[1] & Year<=years[2] & Species %in% species)
  b<-bind_rows(b,b2) %>% arrange(Year,Age)
  
   if ("SSB" %in% ptype) {
    pSSB<-ggplot(a,aes(x=Year,y=SSB))+
    geom_line(lwd=1.5)+
    geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 1.5)+
    labs(x='',y=plotLabels['SSB'],title=paste(species,'SSB',sep=', '))+
    ylim(0,max(a$SSB))+
    theme(plot.title = element_text(size = 16, face = "bold",hjust=0))
    
    if (splitLine) pSSB<- pSSB+geom_vline(xintercept=stq_year,col='blue',lty=3,lwd=1)
    if (incl.reference.points) {
       if ( refPoints[species,'Blim']>0) pSSB<- pSSB+geom_hline(yintercept=refPoints[species,'Blim'],lty=2,lwd=1,col='red')
       if (refPoints[species,'Bpa']>0)   pSSB<- pSSB+geom_hline(yintercept=refPoints[species,'Bpa'],lty=3,lwd=1,col='green')
    }
  }
  

  if ("Recruits" %in% ptype) {
    y <-aggregate(Recruits~Year,data=a,sum)
    pRec<-ggplot(y,aes(x=Year,y=Recruits)) +
      geom_bar(stat = "identity")+
      labs(x='',y=plotLabels['Recruits'],title='Recruitment')+
      theme(plot.title = element_text(size = 16, face = "bold"))
    if (splitLine) pRec<- pRec+geom_vline(xintercept=stq_year,col='blue',lty=3,lwd=1)
  }
  
  if ("Fbar" %in% ptype) {
     pF<-ggplot(a,aes(x=Year,y=Fbar))+
       geom_line(lwd=1.5)+
       geom_point(shape = 21, colour = "black", fill = "white", size = 2, stroke = 1.5)+
       labs(x='',y=plotLabels['Fbar'],title='Fishing mortality, F')+
       ylim(0,max(a$Fbar))+
       theme(plot.title = element_text(size = 16, face = "bold"))
     
     if (splitLine) pF<- pF+geom_vline(xintercept=stq_year,col='blue',lty=3,lwd=1)
     if (incl.reference.points) {
       if ( refPoints[species,'Flim']>0) pF<- pF+geom_hline(yintercept=refPoints[species,'Flim'],lty=2,lwd=1,col='red')
       if (refPoints[species,'Fpa']>0)   pF<- pF+geom_hline(yintercept=refPoints[species,'Fpa'],lty=3,lwd=1,col='green')
     }
  } 
  
  if ("Yield" %in% ptype) {
    y <-aggregate(Yield~Year,data=a,sum)
    pY<-ggplot(y,aes(x=Year,y=Yield)) +
      geom_bar(stat = "identity")+
      labs(x='',y=plotLabels['Yield'],title='Catch')+
      theme(plot.title = element_text(size = 16, face = "bold"))
    if (splitLine) pY<- pY+geom_vline(xintercept=stq_year,col='blue',lty=3,lwd=1)
  }  
  
  if ("Dead" %in% ptype) {
    y<-select(a,Year,Yield,DeadM1,DeadM2)
    y<-as.data.frame(y)
    if (any(y$DeadM2>0)) titl<-'Biomass removed\ndue to F, M1 and M2' else titl<-'Biomass removed\ndue to F and M1'
    y<-reshape(y,direction='long',varying = list(2:4))
    
    pD<-ggplot(y, aes(x = Year, y = Yield, fill = as.factor(time)) )+
       geom_bar(stat = "identity")+ 
       labs(x='',y=plotLabels['DeadM'],title=titl)+
       theme(plot.title = element_text(size = 16, face = "bold"),legend.position="none")
    if (splitLine) pD<- pD+geom_vline(xintercept=stq_year,col='blue',lty=3,lwd=1)
  }
 
  if ("M2" %in% ptype) if (any(b$M2>0)) {
    b$Age<-as.factor(b$Age)
    m2<-aggregate(M2~Year+Age,data=b,sum)
    m2max<-aggregate(cbind(maxM2=M2)~Age,data=b,max)
    m2<-droplevels(subset(merge(m2,m2max),maxM2>0.01 & as.character(Age)<"6"))
    pM<-ggplot(m2,aes(x=Year,y=M2,color=Age, shape=Age)) +
      geom_line(lwd=1)+
      geom_point()+
      labs(x='',y=plotLabels['Fbar'],title='M2 at age')+
      ylim(0,max(m2max$maxM2))+
      theme(plot.title = element_text(size = 16, face = "bold"),legend.position="none")
    
    if (splitLine) pM<- pM+geom_vline(xintercept=stq_year,col='blue',lty=3,lwd=1)
  }
  if (any(b$M2>0)) out<-plot_grid(pSSB,pD,pF,pRec,pY,pM,ncol=3, nrow=2) else out<-plot_grid(pSSB,pD,pF,pRec,pY,ncol=3, nrow=2)
  return(out)
}

 #plot_summary_new(res=res$rv,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),years=c(0,5000),species='Cod',splitLine=FALSE,incl.reference.points=FALSE) 


plot_summary_old<-function(res,ptype=c('Yield','Fbar','SSB','Recruits','Dead','M2'),years=c(0,5000),species='Cod',splitLine=FALSE,incl.reference.points=FALSE,nox=2,noy=3) {
  a<-subset(res$out$detail_sum,Year>=years[1] & Year<=years[2] & Species %in% species)
  a2<-subset(histCondensed,Year>=years[1] & Year<=years[2] & Species %in% species)
  a<-bind_rows(a,a2)%>% arrange(Year)
  
  b<-subset(res$out$detail_M2,Year>=years[1] & Year<=years[2] & Species %in% species)
  b2<-subset(histAnnoM,Year>=years[1] & Year<=years[2] & Species %in% species)
  b<-bind_rows(b,b2) %>% arrange(Year,Age)
  
  #X11(width=11, height=8, pointsize=12)
  par(mfcol=c(nox,noy))
  par(mar=c(3,4,3,2))
  
  
  
  if ("SSB" %in% ptype) {
    plot(a$Year,a$SSB,type='b',lwd=3,xlab='',ylab=plotLabels['SSB'],main=paste(species,'SSB',sep=', '),ylim=c(0,max(a$SSB)))
    if (splitLine) abline(v=SMS@last.year.model,lty=2, col='red')
    if (incl.reference.points) {
      if ( refPoints[species,'Blim']>0) abline(h=refPoints[species,'Blim'],lty=2,lwd=2,col='red')
      if (refPoints[species,'Bpa']>0) abline(h=refPoints[species,'Bpa'],lty=3,lwd=2,col='green')
    }
  }
  
  if ("Recruits" %in% ptype) {
    y<-tapply(a$Recruits,list(a$Year),sum)
    barplot(y,space=1,ylab=plotLabels['Recruits'],xlab='',main=paste('Recruitment',sep=', '),ylim=c(0,max(y)))
    if (splitLine) abline(v=stq_year,lty=2, col='red')
  }  
  
  if ("Fbar" %in% ptype) {
    plot(a$Year,a$Fbar,type='b',lwd=3,xlab='',ylab=plotLabels['Fbar'],main="Fishing mortality (F)",ylim=c(0,max(a$Fbar)))
    if (splitLine) abline(v=SMS@last.year.model,lty=2, col='red')
    if (incl.reference.points) {
      if (refPoints[species,'Flim']>0) abline(h=refPoints[species,'Flim'],lty=2,lwd=2,col='red')
      if (refPoints[species,'Fpa']>0) abline(h=refPoints[species,'Fpa'],lty=3,lwd=2,col='green')
    }
  } 
  
  if ("Yield" %in% ptype) {
    y<-tapply(a$Yield,list(a$Year),sum)
    barplot(y,space=1,ylab=plotLabels['Yield'],xlab='',main='Catch',ylim=c(0,max(y)))
    if (splitLine) abline(v=stq_year,lty=2, col='red')
  }  
  
  if ("Dead" %in% ptype) {
    Yield<-tapply(a$Yield,list(a$Year),sum)
    deadM1<-tapply(a$DeadM1,list(a$Year),sum)
    deadM2<-tapply(a$DeadM2,list(a$Year),sum)
    barplot(rbind(Yield,deadM1,deadM2),space=1,main='Biomass removed\ndue to F, M1 and M2',ylab=plotLabels['DeadM'],
            col=c('red','green','plum'))
    
    if (splitLine) abline(v=stq_year,lty=2, col='red')
  }  
  
  if ("M2" %in% ptype) if (any(b$M2>0)) {
    M2<-tapply(b$M2,list(b$Year,b$Age),sum)
    y<-as.numeric(dimnames(M2)[[1]])
    plot(y,M2[,1],main=paste("M2 at age"),xlab="",ylab=plotLabels['M2'],
         type='l',lwd=1.5,ylim=c(0,max(M2,na.rm=T)))
    for (a in (2:(dim(M2)[2]))) if(max(M2[,a],na.rm=T)>0.001) lines(y,M2[,a],lty=a,col=a,lwd=2)
    if (splitLine) abline(v=SMS@last.year.model,lty=2, col='red')
  }
}


plot_who_eats<-function(x,pred,prey,predPrey='by prey',years=c(0,5000),exclHumans=TRUE){
  
  x<-bind_rows(histEaten,x)
  
  x<-filter(x,Year>=years[1] & Year<=years[2])
  if (exclHumans)  x<-filter(x,Predator !='Humans')
  
  if (prey != 'all preys') {
    tit<-paste(prey, "eaten by"); 
    x<-filter(x,Prey==prey) 
  } else tit<-'All preys eaten by'
  
  if (pred !='all predators') {
    x<-filter(x,Predator==pred); 
    tit<-paste(tit,pred)
  } else tit<-paste(tit,'all predators')
  
  scale_fill_pp <- function(...){
    ggplot2:::manual_scale(
      'fill', 
      values = setNames(my.colors, predPreyFormat), 
      ...
    )
  }
  
  if (predPrey=='by prey') {
    x <-aggregate(eatenW~Year+Prey,data=x,sum)
    p<-ggplot(x, aes(x = Year, y = eatenW, fill = Prey))  
    
  } else   if (predPrey=='by predator') {
    x <-aggregate(eatenW~Year+Predator,data=x,sum)
    p<-ggplot(x, aes(x = Year, y = eatenW, fill = Predator)) 
  }
  p +  geom_bar(stat = "identity") + 
    scale_fill_pp() +
    labs(x="", y = paste("Biomass eaten",plotLabels['DeadM'])) +
    ggtitle(tit) +
    theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold")) 
}  



# Radar plot function
plot_one<-function(x,type='Yield',plot.legend = TRUE) {
  a1 <- filter(x$out$b,variable==type)
  a1[,2:(n.fleet+1)]<- a1[,2:(n.fleet+1)]/x$baseLine[,type]
  
  a2<- a1
  a2[,2:(n.fleet+1)]<- 1
  a<-bind_rows(a1,a2)
  tit<-ifelse(type=='Fbar'," F",paste0(' ',type))
  a[1,'variable']<-tit
  a[2,'variable']<-'Baseline'
  gmax<-max( a[,2:(n.fleet+1)])
  
  DTU.col<-c("#990000",  # red
             "#F6D04D")  # yellow
  
  ggradar(a,grid.max=gmax,grid.min=0,plot.title='',plot.legend=plot.legend,legend.position='top',
          group.colours =DTU.col,legend.text.size = 18)
}

plot_radar_all<-function(res){
  pF<-plot_one(res,type='Fbar',plot.legend = TRUE)
  pYield<-plot_one(res,type='Yield')
  pRec<-plot_one(res,type='Recruits')
  pSSB<-plot_one(res,type='SSB')
  plot_grid(pF, pYield, pRec,pSSB, 
            ncol = 2, nrow = 2)
}
