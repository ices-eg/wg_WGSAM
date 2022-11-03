
make.template<-function(sp,tit='Yield',my.dev='png') {
  close.screen(all = TRUE)
  p<-0.05
  n<-length(sp)
  des<-matrix(c(
          p,1,1-p,1,   # 1 Yield title
          0,0.1,0,1-p, # 2 F title
          p,1,0,1-p)    # 3 figures
          , nrow=3, ncol=4, byrow=TRUE)
  split.screen(des)     # split display into three screens
  split.screen(c(2,1), screen = 1) # top split yield title in two lines, screen 4 & 5
  split.screen(c(1,n), screen = 5) # top split yield  screen 5 into n screens e.g. 6:13
  split.screen(c(n,1), screen = 2) # split into n.species rows

  split.screen(c(n,n), screen = 3) # Split plot area into n x n graphs

  screen(4); par(mar=c(0,0,1,0));  mtext(tit,3)
  for (i in (1:n)) { screen(i+5);par(mar=c(0,0,1.2,0)); mtext(paste(sp[i],'    '),3,cex=0.8,adj=1) }
  if (my.dev!='png') for (i in (1:n)) { screen(i+n+5); par(mar=c(1,0,0,0));mtext(paste(sp[i],'F:    '),side=1,cex=0.8) } else {
     for (i in (1:n)) { screen(i+n+5); par(mar=c(1,0,0,0));mtext(paste(" ",substr(sp[i],1,3),'F:'),side=1,adj = 0,cex=0.8) }
  }
}

new.screen<-function(coln=1,rown=1,first.sc=1,n.species=1) {
  sc<-first.sc-1+(rown-1)*n.species+coln
  # cat('column:',coln,' row:',rown,' screen:',sc,'\n')
  screen(sc)
  par(mar=c(2,2.5,1.0,0.2))    #bottom, left, top, right
  par(cex.axis=0.75)
  #sc<<-sc+1
}


plotfile<-function(dev='screen',out,h.screen=8,w.screen=12,h.wmf=8,w.wmf=12,h.png=1400,w.png=1200,h.pdf=12,w.pdf=8,scenario.dir='') {
  cleanup()
  #dev<-'screen'  ######################
  
  if (dev=='screen') X11(width=8, height=h.screen, pointsize=w.screen)
  if (dev=='wmf') win.metafile(filename = file.path(scenario.dir,paste(out,'.wmf',sep='')), width=w.wmf, height=h.wmf, pointsize=12)
  if (dev=='png') png(filename =file.path(scenario.dir,paste(out,'.png',sep='')), width = w.png, height = h.png,units = "px", pointsize = 18, bg="white")
  if (dev=='pdf') pdf(file =file.path(scenario.dir,paste(out,'.pdf',sep='')),width =w.pdf, height = h.pdf, pointsize = 12)
}


plot.matrix<-function(tit='Yield',type=c('Yield','SSB','Recruit','Fbar')[1],first.sp='COD',spNames=sp.names[16:25],my.dev='png',out.file='a',add.to.file.name='',plot.ref=F,scenario.dir='a',a=a) {

  cat('plotting ',tit,'\n')
 
  plotfile(dev=my.dev,out=paste(out.file,type,add.to.file.name,ifelse(plot.ref,'1','0'),sep='_'),scenario.dir=scenario.dir)
  make.template(sp=spNames,tit=tit,my.dev=my.dev)
  
  n.species<-length(spNames)
  #cat("n.species:",n.species,"\n")
  column<- 0
  SP.codes<-dimnames(a)[[2]][grep(first.sp,dimnames(a)[[2]])[1]:(grep(first.sp,dimnames(a)[[2]])[1]+nsp.VPA-1-2)]
  
  dummy<-by(a,list(a$Species.n),function(x) {
    column<<- column+1
    sp<-unique(x$Species)
    #cat(sp,column,'\n')

    ylab<-'' ;xlab<-''
    if (type=='Yield') x$y<-x$yield/1000 else if (type=='SSB') x$y<-x$SSB/1000  else if (type=='Recruit')  x$y<-x$recruit/1000000  else if (type=='Fbar') x$y<-x$Fbar else if (type=='belowBlim')  x$y<-x$probBelowBlim*100  else if (type=='belowBpa')  x$y<-x$probBelowBpa*100  else if (type=='riskBlim') x$y<-x$riskBlim*100       
    ref.lines<-function(sp){
       if (!plot.ref) return(NULL)
       if (type=='Yield') return(NULL)
       if (type=='Recruit') return(NULL)
       if (type=='Fbar') return(NULL)
       ref<-Read.reference.points.OP(dir=data.path) 
       
       abline(h=ref[sp-first.VPA+1,'Bpa']/1000,col='blue',lwd=2)
       abline(h=ref[sp-first.VPA+1,'Blim']/1000,col='red',lwd=2)
    }
    ii<-1
    for (i in SP.codes) {
       new.screen(coln=column,rown=ii,first.sc=6+n.species*2,n.species=n.species)
       boxplot(as.formula(paste('y~',i,sep=' ')),data=x,show.names = T,xlab=ylab,ylab=ylab)
       ref.lines(sp)
       ii<-ii+1
    }
   
  })
  close.screen(all = TRUE)    # exit split-screen mode
  if (my.dev %in% c('png','wmf','pdf')) dev.off()
}
 #########  END  functions ###################################################

