
make.template<-function(sp,tit='Yield',my.dev='png',texts='a ',n.types=7) {
  # cat(texts,"\n")
  p<-0.05
  n<-length(sp)

  des<-matrix(c(
          p,1,1-p,1,   # 1 Yield title
          0,0.1,0,1-p, # 2 F title
          p,1,0,1-p)    # 3 figures
          , nrow=3, ncol=4, byrow=TRUE)
  split.screen(des)     # split display into three screens
  split.screen(c(2,1), screen = 1) # top split yield title in two lines, 
  split.screen(c(1,n), screen = 5) # top split yield  screen 5 into n screens 
  split.screen(c(n.types,1), screen = 2) # split into n.types rows

  split.screen(c(n.types,n), screen = 3) # Split plot area into n x n graphs
  texts<-sub('value', 'Value\n (bill.EUR)', texts)
  texts<-sub('SSB', 'SSB\n (mill. t)', texts)
  texts<-sub('mweca', 'W catch\n (kg)', texts)
  
  texts<-sub('yield', 'Yield\n (mill. t)', texts)
  texts<-sub('LFI', 'LFI\n (%)', texts)

  texts<-sub('M2', 'M2\n (%)', texts)
  texts<-sub('comm.life.expec', 'life\n expec.\n (years)', texts)
  texts<-sub('comm.Fall', 'F\n (%)', texts)

  screen(4); par(mar=c(0,0,1,0));  mtext(tit,3)
  for (i in (1:n)) { screen(i+5);par(mar=c(0,0,1.2,0)); mtext(paste(sp[i],'    '),3,cex=0.8,adj=1) }
  if (my.dev!='png') for (i in (1:length(texts))) { screen(i+n+5); par(mar=c(1,0,0,0));mtext(paste(" ",texts[i],"\n\n",sep=""),side=1,cex=0.8,adj=0) } else {
     for (i in (1:length(texts))) { screen(i+n+5); par(mar=c(1,0,0,0));mtext(paste(" ",texts[i],"\n\n\n\n",sep=''),side=1,adj=0,cex=0.8) }
  }
}

new.screen<-function(coln=1,rown=1,first.sc=1,n.species=1) {
  sc<-first.sc-1+(rown-1)*n.species+coln
  #cat('column:',coln,' row:',rown,' screen:',sc,' first screen:',first.sc,'\n')
  screen(sc)
  par(mar=c(2,2.5,1.0,0.2))    #bottom, left, top, right

  par(cex.axis=0.75)
  #sc<<-sc+1
}


plotfile<-function(dev='screen',out,h.screen=8,w.screen=12,h.wmf=8,w.wmf=12,h.png=1400,w.png=1200,h.pdf=12,w.pdf=8,scenario.dir=scenario.dir) {
  cleanup()
  if (dev=='screen') X11(width=8, height=h.screen, pointsize=w.screen)
  if (dev=='wmf') win.metafile(filename = file.path(scenario.dir,paste(out,'.wmf',sep='')), width=w.wmf, height=h.wmf, pointsize=12)
  if (dev=='png') png(filename =file.path(scenario.dir,paste(out,'.png',sep='')), width = w.png, height = h.png,units = "px", pointsize = 18, bg="white")
  if (dev=='pdf') pdf(file =file.path(scenario.dir,paste(out,'.pdf',sep='')),width =w.pdf, height = h.pdf, pointsize = 12)
}


plot.matrix<-function(tit='Yield',types=c('SSB','value','yield','comm.Fall','mweca','M2','LFI','comm.life.expec'),spNames=sp.names[17:24],my.dev='png',out.file='a',plot.ref=F,scenario.dir=scenario.dir,a=indi,first.sc=1) {
  cat("\nSystem_indicators:\n")
  plotfile(dev=my.dev,out=paste(out.file,"sys_indicator",sep='_'),scenario.dir=scenario.dir)
  make.template(spNames,tit=tit,my.dev=my.dev,texts=types, n.types=length(types))
    n.species<-length(spNames)
  rown<- 0
  first.sc<-23
  for (indicator in types)  {
  cat(indicator,'\n')
    rown<- rown+1

    ylab<-'' ;xlab<-''
    if (indicator=='LFI') a$y<-a$LFI*100 else if (indicator=='M2') a$y<-a$comm.M2*100 else if (indicator=='comm.life.expec') a$y<-a$comm.life.expec else if (indicator=='comm.Fall') a$y<-a$comm.Fall*100 else if (indicator=='SSB') a$y<-a$SSB/1E6 else if (indicator=='yield') a$y<-a$yield/1E6 else if (indicator=='value') a$y<-a$value/1E6  else if (indicator=='mweca') a$y<-a$mweca    
    

    x<-subset(a,select=c(COD,WHG ,HAD,POK,HER,NSA,SSA,NOR,SPR,y))
    #x<-subset(a,select=c(COD,WHG ,HAD,POK,HER,SAN,NOR,SPR,PLE,SOL,y))

    #print(summary(x))
    
    new.screen(coln=1,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~COD,data=x,show.names = T,xlab=ylab,ylab=ylab)

    new.screen(coln=2,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~WHG,data=x,show.names = T,xlab=ylab,ylab=ylab)

    new.screen(coln=3,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~HAD,data=x,show.names = T,xlab=ylab,ylab=ylab)

    new.screen(coln=4,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~POK,data=x,show.names = T,xlab=ylab,ylab=ylab)

    new.screen(coln=5,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~HER,data=x,show.names = T,xlab=ylab,ylab=ylab)

    new.screen(coln=6,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~NSA,data=x,show.names = T,xlab=ylab,ylab=ylab)
    
    new.screen(coln=7,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~SSA,data=x,show.names = T,xlab=ylab,ylab=ylab)
    
    new.screen(coln=8,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~NOR,data=x,show.names = T,xlab=ylab,ylab=ylab)

    new.screen(coln=9,rown=rown,first.sc=first.sc,n.species=n.species)
    boxplot(y~SPR,data=x,show.names = T,xlab=ylab,ylab=ylab)
  }
  close.screen(all = TRUE)    # exit split-screen mode
  if (my.dev %in% c('png','wmf','pdf')) dev.off()
}
 #########  END  functions ###################################################
 