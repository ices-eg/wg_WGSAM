load(file =file.path(scenario.dir, "a.RData"))  # SKULLE IKKE VÆRE NØDVENDIGT ??
       
cleanup()
plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=8, height=8, pointsize=12)
  if (dev=='wmf') win.metafile(filename = file.path(scenario.dir,paste(out,'.wmf',sep='')), width=8, height=8, pointsize=12)
  if (dev=='png') png(filename =file.path(scenario.dir,paste(out,'.png',sep='')), width = 1200, height = 1200,units = "px", pointsize = 25, bg = "white")
  if (dev=='pdf') pdf(file =file.path(scenario.dir,paste(out,'.pdf',sep='')), width = 8, height = 8,pointsize = 12,onefile=FALSE)
}

plotfile(dev=my.dev,out='abox-yield')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  x$y<-x$yield/1000
  ylab<-paste(sp.names[x[1,'Species.n']], "Yield (1000 t)")
  #x$y<-x$SSB/1000
  #ylab<-paste(sp.names[x[1,'Species.n']], "SSB (1000 t)")

  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-discard')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  x$y<-(x$CWsum-x$yield)/x$CWsum*100
  ylab<-paste(sp.names[x[1,'Species.n']], " Discard (Weight %)")
  #x$y<-x$SSB/1000
  #ylab<-paste(sp.names[x[1,'Species.n']], "SSB (1000 t)")

  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()

plotfile(dev=my.dev,out='abox-SSB')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  x$y<-x$SSB/1000
  ylab<-paste(sp.names[x[1,'Species.n']], "SSB (1000 t)")

  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-recruit')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  maxRec<-max(x$rec)
  if (maxRec<1E3) {  ylab<-paste(sp.names[x[1,'Species.n']], "Recruit (thousands)"); x$y<-x$rec }
  else if (maxRec<1E6) {  ylab<-paste(sp.names[x[1,'Species.n']], "Recruit (millions)"); x$y<-x$rec/1000 }
  else if (maxRec<1E9) {  ylab<-paste(sp.names[x[1,'Species.n']], "Recruit (billions)"); x$y<-x$rec/1000000 }

  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()



plotfile(dev=my.dev,out='abox-Fbar')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  maxF<-max(x$Fbar)
  ylab<-paste(sp.names[x[1,'Species.n']], "Realized F"); 
  x$y<-x$Fbar 
 
  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-riskBlim')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right
tmp<-by(a,list(a$Species.n),function(x) {

  x$y<-x$belowBlim/years.in.average*100;
  ylab<-paste(sp.names[x[1,'Species.n']], "Risk to Blim (%)"); 
 
  boxplot(y~COD,data=x,xlab='F Cod',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~HER,data=x,xlab='F Herring',ylab=ylab,main=sp.names[x[1,'Species.n']])
  boxplot(y~SPR,data=x,xlab='F Sprat',ylab=ylab,main=sp.names[x[1,'Species.n']])
})

if (my.dev %in% c('png','wmf','pdf'))  dev.off()



if (F) {
  # find optimum
  sel.year<-2040:HCR@last.prediction.year
  a<-subset(detailed, Year %in% (sel.year) & COD>0,select=c(-Repetion, -Iteration,-iter) )
  a<-aggregate(yield~Species.n+COD+HER+SPR,mean,data=a)

  yield.weighting<-c(10, 1.5, 1.0)
  b<-data.frame(a,val=a$yield*yield.weighting[a$Species.n])
  b<-aggregate(val~COD+HER+SPR,sum,data=b)
  b<-b[order(b$val,decreasing=T),]
  head(b,10)

  if (F) { # compare with a SMS-OP run
    g<-Read.OP.condensed()
    g<-subset(g,Year >=2040)
    g<-data.frame(g,val=g$yield*yield.weighting[g$Species.n])
    g<-aggregate(val~Species.n,mean,data=g)
    g
    sum(g$val)
  }
}


a$Species<-sp.names[a$Species.n]
a$Fround<- -1
a[a$Species.n==1,"Fround"]<-a[a$Species.n==1,"COD"]
a[a$Species.n==2,"Fround"]<-a[a$Species.n==2,"HER"]
a[a$Species.n==3,"Fround"]<-a[a$Species.n==3,"SPR"]

a$COD1<-formatC(a$COD,digits = 3, width = 4, format = "f")
a$HER1<-formatC(a$HER,digits = 3, width = 4, format = "f")
a$SPR1<-formatC(a$SPR,digits = 3, width = 4, format = "f")



table.MSY<-function(a,out.file='a'){
  aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  sink(file.path(scenario.dir,paste(out.file,'_','MSY','.out',sep='')))
  cat('Median MSY (1000 tonnes)\n')
  print(round(aa/1000,1))
  cat('\n\n')
  cat('CV of yield\n')
  aa2<-tapply(a$yield,list(a$Species.n,a$Fround),sd)/tapply(a$yield,list(a$Species.n,a$Fround),mean)
  dimnames(aa2)[[1]]<-dimnames(aa)[[1]]
  print(round(aa2,2))

  MSY<-apply(aa,1,max,na.rm=T)
  ab<-aa/rep(MSY,times=dim(aa)[2])
  cat('\nYield relative to observed MSY\n')
  print(round(ab,2))
  cat('\n')

  aa<-tapply(a$SSB,list(a$Species.n,a$Fround),median)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Median SSB (1000 tonnes)\n')
  print(round(aa/1000,0))


  sink()

  print(round(ab,2))
}

table.MSY(a,out.file='MSY-summary')

