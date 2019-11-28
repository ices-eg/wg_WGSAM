cleanup()

#w<-Read.summary.data() #historical
#w<-Read.mean.weights() # forecast
w<-Read.mean.weights.OP(dir=file.path(data.path,scenario))    # OP forecast
#w<-Read.mean.weights.OP()    # OP forecast

Clupeids<-c(2,3)    # herring and sprat

w2<-droplevels(subset(w,west>0 & Quarter==1 & Species.n %in% Clupeids))
head(w2)
clupeid_N<-aggregate(N~Year,data=w2,sum)

trellis.device(height=10,width=8,pointsize=8)
print(xyplot(N/1000000~Year ,data=clupeid_N,ylab="Clupeid abundance (10^9)",type='b'))


w2<-droplevels(subset(w,west>0 & (Quarter==1 & Age<=9) |(Quarter==3 & Age==0) ))
by(w2,w2$Species.n,function(dat) {
  trellis.device(height=10,width=8,pointsize=8)
  tit<-paste(dat[1,'Species'],"Q1")
  ylab<-'Weight in the stock (kg)'
  maxW<-max(dat$west)
  if (maxW <1) {
   ylab<-'Weight in the stock (gram)'
   dat$west<-dat$west*1000
  }
  print(xyplot(west~Year| paste("Age:",Age," Q",Quarter,sep='') ,data=dat,layout = c(3, 3),
    scales = list(y="free"),
    main=tit, ylab=ylab,
     panel=function(x,y) {
      panel.xyplot(x,y,col=1 ,pch=1,type='l')
      #panel.loess(x,y, span=1)
      #panel.lmline(x,y,col=2)
    }
  ))
})

w2<-droplevels(subset(w,west>0 & Age<9 ))
w2$Age<-w2$Age+(w2$Quarter-1)*0.25
w2<-subset(w2,select=c(west,Age,Year,Species.n,Species))
w2<-w2[order(w2$Species.n,w2$Year,w2$Age),]
#cleanup()
trellis.device(height=10,width=8,pointsize=8)
print(xyplot(1000*west~Age|paste(Species),data=w2,layout = c(1, 3),
  scales = list(y="free"),group=Year, type='l',
  ylab='Weight in the stock (g)'
))


maxY<-max(w$Year)
minY<-min(w$Year)
w2<-droplevels(subset(w,west>0 & Age<9 & (Year==maxY | Year==minY) ))

w2$Age<-w2$Age+(w2$Quarter-1)*0.25
w2<-subset(w2,select=c(west,Age,Year,Species.n,Species))
w2<-w2[order(w2$Species.n,w2$Year,w2$Age),]
#cleanup()
trellis.device(height=10,width=8,pointsize=8)
print(xyplot(1000*west~Age|paste(Species),data=w2,layout = c(1, 3),
  scales = list(y="free"),group=Year, type='l',
  ylab='Weight in the stock (g)',main='Weights in first and last year'
))
