
#my.data.path<-file.path(data.path,"HCR_1_deter_noadjust_narrow_01_HCR1_0_Rec0__2051")
my.data.path<-file.path(data.path,"HCR_1_deter_adjust_narrow_01_HCR1_0_Rec0_Recadj_2051")

 # prøv også at sammenlign med sum af yield/MSY per species
 
load( file =file.path(my.data.path, "a.RData"))
head(a)

b<-aggregate( cbind(value,yield,belowBlim)   ~run+iteration+COD+WHG+HAD+POK+HER+SAN+NOR+SPR+PLE+SOL ,data=a,sum)

head(b)
b[b$belowBlim>0,'belowBlim']<- 1

if (max(b$iteration) >1)   b<-aggregate( cbind(value,yield,belowBlim)   ~run+COD+WHG+HAD+POK+HER+SAN+NOR+SPR+PLE+SOL ,data=b,mean)
dim(b)
b<-droplevels(subset(b,belowBlim==0))
dim(b)

sord<-order(b$yield,decreasing = T)

b<-b[sord,]
bsum<-summary(b)

dim(b)
gemb<-b
b<-gemb

# pruning factor
pruneF<-2000

b<-b[1:dim(b)[[1]] %% pruneF ==0,]

#first try

maxN<-5000
bb<-head(b,maxN)


cleanup()
bb$i<-1:(dim(bb)[[1]])
plot(bb$i,bb$yield/1E6)
points( bb$i,bb$value/1E6/10 )

aa<-bb

newplot(dev='screen',nox=2,noy=4,Portrait=T)
par(mar=c(3,4,3,2))  #bottom, left, top, right

lim<-rr  # from batch run

plot(aa$i,aa$COD,xlab='Combinations',ylim=c(lim['Cod',1],lim['Cod',2]),cex=0.5)
plot(aa$i,aa$WHG,xlab='Combinations',ylim=c(lim['Whiting',1],lim['Whiting',2]),cex=2)
plot(aa$i,aa$HAD,xlab='Combinations',ylim=c(lim['Haddock',1],lim['Haddock',2]))
plot(aa$i,aa$POK,xlab='Combinations',ylim=c(lim['Saithe',1],lim['Saithe',2]))
plot(aa$i,aa$HER,xlab='Combinations',ylim=c(lim['Herring',1],lim['Herring',2]))
plot(aa$i,aa$SAN,xlab='Combinations',ylim=c(lim['Sandeel',1],lim['Sandeel',2]))
plot(aa$i,aa$NOR,xlab='Combinations',ylim=c(lim['Nor. pout',1],lim['Nor. pout',2]))
plot(aa$i,aa$SPR,xlab='Combinations',ylim=c(lim['Sprat',1],lim['Sprat',2]))


# second try

maxN<-50      # not more than 1000
bb<-head(b,maxN)


aa<-NULL
for (i in (1:maxN)) {
  aa<-rbind(data.frame(i=i,head(bb,i)),aa)
}
head(aa)


cleanup()
bb$i<-1:(dim(bb)[[1]])
plot(bb$i,bb$yield/1E6)

newplot(dev='screen',nox=2,noy=4,Portrait=T)

plot(aa$i,aa$COD,xlab='Combinations',ylim=c(0.3,0.7))
plot(aa$i,aa$WHG,xlab='Combinations')
plot(aa$i,aa$HAD,xlab='Combinations')
plot(aa$i,aa$POK,xlab='Combinations')
plot(aa$i,aa$HER,xlab='Combinations')
plot(aa$i,aa$SAN,xlab='Combinations')
plot(aa$i,aa$NOR,xlab='Combinations')
plot(aa$i,aa$SPR,xlab='Combinations')

