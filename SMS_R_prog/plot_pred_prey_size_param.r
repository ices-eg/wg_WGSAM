
nprey<-nsp-first.VPA+1

a<-scan(file=file.path(data.path,"pred_prey_size_range_param.in"),comment.char = "#")
a<-matrix(head(a,-1),nrow=nprey)
a<-t(a)
a


b<- array(0,dim=c(4,npr,nprey))
 dimnames(b)<-list(c('intercept.low','slope.low','intercept.high','slope.high'),sp.names[1:npr],sp.names[first.VPA:(first.VPA+nprey-1)])

r<-1
for (i in (1:4)) {
    #print(paste( r,':',r+npr-1))
    b[i,,]<-a[r:(r+npr-1),]
    r<-1+(i)*npr
    #print(paste(i,r))
}

ftable(b)


    
b<-arr2df(b)

names(b)<-c('parm','pred','prey','value')

b1<-subset(b,parm=='intercept.low',select=-parm); b1$intercept<-b1$value; b1$value<-NULL
b2<-subset(b,parm=='slope.low',select=-parm); b2$slope<-b2$value; b2$value<-NULL
b12<-merge(b1,b2,by=c('pred','prey'))
b12$value<-'low'


b3<-subset(b,parm=='intercept.high',select=-parm); b3$intercept<-b3$value; b3$value<-NULL
b4<-subset(b,parm=='slope.high',select=-parm); b4$slope<-b4$value; b4$value<-NULL
b34<-merge(b3,b4,by=c('pred','prey'))
b34$value<-'high'

bb<-rbind(b12,b34)
bb<-droplevels(subset(bb,intercept!=-99 & slope!=-99))


bb1<-bb
bb2<-bb


stom<-Read.stomach.data()
stom<-subset(stom,stom.used.like==1,select=c(Predator,Prey,Predator.size,Prey.size))

stom$ratio<-stom$Predator.size/stom$Prey.size
m1<-tapply(stom$Predator.size,list(pred=stom$Predator,prey=stom$Prey),min)
m1<-arr2df(m1)
names(m1)<-c('pred','prey','x')
m1$x<-log(m1$x)

m2<-tapply(stom$Predator.size,list(pred=stom$Predator,prey=stom$Prey),max)
m2<-arr2df(m2)
names(m2)<-c('pred','prey','x')
m2$x<-log(m2$x)

bb1<-merge(bb1,m1)
bb1$y<-bb1$intercept+bb1$slope*bb1$x

bb2<-merge(bb2,m2)
bb2$y<-bb2$intercept+bb2$slope*bb2$x

bb<-rbind(bb1,bb2)
option<-data.frame(pred=sp.names[1:npr],option=SMS.control@size.selection)

bb<-merge(bb,option)

bbb<-subset(bb,option==4)
library(lattice)
print(xyplot(y~x|pred*prey,group=value,data=subset(bb,!is.na(x) & !is.na(y)),type='b',xlab='log(predator size)',ylab='log(predator size / prey size)'))




