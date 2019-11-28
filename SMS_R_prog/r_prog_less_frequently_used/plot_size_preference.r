# size preference

dev<-"screen"
nox<-1; noy<-1;
noxy<-nox*noy
par(ask=TRUE)
i<-0

##############################################################

#size preference
cleanup() 
newplot(dev,nox,noy);


file<-file.path(data.path,'size_pref.out') 
size<-read.table(file,comment.char = "#",header=T)

Plot.size.pref.all<-function(logval=T) {
    n<-dim(size)[1]
    size.ratio<-size[ ,"size.ratio"]
    size.var<-size[,"size.var"]

    file<-file.path(data.path,'min_max_size_pref.out')
    r<-matrix(scan(file,comment.char = "#"),ncol=2,byrow=F)
    r<-log(r)
    min_l<-round(min(r[1,]),2)
    max_l<-round(max(r[,2]),2)
    x<-seq(min_l,max_l, by=0.01)
    len.x<-length(x)
    y<-array(0,c(n,len.x))

    for (pred in (1:n)) if ((size[pred,"size.model"]==1) || (size[pred,"size.model"]==11) || (size[pred,"size.model"]==2)) {
      y[pred,]<-exp(-(x-size.ratio[pred])^2/(2.0*rep(size.var[pred],len.x)))
    }
    xlab<-'log(pred size/prey size)'
    if (logval==F) {
      x<-exp(x);
      xlab<-'pred size/prey size'
    }
    xx<-c(min(x),max(x))
    yy<-c(min(y),max(y))
    if (n>1) {
      plot(xx,yy,type='n',xlab=xlab,ylab='Preference')
          legend(x=0.2,y=0.9,sp.names[1:(n)],fill=(1:n))
      for (pred in (1:n)) if ((size[pred,"size.model"]==1) ||  (size[pred,"size.model"]==11)||(size[pred,"size.model"]==2)) {
        lines(x,y[pred,],col=pred)
        abline(v=size.ratio[pred],col=pred)
      }
    } else if ((size[1,"size.model"]==1) ||  (size[1,"size.model"]==11)||(size[1,"size.model"]==2)){
       plot(x,y[1,],xlab=xlab,ylab='Preference',col=1,type='l')
        abline(v=size.ratio[1],col=1)
    }
}

Plot.size.pref.all(logval=T)

##################################################################
# with special predator length dependent correction

stom<-Read.stomach.data()
min_pred<-tapply(stom$Predator.size,list(stom$Predator.n),min)
max_pred<-tapply(stom$Predator.size,list(stom$Predator.n),max)

cleanup()
newplot(dev,nox,noy);

file<-file.path(data.path,'size_pref.out')
size<-read.table(file,comment.char = "#",header=T)


Plot.size.pref.all<-function(logval=T) {
    if (SMS.control@size.select.model==1) nround<-0 else nround<-3;
    n<-dim(size)[1]
    size.ratio<-size[ ,"size.ratio"]
    size.var<-size[,"size.var"]
    pref.size.ratio.correction<-size[,"pref.size.ratio.correction"]

    file<-file.path(data.path,'min_max_size_pref.out')
    r<-matrix(scan(file,comment.char = "#"),ncol=2,byrow=F)
    r<-log(r)
    min_l<-min(r[1,])   # min pred/prey ratio
    max_l<-max(r[,2])   # max pred/prey ratio

    for (pred in (1:n)) if ((size[pred,"size.model"]==1) || (size[pred,"size.model"]==11) || (size[pred,"size.model"]==2)) {
       x<-seq(min_l,max_l, by=0.1)
      len.x<-length(x)

      l_step<-seq(min_pred,max_pred,(max_pred[pred]-min_pred[pred])/6)
      i<-0
      y<-array(0,c(length(l_step),len.x))
      for  (pred_l in (l_step)) {
        i<-i+1
        y[i,]<-exp(-((x-size.ratio[pred]-pref.size.ratio.correction[pred]*log(pred_l))^2/(2.0*rep(size.var[pred],len.x))))
      }
     }
    xlab<-'log(pred size/prey size)'

     if (logval==F) {
         x<-exp(x);
         xlab<-'pred size/prey size'
     }
     plot(x,y[1,],xlab=xlab,ylab='Preference',col=1,type='l',lwd=2)
     text(x[len.x],y[1,len.x],round(min_pred,nround))
     for (i in (2:length(l_step))) {
       lines(x,y[i,],col=i,lwd=2)
       text(x[len.x],y[i,len.x],round(l_step[i],nround))
     }
}

Plot.size.pref.all(logval=T)


####################################################################################

Plot.size.pref_min_max<-function(log.scale,newPlot=T) {
    dev<-"screen"
    nox<-1; noy<-2;
    noxy<-nox*noy
    par(ask=TRUE)
    i<-0
    
    if (newPlot) newplot(dev,nox,noy);
        
    file<-file.path(data.path,'min_max_size_pref.out')
    size<-scan(file,comment.char = "#")
    
    ncol=nsp-first.VPA+1
    npr<-length(size)/ncol/2

    min.size<-matrix(data=size,ncol=ncol,nrow=npr,byrow=TRUE)
    max.size<-matrix(data=size[(1+length(min.size)):(2*length(min.size))],ncol=ncol,nrow=npr,byrow=TRUE)
    
    file<-file.path(data.path,'size_pref.out') 
    size<-read.table(file,comment.char = "#",header=T)  
    
    for (pred in (1:npr)) if (size[pred,"size.model"]==0 | TRUE) {
    xx<-c(min(min.size[pred,]),max(max.size[pred,]))
    xx.all<-c(min(min.size),max(max.size))
    if (log.scale==1) xx.all<-log(xx.all)
    if (log.scale==1) xx<-log(xx)
    yy<-c(1,nsp+1)
    if (i==noxy & newPlot) {newplot(dev,nox,noy); i<-0 }
    par(mar=c(5,5,3,2))

    plot(xx,yy,type='n',xlab=ifelse(log.scale==1,'log(pred size/prey size)','pred size/prey size'),
          ylab=' ',main=sp.names[pred], xlim=xx.all, axes=FALSE,ylim=c(first.VPA,nsp+1))
    axis(1)
    ax<-sp.names[(first.VPA+1):(nsp+1)]
    axis(2, (first.VPA+1):(nsp+1), sp.names[(first.VPA):(nsp)],las=1)
    box()  
    for (prey in (first.VPA:nsp)) {
        x<-c(min.size[pred,prey-first.VPA+1],max.size[pred,prey-first.VPA+1])
        if (x[2]>0) {
            if (log.scale==1) x<-log(x)
            y<-rep(prey+1,2)    
            lines(x,y,col=prey-first.VPA+1,lwd=4)
           # x<-rep(min.size[pred,prey-first.VPA+1],2)
           # if (log.scale==1) x<-log(x)
           # y<-c(prey+0.85,prey-first.VPA+1+1.15)
           # lines(x,y,col=prey-first.VPA+1,lwd=2)
           # x<-rep(max.size[pred,prey-first.VPA+1],2)
           # if (log.scale==1) x<-log(x)
           # lines(x,y,col=prey-first.VPA+1,lwd=2)       
        }
    }
    i<-i+1
    }
}
Plot.size.pref_min_max(1)  #log scale
Plot.size.pref_min_max(0,newPlot=F)  # no log scale
