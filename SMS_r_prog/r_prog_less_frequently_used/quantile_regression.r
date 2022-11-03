library(quantreg)
q1<-0.025 #lower quantile
q2<-0.975 #higher quantile
first.qr.spcies.no<-1  # first predator species where qr is done  pred(9)=R. radiata

stom<-Read.stomach.data()


cleanup()
dev<-'screen'
dev<-'png'
nox<-2; noy<-2;
newplot(dev,filename='Quantile-regressesion',nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0 &stomcon>1E-4 & stom.used.like==1 & Predator.no>=first.qr.spcies.no)
if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Prey.no,stom2$Predator),function(x) {
 if (min(x$Predator.size) < max(x$Predator.size)) { 
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
    cat(paste(x[1,]$Predator," eating ",x[1,]$Prey,'\n'))
   plot(log(x$Predator.size),log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(predator size)',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio))
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~log(Predator.size) ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3,lwd=2)
   
   #quantile regression
   for (tau in c(q1,q2)) {
     ru<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=tau)
     abline(ru,col='blue',lty=2)
   }
   # weighted regression
   #aa<-lm(log(Predator.size/Prey.size)~log(Predator.size),weights=stomcon ,data=x)
   #ac<-aa[["coefficients"]]
   #abline(aa, lty=2,col=2)

   
   title(main=paste(x[1,]$Predator," eating ",x[1,]$Prey,
     " a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),sep=""))
   i<<-i+1
 } else  cat(paste('No regression done for ',x[1,]$Predator," eating ",x[1,]$Prey,'\n'))
})}

if (dev=='png') cleanup()
stom<-Read.stomach.data()
stom<-subset(stom,Size.model>0 & Prey.no !=0 &stomcon>1E-4 & stom.used.like==1 & Predator.no>=first.qr.spcies.no)

qunatileRegression<-function(tau=0.05){

  q<-by(stom,list(stom$Predator.no,stom$Prey.no),function(x) {
  
   x<-subset(x,Predator.size> Prey.size)

  #cat(x[1,'Predator'],'\n')
   rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=tau)
  })
  dimq<-dimnames(q)
  npred<-max(as.numeric(dimq[[1]]))
  minprey<-min(as.numeric(dimq[[2]]))
  maxprey<-max(as.numeric(dimq[[2]]))
  
  a<-matrix(-99,ncol=maxprey-minprey+1,nrow=npred)
  dimnames(a)[2]<-list(as.character(seq(minprey,maxprey)))
  dimnames(a)[1]<-list(as.character(seq(1,npred)))
  b<-a
  
  for (i in dimq[[1]]) for (j in dimq[[2]]) if (!is.null(q[[i,j]])) {
   a[i,j]<-q[[i,j]][["coefficients"]][1]
   b[i,j]<-q[[i,j]][["coefficients"]][2]
  }
  list(a,b)
}

a.lower<-qunatileRegression(tau=q1)
a.higher<-qunatileRegression(tau=q2)

if (T) {
  out<-file.path(data.path,"pred_prey_size_range_param_draft.in")
  cat("# Quantile regression parameters for predator/prey size ratio as a function of predator size\n",file=out)
  cat("# size select.model used for parameter estimate =",SMS.control@size.select.model,'\n',file=out,append=TRUE)
  #cat("# ",SMS.control@species.names,"\n",file=out,append=TRUE)
  cat("# intercept values for",q1*100,"% Quantile \n",file=out,append=TRUE)
  
  write.table(format(round(a.lower[[1]],4),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
  cat("# slope values for",q1*100,"% Quantile \n",file=out,append=TRUE)
  write.table(format(round(a.lower[[2]],4),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
  
  cat("# intercept values for",q2*100,"% Quantile \n",file=out,append=TRUE)
  write.table(format(round(a.higher[[1]],4),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
  cat("# slope values for",q2*100,"% Quantile \n",file=out,append=TRUE)
  write.table(format(round(a.higher[[2]],4),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
  cat("-999  #checksum\n" ,file=out,append=TRUE )
}


##############################################################
# plot pred prey size realtions for all combinations of predators and preys
#  and overlay with "size band" estimated above

Init.function()
sizeModel<-SMS.control@size.select.model 
 
dat<-Read.summary.data()
dat<-subset(dat,Year==SMS.control@last.year.model,select=c(Species, Year, Quarter, Species.n, Age, west, Lsea))

if (sizeModel==1) dat$size<-dat$Lsea
if (sizeModel==2 | sizeModel==3) dat$size<-dat$west
if (sizeModel==4) {
  lw<-Read.length.weight.relation()
  dat<-merge(dat,lw)
  dat$size<-dat$a*dat$Lsea^dat$b
}

dat<-subset(dat,select=c(Species.n,Quarter,Age,size))

pred<-subset(dat,Species.n<=npr)
pred$Pred.no<-pred$Species.n
pred$predSize<-pred$size
pred$predAge<-pred$Age
pred<-subset(pred,select=c(-Species.n,-size, -Age))


prey<-subset(dat,Species.n>=first.VPA)
prey$Prey.no<-prey$Species.n
prey$preySize<-prey$size
prey$preyAge<-prey$Age
prey<-subset(prey,select=c(-Species.n,-size, -Age))

a<-merge(pred,prey)

vul<-subset(Read.vulnerability(),select=c(Pred.no,Prey.no))
a<-merge(a,vul)
#a<-subset(a,Pred.no>=first.VPA)

a$ratio<-SMS.control@prey.pred.size.fac[a$Pred.no] 
a$color<-'red'
a[a$preySize<=a$predSize*a$ratio,]$color<-'blue'

cleanup()
i<-0
by(a,list(a$Prey.no,a$Pred.no),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   cat(paste(x[1,]$Pred.no," eating ",x[1,]$Prey.no,'\n'))
   X11()
   plot(log(x$predSize),log(x$predSize/x$preySize),main=NULL,xlab='log(predator size)',col=x$color,
           ylab="log(pred.size/prey.size)",ylim=c(min.ratio,max.ratio))
   
   abline(a=a.lower[[1]][as.character(x[1,]$Pred.no),as.character(x[1,]$Prey.no)],
          b=a.lower[[2]][as.character(x[1,]$Pred.no),as.character(x[1,]$Prey.no)],lty=3,col='red')
   abline(a=a.higher[[1]][as.character(x[1,]$Pred.no),as.character(x[1,]$Prey.no)],
          b=a.higher[[2]][as.character(x[1,]$Pred.no),as.character(x[1,]$Prey.no)],lty=3,col='red')
      
   title(paste(sp.names[x[1,]$Pred.no]," eating ",sp.names[x[1,]$Prey.no],'\n'))
   i<<-i+1
})    


##############################################################
#observation of predator-prey size ratio against predator size by predator
## prog size 2b

cleanup()
nox<-2; noy<-2;
dev<-'sceen'
newplot(dev,nox,noy);
i<-0

stom2<-subset(stom,Size.model>0 & Prey.no !=0 &stomcon>1E-4)

if (dim(stom2)[1]==0) stop("No data, probably because there is no size selection applied in the model") else {
min.ratio<-log(min(stom2$Predator.size/stom2$Prey.size))
max.ratio<-log(max(stom2$Predator.size/stom2$Prey.size))

by(stom2,list(stom2$Prey.no,stom2$Predator.no),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   par(mar=c(4,4,8,3))   #c(bottom, left, top, right)
   plot(log(x$Predator.size),log(x$Predator.size/x$Prey.size),main=NULL,xlab='log(predator size)',ylab="log(pred.size/prey.size)",
             ylim=c(min.ratio,max.ratio))
   abline(h=0, lty=3)
   aa<-lm(log(Predator.size/Prey.size)~log(Predator.size) ,data=x)
   ac<-aa[["coefficients"]]
   abline(aa, lty=3,lwd=2)
   
   #quantile regression

   rl<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=q1)
   abline(rl,col='blue',lty=2)
   ru<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=q2)
   abline(ru,col='blue',lty=2)
   rmm<-rq(log(Predator.size/Prey.size)~log(Predator.size) ,data=x, tau=0.5)
   abline(rmm,col='red',lty=2)
   
   # weighted regression
   #aa<-lm(log(Predator.size/Prey.size)~log(Predator.size),weights=stomcon ,data=x)
   #ac<-aa[["coefficients"]]
   #abline(aa, lty=2,col=2)

   
   title(main=paste(x[1,]$Predator," eating ",x[1,]$Prey, "\n",
     "mean:  a=",formatC(ac[1],2,format='f')," b=",formatC(ac[2],2,format='f'),"\n",
     "50%:   a=",formatC(rmm[["coefficients"]][1],2,format='f')," b=",formatC(rmm[["coefficients"]][2],2,format='f'),"\n",
     "lower: a=",formatC(rl[["coefficients"]][1],2,format='f')," b=",formatC(rl[["coefficients"]][2],2,format='f'),"\n",
     "upper: a=",formatC(ru[["coefficients"]][1],2,format='f')," b=",formatC(ru[["coefficients"]][2],2,format='f'),
     
     sep=""))
   i<<-i+1
})}

 ###########################
 # use only intercept values, to allow fixed size ratio in simulation runs
stom2<-subset(stom,Size.model>0 & Prey.no !=0 &stomcon>1E-4)
stom2$ratio<-log(stom2$Predator.size/stom2$Prey.size)

a<-tapply(stom2$ratio,list(stom2$Predator.no,stom2$Prey.no),min)
a[is.na(a)]<- -99    
tt<-matrix(rep(-99,6),ncol=1)      # saithe as prey
a<-cbind(a[,1:3],tt,a[,4:6])

out<-file.path(data.path,"pred_prey_size_range_param_fixed.in")
cat("# Quantile regression parameters for predator/prey size ratio as a function of predator size\n",file=out)
cat("# ",SMS.control@species.names,"\n",file=out,append=TRUE)
cat("# intercept values\n",file=out,append=TRUE)
write.table(format(round(a,4),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
cat("# slope values\n",file=out,append=TRUE)
a[,]<-0

write.table(format(round(a,0),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)


a<-tapply(stom2$ratio,list(stom2$Predator.no,stom2$Prey.no),max)
a[is.na(a)]<- -99
tt<-matrix(rep(-99,6),ncol=1)      # saithe as prey
a<-cbind(a[,1:3],tt,a[,4:6])

cat("# Quantile regression parameters for predator/prey size ratio as a function of predator size\n",file=out,append=T)
cat("# ",SMS.control@species.names,"\n",file=out,append=TRUE)
cat("# intercept values\n",file=out,append=TRUE)
write.table(format(round(a,4),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
cat("# slope values\n",file=out,append=TRUE)
a[,]<-0
write.table(format(round(a,0),width=11),file=out,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)

