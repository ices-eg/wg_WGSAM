
ex1<-'Dirichlet'
stom<-Read.stomach.data(dir=file.path(root,'ns_paper_05_size'))
a<-subset(stom,select=c(-L.N.bar,-Prey.avail,-Suit,-obj.contrib,-sum.p,-like.dir,-Predator.available.food,-Prey.avail))
a$stomcon.hat.dir<-a$stomcon.hat; a$stomcon.hat<-NULL
a$Stom.var.dir<-a$Stom.var ; a$Stom.var<-NULL
a$Residual.dir<-a$Residual; a$Residual<-NULL
dim(a)

ex2<-'log-normal'
stom<-Read.stomach.data(dir=file.path(root,'ns_paper_05_size_log_normal'))
b<-subset(stom,select=c(-L.N.bar,-Prey.avail,-Suit,-obj.contrib,-sum.p,-like.dir,-Predator.available.food,-Prey.avail))
b$stomcon.hat.log<-b$stomcon.hat; b$stomcon.hat<-NULL
b$Stom.var.log<-b$Stom.var ; b$Stom.var<-NULL
b$Residual.log<-b$Residual; b$Residual<-NULL
dim(b)

ab<-merge(a,b)
dim(ab)
ab<-subset(ab,stom.used.like==1,select=c(-stom.used.like, -stom.used.avail ,-N.haul))
dim(ab)



dev<-"print"
dev<-"screen"
nox<-4
noy<-3

a<-subset(ab,Year==1991 & Predator %in% c('Cod','Whiting','Haddock'))

##############################################################
#log observed and predicted stomach contents by Predator and prey

dir.col<-2
dir.pch<-3

cleanup()
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-log(x$stomcon)
   bb.dir<-log(x$stomcon.hat.dir)
   bb.log<-log(x$stomcon.hat.log)
   max.val<-max(aa,bb.log,bb.dir)
   min.val<-min(aa,bb.log,bb.dir)
   plot(bb.log,aa,xlab='log Expected stomach content',ylab='log observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val))
   if (var(bb.log)>0) abline(lm(aa~bb.log), lty=3)
   
   points(bb.dir,aa,col=dir.col,pch=dir.pch)
   if (var(bb.dir)>0) abline(lm(aa~bb.dir), lty=2,col=dir.col)

   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})


##############################################################
# observed and predicted stomach contents by Predator and prey

cleanup()
newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-(x$stomcon)
   bb.dir<-(x$stomcon.hat.dir)
   bb.log<-(x$stomcon.hat.log)
   max.val<-max(aa,bb.log,bb.dir)
   min.val<-min(aa,bb.log,bb.dir)
   plot(bb.log,aa,xlab='Expected stomach content',ylab='observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val))
   if (var(bb.log)>0) abline(lm(aa~bb.log), lty=3)

   points(bb.dir,aa,col=dir.col,pch=dir.pch)
   if (var(bb.dir)>0) abline(lm(aa~bb.dir), lty=2,col=dir.col)

   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})



##############################################################
# observed and predicted stomach contents by Predator and prey
# for very small observed

a<-subset(ab,Year==1991 & Predator %in% c('Cod','Whiting','Haddock') & stomcon<0.01)


cleanup()

newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-log(x$stomcon)
   bb.dir<-log(x$stomcon.hat.dir)
   bb.log<-log(x$stomcon.hat.log)
   max.val<-max(aa,bb.log,bb.dir)
   min.val<-min(aa,bb.log,bb.dir)
   plot(bb.log,aa,xlab='log Expected stomach content',ylab='log observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val))
   if (var(bb.log)>0) abline(lm(aa~bb.log), lty=3)

   points(bb.dir,aa,col=dir.col,pch=dir.pch)
   if (var(bb.dir)>0) abline(lm(aa~bb.dir), lty=2,col=dir.col)

   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})


cleanup()
newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   aa<-(x$stomcon)
   bb.dir<-(x$stomcon.hat.dir)
   bb.log<-(x$stomcon.hat.log)
   max.val<-max(aa,bb.log,bb.dir)
   min.val<-min(aa,bb.log,bb.dir)
   plot(bb.log,aa,xlab='Expected stomach content',ylab='observed',ylim=c(min.val,max.val),xlim=c(min.val,max.val))
   if (var(bb.log)>0) abline(lm(aa~bb.log), lty=3)

   points(bb.dir,aa,col=dir.col,pch=dir.pch)
   if (var(bb.dir)>0) abline(lm(aa~bb.dir), lty=2,col=dir.col)

   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})


##############################################################
# Stomach variance observations

a<-subset(ab,Year==1991 & Predator %in% c('Cod','Whiting','Haddock') )
cleanup()
nox<-3; noy<-3;
newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
    plot(x$Stom.var.log,x$Stom.var.dir,ylab=paste('Stom variance',ex1),xlab='log-normal')

   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})



##############################################################
# Stomach residuals, compare

a<-subset(ab,Year==1991 & Predator %in% c('Cod','Whiting','Haddock') )
cleanup()
newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
    plot(x$Residual.log,x$Residual.dir,ylab=paste('Residual',ex1),xlab=paste('Residual',ex2))

   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})


##############################################################
# Residuals by Predator, and expected stom content

cleanup()
newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$stomcon.hat.log,x$Residual.log,xlab='Expected relative stomach content',ylab=paste('Residual',ex2))
   abline(h=0, lty=3)
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})


cleanup()
newplot(dev,nox,noy);
i<-0

by(a,list(a$Prey,a$Predator),function(x) {
   if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
   plot(x$stomcon.hat.dir,x$Residual.dir,xlab='Expected relative stomach content',ylab=paste('Residual',ex1))
   abline(h=0, lty=3)
   title(main=paste("Pred:",x[1,]$Predator,", Prey:",x[1,]$Prey,sep=""))
   i<<-i+1
})
