# user options

#weigting factor for bias (or other chosen w factor), start with default value
#w.fac<-c(1.0,3.0, 5)
#w.fac<-c(1.0,0.5,2.0)
w.fac<-c(0.01, 0.1, 1, 10, 100)
w.fac<-c(1E-5,1E-6,1E-7)

dev<-'screen'
nox<-1; noy<-1;
cleanup()

######   end user options  ###################



setwd(data.path)
bio.interact<-FALSE

# retrospective runs are made in a separate dirictory
retro.dir<-"retro"

dir.create(retro.dir,showWarnings = FALSE)

SMS.files.single<-c("natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in","fleet_names.in","fleet_info.dat","just_one.in")

for (from.file in SMS.files.single) {
  to.file<-file.path(data.path,retro.dir,from.file)
  file.copy(from.file, to.file, overwrite = TRUE)
}

# read data and options into FLR objects
control<-read.FLSMS.control()

control@read.HCR<-0



setwd(retro.dir)

for (fac in (w.fac)){
    control@obj.func.weight[1,"survey"]<-fac
    #control@obj.func.weight[1,"stomach"]<-fac    # stomach weighting factor used as smooth F factor

    write.FLSMS.control(control,write.multi=bio.interact)

    shell(paste( file.path(data.path,'retro',"sms.exe"),"-nox -nohess",sep=" "), invisible = TRUE)
    
    if (!file.copy("summary.out", paste("summary",fac,".out",sep=""), overwrite = TRUE)) stop("Retro stopped: something went wrong in copying summary.dat")
    file.remove("summary.out")
    
    if (!file.copy("sms.par", paste("sms",fac,".par",sep=""), overwrite = TRUE)) stop("Retro stopped: something went wrong in copying sms.par")
    file.remove("sms.par")

}

av.F.age<-control@avg.F.ages 
  
lab<-''
i<-1
for (fac in(w.fac)) {
  file<-paste('summary',fac,'.out',sep='')
  s<-read.table(file,header=TRUE)
  s<-data.frame(Species=name[s$Species.n+1],s)
  rec<-subset(s,Quarter==control@rec.season & Age==control@first.age)
  rec<-tapply(s$N,list(s$Species.n,s$Year),sum)/1000
 
  s1<-subset(s,Quarter==1)
  ssb<-tapply(s1$SSB,list(s1$Species.n,s1$Year),sum)/1000
  
  if (i==1) {
   SSB<-array(NA,dim=c(nsp,dim(ssb)[2],length(w.fac)),
         dimnames=list(control@species.names,seq(control@first.year,control@last.year.model),(w.fac)))
   FI<-SSB
   REC<-SSB
  }
  SSB[,1:dim(ssb)[2],i]<-ssb
  REC[,1:dim(ssb)[2],i]<-rec
 
  F.at.age<-tapply(s$F,list(s$Year,,s$Age,s$Species.n),sum,na.rm=T)
  for (sp in (1:nsp)){
    FI[,1:dim(ssb)[2],i]<-apply(F.at.age[ ,(av.F.age[1]-fa+1):(av.F.age[2]-fa+1),sp],c(1),mean)  ### kun med en art-- fejl
  }
 i<-i+1
}  


noxy<-nox*noy
i<-0
newplot(dev,nox,noy)

plot.retro<-function(x,label,legend.x=0.15,legend.y=1) { 
yr<-as.numeric(dimnames(x)[[2]])
l.fac<-length(w.fac)

for (sp in (1:nsp)) {
  if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
  plot(yr,x[sp,,1],xlab='',ylab=label,main=lab,type='b',
    ylim=c(0,max(x[sp,,],na.rm=T)),lwd=2,pch=1,col=2)
   legend("topright",legend=as.character(w.fac),pch=seq(1,l.fac),col=seq(1,l.fac))

  i<<-i+1
  f<-1
  for (fac in (w.fac)) {
    if (l.fac>1)  lines(yr,x[sp,,f],type='b',pch=f,col=f,lwd=2);
    f<-f+1
  }
} 
}

plot.retro(SSB,'SSB',legend.x=0.4)
plot.retro(FI,expression(bar(F)),legend.x=0.5)
plot.retro(REC,"recriuts 10^6",legend.x=0.4)




i<-1
for (fac in(w.fac)) {
  file<-paste('sms',fac,'.par',sep='')
  s<-scan(file,comment.char = "#" )
  if (i==1) sms.par<-s else sms.par<-cbind(sms.par,s)
 i<-i+1
}

npar<- dim(sms.par)[1]
first.par<-npar-14
last.par<-npar
bias.fac<-sms.par[first.par:last.par,]
matplot(bias.fac,type='b')
abline(h=1);

setwd(data.path)

