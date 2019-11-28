# comparison with MSVPA
 nox<-2; noy<-3;
sms<-Read.summary.data()

if (F) {
  file<-file.path(root,'data_Northsea','summary.out')

  MSVPA.no<-c('COD','WHG','HAD','POK','HER','SAN','NOP')
  MSVPA.sp<-c('Cod','Whiting','Haddock','Saithe','Herring','Sandeel','Nor. pout')
  MSVPA.prey<-c('Cod','Whiting','Haddock','Herring','Sandeel','Nor. pout')
}
if (T) {
  file<-file.path(root,'data_baltic','Compar01.out')

  MSVPA.no<-c('COD','HER','SPR')
  MSVPA.sp<-c('Cod','Herring','Sprat')
  MSVPA.prey<-MSVPA.sp
}
s<-read.table(file,header=TRUE)

msvpa<-data.frame(MSVPA.species=s$species,  #Species.n=match(s$species,MSVPA.no),
          Species=MSVPA.sp[match(s$species,MSVPA.no)],Age=s$age,Year=s$year,
           MSVPA.M2=s$M2,Quarter=s$quarter,MSVPA.SSB=s$SSB)
unique(msvpa$Species)

b<-merge(sms,msvpa,by=c('Year','Quarter','Age','Species'))
unique(b$Species)

M2<-tapply(b$M2,list(b$Year,b$Age,b$Species),sum,na.rm=T)

msvpa.M2<-tapply(b$MSVPA.M2,list(b$Year,b$Age,b$Species),sum,na.rm=T)

##########################  M2 plot (sum af quarterly M (wrong!!)  #############

#clear graphical windows
 cleanup()

dev<-"print"
dev<-"screen"
nox<-2; noy<-3;
noxy<-nox*noy
par(ask=TRUE)

cleanup()

#by species
year<-as.numeric(dimnames(M2)[[1]])
for (sp in MSVPA.prey) {

  newplot(dev,nox,noy);
  par(mar=c(3,5,3,2))
  i<-0
  for (age in (fa:5)) {
    a<-age+1
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
    max.M2<-max(M2[,a,sp],msvpa.M2[,a,sp])
   print(max.M2)
    if (!is.na(max.M2)) if (max.M2>0.001) {
      plot(year,M2[,a,sp],xlab=" ",ylab="M2",main=paste(sp,' age:',age),type='b', ylim=c(0,max.M2),col=1 )
      lines(year,msvpa.M2[,a,sp],col=4)
      i<-i+1
    }
    a<-age+1
  }
} 

#######################################################
# M2 plot for report (on file if dev="wmf")

#Reformat output
dev<-"print"
dev<-"screen"
#dev<-"wmf"

nox<-4; noy<-3;
noxy<-nox*noy
par(ask=TRUE)

cleanup()
#by species
year<-as.numeric(dimnames(M2)[[1]])
  i<-0
  pic<-1
  newplot(dev,nox,noy,filename="annual_M2_1")
  par(mar=c(3,5,3,2))

  for (sp in (1:nsp)) {
  for (age in (fa:3)) {
    a<-age+1
    if (i==noxy) {pic<-pic+1; newplot(dev,nox,noy,filename=paste("annual_M2_",pic,sep="")); i<-0 }
    max.M2<-max(M2[,a,sp],msvpa.M2[,a,sp])
    if (!is.na(max.M2)) if (max.M2>0.001) {
      plot(year,M2[,a,sp],xlab=" ",ylab="M2",main=paste(name[sp+1]," age:",age),type='b', ylim=c(0,max.M2),col=1 )
      lines(year,msvpa.M2[,a,sp],col=4)
      i<-i+1
    }
  }
} 


###################################

nox<-1; noy<-1;
noxy<-nox*noy
i<-0
newplot(dev,nox,noy)
#by species
year<-as.numeric(dimnames(M2)[[1]])
for (sp in (1:nsp)) {
  if (i==noxy) {newplot(dev,nox,noy); i<-0 }
  max.M2<-max(M2[,,sp],na.rm=T)
  if (!is.na(max.M2)) if (max.M2>0.001) {
    plot(year,M2[,fa+1,sp],xlab=" ",ylab="M2",main=name[sp+1],type='b', ylim=c(0,max.M2),pch=as.character(0) )   
    i<-i+1
    for (age in ((fa+1):8)) {
      a<-age+1
      max.M2<-max(M2[,a,sp])
      if (!is.na(max.M2)) if (max.M2>0.001) lines(year,M2[,a,sp],type='b',pch=as.character(age),col=a)
    }
  }
} 

############################################################ 
##SSB

c<-subset(b,Quarter==1,drop=T)

SSB<-tapply(c$SSB,list(c$Year,c$Species.n),sum,na.rm=T)/1000

msvpa.SSB<-tapply(c$MSVPA.SSB,list(c$Year,c$Species.n),sum,na.rm=T)/1000000


dev<-"print"
dev<-"screen"
#dev<-"wmf"

nox<-2; noy<-2
noxy<-nox*noy
par(ask=TRUE)

cleanup()


#by species
 newplot(dev,nox,noy,filename="SSB_MSVPA_SMS");
 i<-0
year<-as.numeric(dimnames(SSB)[[1]])
for (sp in (1:nsp)) {
    if (i==noxy) {newplot(dev,nox,noy,filename="SSB_MSVPA_SMS"); i<-0 }
    max.SSB<-max(SSB[,sp],msvpa.SSB[,sp])
      plot(year,SSB[,sp],xlab=" ",ylab="SSB (1000t)",main=name[sp+1],type='b', ylim=c(0,max.SSB),col=1 )
      lines(year,msvpa.SSB[,sp],col=4)
      i<-i+1
}
if (dev!="screen") cleanup()

