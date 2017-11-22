retro.runs<-function(f.year,l.year,lab,paper="N"){

file<-file.path(data.path,'SMS.dat',sep="") 
s<-readLines(file)


for (y in(f.year:l.year)) {
 
 s[grep('lastmodelyear',s)]<-paste(y,'  #lastmodelyear')
 file<-paste(data.path,'SMS-retro.dat',sep="") 

 write.table(s,file=file,row.names=FALSE,col.names=FALSE,quote=FALSE)

 #write command file for exe of SMS
 sms.do<-paste(data.path,'sms_do.bat',sep="") 
 cat(paste('cd "',data.path,'"\n',sep=''),file=sms.do)
 cat(paste("del summary.out", "\n",sep=''),file=sms.do,append=T)
 cat(paste(sms.command," -nox -ind sms-retro.dat -nohess >ud",y,".dat\n",sep=''),file=sms.do,append=TRUE)
 cat(paste("copy summary.out summary",y,".dat\n",sep=''),file=sms.do,append=T)
 cat(paste("copy sms.rep sms",y,".rep",sep=''),file=sms.do,append=T)

 command<-paste('"',data.path,"sms_do.bat",'"',sep='')
 system(command,show.output.on.console = TRUE)
}

#f.year<-2002;l.year<-2005
SSB<-array(NA,dim=c(nsp,l.year-years[1]+1,l.year-f.year+1))
FI<-SSB
REC<-SSB
if (last.season.last.year<rec.season) REC<-array(NA,dim=c(nsp,l.year-years[1],l.year-f.year+1))

#name<-Read.species.names()
Init.function()

#y<-2005
for (y in(f.year:l.year)) {
  print(y)
  file<-paste(data.path,"summary",y,".dat",sep="") 
  s<-read.table(file,header=TRUE)
  s<-data.frame(Species=sp.names[s$Species.n],s)
  s1<-subset(s,Quarter==1)
  ssb<-tapply(s1$SSB,list(s1$Species.n,s1$Year),sum)/1000
  SSB[,1:(y-years[1]+1),y-f.year+1]<-ssb
 
  s1<-subset(s,Quarter==rec.season & Age==fa)
  rec<-tapply(s1$N,list(s1$Species.n,s1$Year),sum)/1000
  if (last.season.last.year<rec.season) REC[,1:(y-years[1]),y-f.year+1]<-rec; 
  if (last.season.last.year>=rec.season) REC[,1:(y-years[1]+1),y-f.year+1]<-rec 
 
  
  F.at.age<-tapply(s$F,list(s$Year,,s$Age,s$Species.n),sum,na.rm=T)
 #print(F.at.age)
  for (sp in (1:nsp)){
    FI[,1:(y-years[1]+1),y-f.year+1]<-apply(F.at.age[ ,(av.F.age[1]-fa+1):(av.F.age[2]-fa+1),sp],c(1),mean)  ### kun med en art-- fejl

  }
}  


plot.retro<-function(x,label) { 
if (paper=="Y") dev<-"wmf" else dev<-"screen"
nox<-1; noy<-1;
noxy<-nox*noy
i<-0
if (paper=="Y") cleanup()
newplot(dev,nox,noy,filename=paste(gsub(' ','_',lab),'_',label,sep=''))

yr<-seq(years[1],years[1]+dim(x)[2]-1)
#yr<-seq(years[1],l.year)

for (sp in (1:nsp)) {
  if (i==noxy) {
        if (paper=="Y") cleanup();
        newplot(dev,nox,noy,filename=paste(gsub(' ','_',lab),label,sp,sep='')); i<<-0 
  }
  ret<-l.year-f.year+1
  plot(yr,x[sp,,ret],xlab="",ylab=label,main=lab,type='b', lty=2,
    ylim=c(0,max(x[sp,,],na.rm=T)),lwd=2,pch=ret)   
  i<<-i+1
  if (l.year>f.year) for (y in ((f.year):(l.year-1))) {
    yy<-y-f.year+1 
    if (dev=="wmf") yyc<-1 else yyc<-yy
    lines(yr,x[sp,,yy],type='b',pch=yy,col=yyc,lwd=2)
  }
} 
}
plot.retro(REC,"Recruits (1000)")
plot.retro(SSB,"SSB (1000t)")
plot.retro(FI,paste('F',av.F.age[1],"-",av.F.age[2]))

if (paper=="Y") cleanup()
} 

# remember to check av.F ages
