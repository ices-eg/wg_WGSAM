# user options

#year window, first and last year
f.year<-2018
l.year<-2019

dev<-'screen'
nox<-1; noy<-3;

# use survey obs for the year after the last assessment year (assuming the survey is conducted the 1. January.)
# 0=no, 1=yes, use extended survey series
extend.survey<-1

fleet.no<-"all"    # specification of fleets to be used in retrospective analysis
                   # "all"  all availeble fleets area used
                   #  fleet number, e.g. fleet.no<-1 or fleet.no<-c(1,2)
#fleet.no<-c(2)

######   end user options  ###################
cleanup()

setwd(data.path)
bio.interact<-FALSE

# retrospective runs are made in a separate dirictory
retro.dir<-"retro"

dir.create(retro.dir,showWarnings = FALSE)

SMS.files.single<-c("sms.exe","effort.in","natmor.in","canum.in","west.in","weca.in","propmat.in","fleet_catch.in","fleet_names.in","fleet_info.dat",
                     "zero_catch_season_ages.in","zero_catch_year_season.in","proportion_M_and_F_before_spawning.in","just_one.in","proportion_landed.in",'Recruitment_years.in')

for (from.file in SMS.files.single) {
  to.file<-file.path(data.path,retro.dir,from.file)
  file.copy(from.file, to.file, overwrite = TRUE)
}

# read data and options into FLR objects
control<-read.FLSMS.control()
#stock<-SMS2FLStocks(read.input=TRUE,read.output=FALSE,bio.interact=FALSE,control=control)
indices<-SMS2FLIndices(control) 
              
old.dir<-getwd()
setwd(retro.dir)

#FLStocks2SMS(FLStock=stock,control=control,bio.interact=bio.interact)
FLIndices2SMS(indices=indices,control=control)

write.FLSMS.control(control,write.multi=bio.interact) 



for (y in (l.year:f.year)){
  
    for (j in 1:length(indices))
    {
      min.yr <- min(as.numeric(dimnames(indices[[j]]@index)$year))
      max.yr <- max(as.numeric(dimnames(indices[[j]]@index)$year))
      if (y < min.yr) stop("year.range is outside indices year range")
      indices[[j]] <- trim(indices[[j]],year=min.yr:(min(max.yr,y+extend.survey)))

    }
    
    outIndi<-indices
    if (fleet.no=="all") outIndi<-indices
    if (fleet.no!="all") {
       outIndi <- FLIndices()
       used.f<-1
       for (f in fleet.no) {  outIndi[[used.f]]<-indices[[f]]; used.f<-used.f+1}
    }
    
    FLIndices2SMS(indices=outIndi,control=control)
 
    control@last.year<-y
    control@last.year.model<-y
    control@read.HCR<-0
    write.FLSMS.control(control,write.multi=bio.interact) 

    shell(paste( file.path(data.path,'retro',"sms.exe"),"-nox -nohess",sep=" "), invisible = TRUE)
    if (!file.copy("summary.out", paste("summary",y,".out",sep=""), overwrite = TRUE)) stop(paste("Retro stopped: something went wrong in copying summary.dat for year:",y))
    file.remove("summary.out")
}

av.F.age<-control@avg.F.ages 
  
lab<-paste('Retrospective anlysis:',f.year,'-',l.year)
if (fleet.no!="all") lab<-paste(lab," fleet:",list(fleet.no))
for (y in(l.year:f.year)) {
  file<-paste('summary',y,'.out',sep='')
  s<-read.table(file,header=TRUE)
  s<-data.frame(Species=control@species.names[s$Species.n],s)
  s<-subset(s,Z>-1)
  rec<-subset(s,Quarter==control@rec.season & Age==control@first.age )
  rec<-tapply(rec$N,list(rec$Species.n,rec$Year),sum)/1000
 
  s1<-subset(s,Quarter==1)
  ssb<-tapply(s1$SSB,list(s1$Species.n,s1$Year),sum)/1000
  if (y==l.year) {
   SSB<-array(NA,dim=c(nsp,dim(ssb)[2],l.year-f.year+1),   
         dimnames=list(control@species.names,seq(control@first.year,l.year),(l.year:f.year)))
   FI<-SSB
   REC<-SSB # copy structure
  }
  SSB[,1:dim(ssb)[2],l.year-y+1]<-ssb
  REC[,1:dim(ssb)[2],l.year-y+1]<-rec

  F.at.age<-tapply(s$F,list(s$Year,s$Age,s$Species.n),sum,na.rm=T)
  for (sp in (1:nsp)){
    FI[,1:dim(ssb)[2],l.year-y+1]<-apply(F.at.age[ ,(av.F.age[1]-fa+1):(av.F.age[2]-fa+1),sp],c(1),mean)  ### kun med en art-- fejl
  }
}  


noxy<-nox*noy
i<-0
newplot(dev,nox,noy,Portrait=T)
#par(oma=c(2.5,0.5,1.5,0.5),cex=1,las=1,mar=c(1,6,2,1))
par(mar=c(1,6,2,2))
plot.retro<-function(x,label,main=' ',legend.x=0.15,legend.y=1) { 
yr<-as.numeric(dimnames(x)[[2]])
nyr<-l.year-f.year+1
for (sp in (1:nsp)) {
  if (i==noxy) {newplot(dev,nox,noy); i<<-0 }
  plot(yr,x[sp,,1],xlab='',ylab=label,main=main,type='b',
    ylim=c(0,max(x[sp,,],na.rm=T)),lwd=2,pch=1)
   legend(x=max(yr)-legend.x*(max(y)-min(yr)),y=max(x)*legend.y,legend=as.character(seq(f.year,l.year)),
    pch=seq(1,nyr),col=1)
      grid()
  i<<-i+1
  if (nyr>1) for (y in ((f.year+1):(l.year))) {
    yy<-y-f.year+1
    lines(yr,x[sp,,yy],type='b',pch=yy,col=1,lwd=2)
  }
} 
}

plot.retro(SSB,'SSB',main=lab,legend.x=0.15)
plot.retro(FI,expression(bar(F)),legend.x=0.4)
plot.retro(REC,"recriuts 10^6",legend.x=0.4)

savePlot(filename= file.path(data.path,'retro',"retro_plot"),type="png")

setwd(data.path)

