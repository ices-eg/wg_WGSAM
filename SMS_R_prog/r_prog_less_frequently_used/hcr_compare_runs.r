
nox<-2; noy<-3;
paper<-T        # graphics on paper=file (TRUE) or on screen (FALSE)
run.ID<-'all'         # file id used for paper output
cleanup()

first.year.on.plot<-1990
last.year.on.plot<-2040
plot.MCMC<-T                          #plot values from MCMC scenarios. FALSE=plot hindcast values from "summary_table_raw.out"
single.species<-F                    # single species mode or multispecies mode
Portrait<-T
 
include.assess.forcast.line<-F      # verical line at last assessment year
include.F.reference.points<-F
include.SSB.reference.points<-T
include.1.std<-F                   # Include values plus/minus 1 times the stadndard deviation
include.2.std<-T
incl.sp<-seq(15,23)                      # species number to be included. Numbers or "all"
incl.sp<-"all"

first.pch<-0    # first pch symbol
first.color<-1   # first color

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only

dirs<-c("T1-T2_stoc_rec_assess_run01", "T1-T2_stoc_rec_assess_run02","T1-T2_stoc_rec_assess_run03")
labels<-c("Run 1 Single sp Fmsy","Run 2 Multi sp Fmsy","Run 3, with TAC constraints")


dirs<-c("T1-T2_stoc_rec_assess_run01_0pctConstraints", "T1-T2_stoc_rec_assess_run01_15pctConstraints","T1-T2_stoc_rec_assess_run01-Icelandic")
labels<-c("Run 1, no constraints","Run 2, +-15% constraints","Run 3, 50-50 constraints")


 #####################  
for (dir in dirs) {
  if ( file.access(file.path(data.path,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 

Init.function() # get SMS.contol object  including sp.names

for (dir in dirs) {
 if (plot.MCMC) {
  if (!single.species) {
    file<-file.path(data.path,dir,'mcout2_average_eaten_M2.out')
    if ( file.access(file, mode = 0) ==0) { 
       eaten<-read.table(file,header=TRUE)
       eaten<-data.frame(scen=dir,vari='Eaten',eaten)
       names(eaten)<-c("scenario","Variable","Species.n","Year","Value","std")
    } 
  }
  
  file<-file.path(data.path,dir,'mcout2_average_recruit.out')
  rec<-read.table(file,header=TRUE)
  rec<-data.frame(scen=dir,vari='Rec',rec)
  names(rec)<-c("scenario","Variable","Species.n","Year","Value","std")

  file<-file.path(data.path,dir,'mcout2_average_mean_F.out')
  FF<-read.table(file,header=TRUE)
  FF<-data.frame(scen=dir,vari='F',FF)
  names(FF)<-c("scenario","Variable","Species.n","Year","Value","std")
  FF[FF$std== -1,'std']<-0

  file<-file.path(data.path,dir,'mcout2_average_SSB.out')
  ssb<-read.table(file,header=TRUE)
  ssb<-data.frame(scen=dir,vari='SSB',ssb)
  names(ssb)<-c("scenario","Variable","Species.n","Year","Value","std")

  file<-file.path(data.path,dir,'mcout2_average_yield.out')
  yield<-read.table(file,header=TRUE)
  yield<-data.frame(scen=dir,vari='Yield',yield)
  names(yield)<-c("scenario","Variable","Species.n","Year","Value","std")
  #yield.hat<-yield
 }
 if (!single.species) { if (dir==dirs[1]) all<-rbind(rec,FF,ssb,yield,eaten) else all<-rbind(all,rec,FF,ssb,yield,eaten)}
 else {if (dir==dirs[1]) {all<-rbind(rec,FF,ssb,yield)} else all<-rbind(all,rec,FF,ssb,yield)}
 #a$label<-labels[which(dirs==dir)]
 #if (dir==dirs[1]) all2<-a else all2<-rbind(all2,a)
}

head(subset(all,Species.n==1 & scenario=='T1-T2_stoc_rec_assess_run02' & Year>2010 & Year<2020))

all<-subset(all,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
values<-tapply(all$Value,list(all$Year,all$scenario,all$Species.n,all$Variable),sum)/1000
stds<-  tapply(all$std,  list(all$Year,all$scenario,all$Species.n,all$Variable),sum)/1000

values[,,1,'Yield']

y<-as.numeric(dimnames(values)[[1]])

if (include.F.reference.points | include.SSB.reference.points) ref.points<-Read.reference.points()

if (paper) dev<-"png" else dev<-"screen"
if (incl.sp=="all") sp.plot<-unique(yield$Species.n) else sp.plot<-incl.sp
len.dir<-length(dirs)

  plotvar<-function(sp=sp,vari='SSB',tit=vari,ylab='(1000t)') {
    v<-values[,,as.character(sp),vari]
    s<-stds[,,as.character(sp),vari]
    if (vari=='F') {v<-v*1000; s<-s*1000;}
    if (vari=='Rec') {v<-v/1000; s<-s/1000;}

    maxval<-max(v,na.rm=T)
    if (include.1.std) maxval<-max(v+s,na.rm=T)
    if (include.2.std) maxval<-max(v+2*s,na.rm=T)

    if ((gi %% (nox*noy))==0  | gi==0) {
      newplot(dev,nox,noy,filename=paste("com_",run.ID,'_',sp.names[sp],sep=''),Portrait=Portrait);

      # make legends
      if (paper) lwds<-1
      else lwds<-2
      par(mar=c(0,0,0,0))
      plot(10,10,axes=FALSE,xlab=' ',ylab=' ',xlim=c(0,1),ylim=c(0,1))
      legend("center",legend=labels,col=first.color:(first.color+len.dir-1),
              pch=first.pch:(first.pch-1+len.dir),cex=2,title=sp.names[sp])
      gi<<-gi+1  
      par(mar=c(2.5,5,3,1))   # c(bottom, left, top, right)
    }

    gi<<-gi+1

    gt0<-sum(v[,1],na.rm=T)>0.01
    if (gt0) typ<-'b'
    else typ<-'n'
    if (paper) {lwds<-1}
    else { lwds<-2;}

    plot(y,v[,1],main=tit,xlab="",ylab=ylab,
          type=typ,lwd=lwds,ylim=c(0,maxval),col=first.color,pch=first.pch)

    for (i in (2:len.dir)) {
      gt0<-sum(v[,i],na.rm=T)>0.01 
      if (paper) lwds<-1
      else  lwds<-2;

      if (gt0) lines(y,v[,i],col=first.color+i-1,pch=first.pch+i-1,type='b',lwd=lwds)
    }
    
    if (include.1.std) {
       for (i in (1:len.dir)) lines(y,v[,i]+s[,i],lty=2,col=i,lwd=1.5)
       for (i in (1:len.dir)) lines(y,v[,i]-s[,i],lty=3,col=i,lwd=1.5)
    }
     if (include.2.std) {
       for (i in (1:len.dir)) lines(y,v[,i]+2*s[,i],lty=2,col=i,lwd=1.5)
       for (i in (1:len.dir)) lines(y,v[,i]-2*s[,i],lty=3,col=i,lwd=1.5)
    }

    if (include.assess.forcast.line) abline(v=SMS.control@last.year.model)
    
    if (include.SSB.reference.points & vari=="SSB") {
      if (ref.points[sp,"Blim"] >0) abline(h=ref.points[sp,"Blim"]/1000,lty=2)
      if (ref.points[sp,"Bpa"] >0) abline(h=ref.points[sp,"Bpa"]/1000,lty=3)
    }
    if (include.F.reference.points & vari=="F") {
      if (ref.points[sp,"Flim"] >0) abline(h=ref.points[sp,"Flim"],lty=2)
      if (ref.points[sp,"Fpa"] >0) abline(h=ref.points[sp,"Fpa"],lty=3)
    }
  }
  
 for (sp in (sp.plot)) {
  gi<-0

  plotvar(sp=sp,vari="Rec",tit="Recruits",ylab="(billions)")
  plotvar(sp=sp,vari="F",ylab=' ')
  plotvar(sp=sp,vari="SSB")
  plotvar(sp=sp,vari="Yield",tit="Yield")
  #plotvar(sp=sp,vari="Yield.hat",tit="Expected Yield")
  if (!single.species) plotvar(sp=sp,vari="Eaten",tit="Eaten biomass")
  if (paper) cleanup()
}



