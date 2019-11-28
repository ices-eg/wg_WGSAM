
nox<-2; noy<-3;
paper<-T        # graphics on paper=file (TRUE) or on screen (FALSE)
run.ID<-'SumPerc'         # file id used for paper output
cleanup()

first.year.on.plot<-1990
last.year.on.plot<-2040
single.species<-F                    # single species mode or multispecies mode
Portrait<-F

percentile<-c(0.50,0.25,0.75,0.05,0.95)   #first value must be 50 and last value the highest
percentile<-c(0.50,0.05,0.95)   #first value must be 50 and last value the highest

include.assess.forcast.line<-T      # verical line at last assessment year
include.F.reference.points<-F
include.SSB.reference.points<-T
incl.sp<-seq(15,23)                      # species number to be included. Numbers or "all"
incl.sp<-"all"

first.pch<-0    # first pch symbol
first.color<-1   # first color

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only

dirs<-c("T1-T2_stoc_rec_assess_run01", "T1-T2_stoc_rec_assess_run02","T1-T2_stoc_rec_assess_run03")
labels<-c("Run 1 Single sp Fmsy","Run 2 Multi sp Fmsy","Run 3, with TAC constraints")

 #####################  
for (dir in dirs) {
  if ( file.access(file.path(data.path,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 

Init.function() # get SMS.contol object  including sp.names

for (dir in dirs) {
  if (!single.species) {
    file<-file.path(data.path,dir,'mcout_eaten_M2.out')
    if ( file.access(file, mode = 0) ==0) { 
       eaten<-read.table(file,header=TRUE)
       eaten<-data.frame(scen=dir,vari='Eaten',eaten)
       eaten<-subset(eaten,select=c(-Repetion,-Iteration))
       names(eaten)<-c("scenario","Variable","Species.n","Year","Value")
    }
  }
  
  file<-file.path(data.path,dir,'mcout_recruit.out')
  rec<-read.table(file,header=TRUE)
  rec<-data.frame(scen=dir,vari='Rec',rec)
  rec<-subset(rec,select=c(-Repetion,-Iteration))
  names(rec)<-c("scenario","Variable","Species.n","Year","Value")

  file<-file.path(data.path,dir,'mcout_mean_F.out')
  FF<-read.table(file,header=TRUE)
  FF<-data.frame(scen=dir,vari='F',FF)
  FF<-subset(FF,select=c(-Repetion,-Iteration))
  names(FF)<-c("scenario","Variable","Species.n","Year","Value")

  file<-file.path(data.path,dir,'mcout_SSB.out')
  ssb<-read.table(file,header=TRUE)
  ssb<-data.frame(scen=dir,vari='SSB',ssb)
  ssb<-subset(ssb,select=c(-Repetion,-Iteration))
  names(ssb)<-c("scenario","Variable","Species.n","Year","Value")

  file<-file.path(data.path,dir,'mcout_yield.out')
  yield<-read.table(file,header=TRUE)
  yield<-data.frame(scen=dir,vari='Yield',yield)
  yield<-subset(yield,select=c(-Repetion,-Iteration))
  names(yield)<-c("scenario","Variable","Species.n","Year","Value")

 if (!single.species) { if (dir==dirs[1]) all<-rbind(rec,FF,ssb,yield,eaten) else all<-rbind(all,rec,FF,ssb,yield,eaten)}
 else {if (dir==dirs[1]) {all<-rbind(rec,FF,ssb,yield)} else all<-rbind(all,rec,FF,ssb,yield)}
}

all<-subset(all,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
len.per<-length(percentile)

if (include.F.reference.points | include.SSB.reference.points) ref.points<-Read.reference.points()

if (paper) {dev<-"png"; w8=10} else {dev<-"screen"; w8=8}
if (incl.sp=="all") sp.plot<-unique(yield$Species.n) else sp.plot<-incl.sp
len.dir<-length(dirs)

 plotvar<-function(sp=sp,vari='SSB',tit=vari,ylab='(1000t)',div=1) {
  v<-droplevels(subset(values,Variable==vari,select=c(-Variable,-Species.n)))

    if ((gi %% (nox*noy))==0  | gi==0) {
      newplot(dev,nox,noy,filename=paste("com_",run.ID,'_',sp.names[sp],sep=''),Portrait=Portrait,w8=w8);

      # make legends
      if (paper) lwds<-2
      else lwds<-2
      par(mar=c(0,0,0,0))
      plot(10,10,axes=FALSE,xlab=' ',ylab=' ',xlim=c(0,1),ylim=c(0,1))
      legend("center",legend=labels,col=first.color:(first.color+len.dir-1),
              pch=first.pch:(first.pch-1+len.dir),cex=2,title=sp.names[sp])
      gi<<-gi+1  
      par(mar=c(2.5,5,3,1))   # c(bottom, left, top, right)
    }

    gi<<-gi+1

     b<-tapply(v$Value/div,list(v$scenario,v$Year), function(x) quantile(x,probs = percentile))
     y<-as.numeric(dimnames(b)[[2]])
     maxval<-max(unlist(b))
     v<-matrix(unlist(b[1,]),nrow=len.per)

    if (paper) {lwds<-1}
    else { lwds<-2;}

    plot(y,v[1,],main=tit,xlab="",ylab=ylab,type='b',lwd=lwds,ylim=c(0,maxval),col=first.color,pch=first.pch)
    for (j in (2:len.per)) lines(y,v[j,],col=first.color,type='l',lwd=lwds,lty=2)
    
    for (i in (2:len.dir)) {
      v<-matrix(unlist(b[i,]),nrow=len.per)
      lines(y,v[1,],col=first.color+i-1,pch=first.pch+i-1,type='b',lwd=lwds)
      for (j in (2:len.per)) lines(y,v[j,],col=first.color+i-1,type='l',lwd=lwds,lty=2)
    }

    if (include.assess.forcast.line) abline(v=SMS.control@last.year.model)
    
    if (include.SSB.reference.points & vari=="SSB") {
      if (ref.points[sp,"Blim"] >0) abline(h=ref.points[sp,"Blim"]/div,lty=2)
      if (ref.points[sp,"Bpa"] >0) abline(h=ref.points[sp,"Bpa"]/div,lty=3)
    }
    if (include.F.reference.points & vari=="F") {
      if (ref.points[sp,"Flim"] >0) abline(h=ref.points[sp,"Flim"],lty=2)
      if (ref.points[sp,"Fpa"] >0) abline(h=ref.points[sp,"Fpa"],lty=3)
    }
  }
  
 for (sp in (sp.plot)) {
  gi<-0
  values<-subset(all,Species.n==sp)
  
  plotvar(sp=sp,vari="Rec",tit="Recruits",ylab="(billions)",div=1000000)
  plotvar(sp=sp,vari="F",ylab=' ',div=1)
  plotvar(sp=sp,vari="SSB",div=1000)
  plotvar(sp=sp,vari="Yield",tit="Yield",div=1000)
  #plotvar(sp=sp,vari="Yield.hat",tit="Expected Yield")
  if (!single.species) plotvar(sp=sp,vari="Eaten",tit="Eaten biomass",div=1000)
  if (paper) cleanup()
}



