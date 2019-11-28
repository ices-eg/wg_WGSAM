
nox<-2; noy<-2;
paper<-T      # graphics on paper=file (TRUE) or on screen (FALSE)
run.ID<-'NielsH'         # file id used for paper output
run.ID<-'ICES_SMS' 
cleanup()

doGrid<-T
extent.SSB<-F  # plot SSB for the year after last assessment year

first.year.on.plot<-1970
last.year.on.plot<-2018
plot.MCMC<-F                        # plot values from MCMC scenarios. FALSE=plot hindcast values from "summary_table_raw.out"
single.species<-T                   # single species mode or multispecies mode

include.assess.forcast.line<-F      # verical line at last assessment year
include.F.reference.points<-F
include.SSB.reference.points<-F
include.1.std<-FALSE                   # Include values plus/minus 1 times the standard deviation
include.2.std<-F

incl.sp<-c("Cod","Whiting","Haddock","Saithe",'Herring',"Sprat",'Nor. pout')                      # species number to be included. Numbers or "all"
incl.sp<-"all"

first.pch<-0    # first pch symbol
first.color<-1   # first color

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only



if (makeAllGraphs) {
  dirs<-c("NS_key-2014-ver17","NS_key-2017-ver05")
  labels<-c("2014-run","2017-run")
}


if (FALSE) { 


dirs<-c("Baltic-2019-single","Baltic-2019-ICES-results")
labels<-c("SMS-single","ICES-single")

dirs<-c("Baltic-2019-no_size_selection0","Baltic-2019-no_size_selection4","Baltic-2019-keyRun")
labels<-c("Uniform","Constraint uniform","2019 key run")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-keyRun")
labels<-c("2012 keyrun","2019 keyrun")


}


dirs<-c("Baltic-2019-old_stomachs","Baltic-2019-old-new_stomachs","Baltic-2019-keyRun")
labels<-c("old stomachs","old & new stomachs","2019 keyrun")




dirs<-c("Baltic-2019-ICES-results","Baltic-2019-keyRun")
labels<-c("ICES single species","2019 keyrun")


 #####################  
for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 


for (dir in dirs) {
  Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
  
 if (plot.MCMC) {
  if (!single.species) {
    file<-file.path(root,dir,'mcout2_average_eaten_M2.out')
    if ( file.access(file, mode = 0) ==0) { 
       eaten<-read.table(file,header=TRUE)
       eaten<-data.frame(scen=dir,vari='Eaten',eaten)
       names(eaten)<-c("scenario","Variable","Species.n","Year","Value","std")
    } 
  }
  
  file<-file.path(root,dir,'mcout2_average_recruit.out')
  rec<-read.table(file,header=TRUE)
  rec<-data.frame(scen=dir,vari='Rec',rec)
  names(rec)<-c("scenario","Variable","Species.n","Year","Value","std")

  file<-file.path(root,dir,'mcout2_average_mean_F.out')
  FF<-read.table(file,header=TRUE)
  FF<-data.frame(scen=dir,vari='F',FF)
  names(FF)<-c("scenario","Variable","Species.n","Year","Value","std")

  file<-file.path(root,dir,'mcout2_average_SSB.out')
  ssb<-read.table(file,header=TRUE)
  ssb<-data.frame(scen=dir,vari='SSB',ssb)
  names(ssb)<-c("scenario","Variable","Species.n","Year","Value","std")

  file<-file.path(root,dir,'mcout2_average_yield.out')
  yield<-read.table(file,header=TRUE)
  yield<-data.frame(scen=dir,vari='Yield',yield)
  names(yield)<-c("scenario","Variable","Species.n","Year","Value","std")
  yield.hat<-yield
 } else {
 
 # hindcast only
    
   a<-Read.summary.table(dir=file.path(root,dir),read.init.function=FALSE)
   a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)

   if (!single.species) {
     eaten<-subset(a,select=c(Species, Year,Eaten))
     eaten<-data.frame(scen=dir,vari='Eaten',eaten,std=0)
     names(eaten)<-c("scenario","Variable","Species","Year","Value","std")
   }
   
   rec<-subset(a,select=c(Species, Year, Rec))
   rec<-data.frame(scen=dir,vari='Rec',rec,std=0)
   names(rec)<-c("scenario","Variable","Species","Year","Value","std")

   FF<-subset(a,select=c(Species, Year,mean.F),!is.na(mean.F))
   max.year<-max(FF$Year)
   FF<-data.frame(scen=dir,vari='F',FF,std=0)
   names(FF)<-c("scenario","Variable","Species","Year","Value","std")

   ssb<-subset(a,select=c(Species, Year, SSB))
   if (!extent.SSB) ssb<-subset(ssb,Year<=max.year)
   ssb<-data.frame(scen=dir,vari='SSB',ssb,std=0)
   names(ssb)<-c("scenario","Variable","Species","Year","Value","std")

   yield<-subset(a,select=c(Species, Year, SOP))
   yield<-data.frame(scen=dir,vari='Yield',yield,std=0)
   names(yield)<-c("scenario","Variable","Species","Year","Value","std")
   
   yield.hat<-subset(a,select=c(Species, Year, SOP.hat))
   yield.hat<-data.frame(scen=dir,vari='Yield.hat',yield.hat,std=0)
   names(yield.hat)<-c("scenario","Variable","Species","Year","Value","std")

 }
 if (!single.species) { if (dir==dirs[1]) all<-rbind(rec,FF,ssb,yield,yield.hat,eaten) else all<-rbind(all,rec,FF,ssb,yield,yield.hat,eaten)}
 else {if (dir==dirs[1]) {all<-rbind(rec,FF,ssb,yield,yield.hat)} else all<-rbind(all,rec,FF,ssb,yield,yield.hat)}
 a$label<-labels[which(dirs==dir)]
 #if (dir==dirs[1]) all2<-a else all2<-rbind(all2,a)
}

all<-subset(all,(Year>=first.year.on.plot & Year<=last.year.on.plot) ,drop=T)
values<-tapply(all$Value,list(all$Year,all$scenario,all$Species,all$Variable),sum)/1000
stds<-  tapply(all$std,  list(all$Year,all$scenario,all$Species,all$Variable),sum)/1000


#subset(all,Species=='Plaice' & Variable=='SSB')

#values[,,"S. sandeel",'Rec']

y<-as.numeric(dimnames(values)[[1]])

if (include.F.reference.points | include.SSB.reference.points) ref.points<-Read.reference.points()

if (paper) dev<-"png" else dev<-"screen"
if (incl.sp[1]=="all") sp.plot<-unique(yield$Species) else sp.plot<-incl.sp

if (makeAllGraphs) dev<-'png'

len.dir<-length(dirs)

  plotvar<-function(sp=sp,vari='SSB',tit=vari,ylab='(1000t)') {
    v<-values[,,sp,vari]
    s<-stds[,,sp,vari]
    if (vari=='F') {v<-v*1000; s<-s*1000;}
    if (vari=='Rec') {v<-v/1000; s<-s/1000;}

    maxval<-max(v,na.rm=T)
    if (include.1.std) maxval<-max(v+s,na.rm=T)
    if (include.2.std) maxval<-max(v+2*s,na.rm=T)

    if ((gi %% (nox*noy))==0  | gi==0) {
      
      filename<-paste0("compare_",run.ID,'_',sp)
      if (makeAllGraphs) filename=file.path(compare.dir,filename)
      
      newplot(dev,nox,noy,filename=filename,Portrait=F);

      # make legends
      if (paper) lwds<-2  else lwds<-2
      par(mar=c(0,0,0,0))
      plot(10,10,axes=FALSE,xlab=' ',ylab=' ',xlim=c(0,1),ylim=c(0,1))
      legend("center",legend=labels,col=first.color:(first.color+len.dir-1),
              pch=first.pch:(first.pch-1+len.dir),cex=2,title=sp)
      gi<<-gi+1  
      par(mar=c(2.5,5,3,1))   # c(bottom, left, top, right)
    }

    gi<<-gi+1

    gt0<-sum(v[,dirs[1]],na.rm=T)>0.01
    if (gt0) typ<-'b'  else typ<-'n'
    if (paper) lwds<-2 else lwds<-2;

    plot(y,v[,dirs[1]],main=tit,xlab="",ylab=ylab,
          type=typ,lwd=lwds,ylim=c(0,maxval),col=first.color,pch=first.pch)
    if (doGrid) grid()
    for (i in (2:len.dir)) {
      ii<-dirs[i]
      gt0<-sum(v[,ii],na.rm=T)>0.01 
      #if (paper) lwds<-1 else  lwds<-2;

      if (gt0) lines(y,v[,ii],col=first.color+i-1,pch=first.pch+i-1,type='b',lwd=lwds)
    }
    
    if (include.1.std) {
       for (i in (1:len.dir)) lines(y,v[,ii]+s[,i],lty=2,col=i)
       for (i in (1:len.dir)) lines(y,v[,ii]-s[,i],lty=3,col=i)
    }
     if (include.2.std) {
       for (i in (1:len.dir)) lines(y,v[,ii]+2*s[,i],lty=2,col=i)
       for (i in (1:len.dir)) lines(y,v[,ii]-2*s[,i],lty=3,col=i)
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
  
  sp<-sp.plot[1]  #test only
 for (sp in (sp.plot)) {
  gi<-0

  plotvar(sp=sp,vari="Rec",tit="Recruits",ylab="(billions)")
  plotvar(sp=sp,vari="F",ylab=' ')
  plotvar(sp=sp,vari="SSB")
  #plotvar(sp=sp,vari="Yield",tit="Yield")
  #plotvar(sp=sp,vari="Yield.hat",tit="Expected Yield")
  #if (!single.species) plotvar(sp=sp,vari="Eaten",tit="Eaten biomass")
  if (dev %in% c('png','print')) cleanup()
  
}

