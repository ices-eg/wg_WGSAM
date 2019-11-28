
nox<-2; noy<-3;
paper<-T        # graphics on paper=file (TRUE) or on screen (FALSE)
Portrait<-T

cleanup()
include.probability<-T
incl.sp<-c(1)                    # species number to be included.

palette("default")               # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only

dirsLabel<-c(
"FWK_ss_Hockey_74_05",                "ss Hockey 1974-05 160kt",
"FWK_ss_Hockey_87_05_90",             "ss Hockey 1987-05 90kt",
"FWK_ss_Hockey_87_05_160",            "ss Hockey 1987-05 160kt",
"FWK_ss_geo_74_05",                   "ss geo mean 1974-05",
"FWK_ss_geo_77_83",                   "ss geo mean 1977-83",
"FWK_ss_geo_87_05",                   "ss geo mean 1987-05",

"FWK_ms_all_stom_Hockey_74_05",       "ms 1977-94 stom, Hockey 1974-05 160kt",
"FWK_ms_all_stom_Hockey_87_05_90",    "ms 1977-94 stom, Hockey 1987-05 90kt",
"FWK_ms_all_stom_Hockey_87_05_160",   "ms 1977-94 stom, Hockey 1987-05 160kt",
"FWK_ms_all_stom_geo_74_05",          "ms 1977-94 stom, geo 1974-05",
"FWK_ms_all_stom_geo_77_83",          "ms 1977-94 stom, geo 1977-83",
"FWK_ms_all_stom_geo_87_05",          "ms 1977-94 stom, geo 1987-05",

"FWK_ms_77_86_stom_Hockey_74_05",     "ms 1977-86 stom, Hockey 1974-05 160kt",
"FWK_ms_77_86_stom_Hockey_87_05_90",  "ms 1977-86 stom, Hockey 1987-05 90kt" ,
"FWK_ms_77_86_stom_Hockey_87_05_160", "ms 1977-86 stom, Hockey 1987-05 160kt" ,
"FWK_ms_77_86_stom_geo_74_05",        "ms 1977-86 stom, geo 1974-05" ,
"FWK_ms_77_86_stom_geo_77_83",        "ms 1977-86 stom, geo 1977-83",
"FWK_ms_77_86_stom_geo_87_05",        "ms 1977-86 stom, geo 1987-05",

"FWK_ms_87_94_stom_Hockey_74_05",     "ms 1987-94 stom, Hockey 1974-05 160kt",
"FWK_ms_87_94_stom_Hockey_87_05_90",  "ms 1987-94 stom, Hockey 1987-05 90kt" ,
"FWK_ms_87_94_stom_Hockey_87_05_160", "ms 1987-94 stom, Hockey 1987-05 160kt" ,
"FWK_ms_87_94_stom_geo_74_05",        "ms 1987-94 stom, geo 1974-05",
"FWK_ms_87_94_stom_geo_77_83",        "ms 1987-94 stom, geo 1977-83",
"FWK_ms_87_94_stom_geo_87_05",        "ms 1987-94 stom, geo 1987-05"
)


dirs<-matrix(dirsLabel,ncol=2)
dirs<-dirsLabel[(1:length(dirsLabel)) %% 2==1]
labels<-dirsLabel[(1:length(dirsLabel)) %% 2==0]


# delete files before the first run is made, only in force if do.single=T
#deleteFiles<-c("*.?0?","*.out","*.mcm","*.bin","admodel.*","*.csv","*.std","*.bar","*.mc2","*.cor","*.psv","*.wmf","*.lg","ud.dat","HCR_prob.dat","HCR_yield.dat","HCR_SSB.dat")
deleteFiles<-NA
do.assessment<-T
do.forecast<-T


new_rec<-function(changeRec=F,dir=dir) {
  # extract Recruits
  s<-Read.summary.data(dir=file.path(root,dir))
  rec<-subset(s,Quarter==3,select=c(Year,Species.n,Age,N)) 
  rec<-subset(rec,Age==SMS.control@first.age & Year>=1977 & Year<=1983 & Species.n==1)
   
  recMean<-tapply(log(rec$N),list(rec$Species.n),mean)
  recStd<-tapply(log(rec$N),list(rec$Species.n),sd)
  p<-Read.SSB.Rec.data(file.path(root,dir))
  if (changeRec) {
    p[1,'alfa']<-recMean
    p[1,'beta']<-0
    p[1,'std']<-recStd
    p[1,'model']<-3
  }
  p[2,'beta']<-p[2,'beta']*1E6   # herring
  p$Species<-NULL
  p$Species.n<-NULL
  write.table(file=file.path(root,dir,"SSB_R.in"),p,row.names = F,col.names = F)
}


if (F) { for (dir in dirs) {   # re-run ms and ss runs
  #control<-read.FLSMS.control(file.path(root,dir,"SMS.dat"))
  #cat(paste(dir,":",control@species.info[1,"SSB/R"],":",control@SSB.R.year.first[1],control@SSB.R.year.last[1],"\n"))
   
  from.file <-file.path(root,"fw-single","HCR_options.dat.gem")
  to.file<-file.path(root,dir,"HCR_options.dat.gem")
  file.copy(from.file, to.file, overwrite = TRUE)
  
  if (T & (any(i<-grep("_ms_", dir)))) {     #re-run all multi-species 
    data.path<-file.path(root,dir)
    print(dir)
    source(file.path(prog.path,"Init.R"))  # remember first to remove data.path and rm statement from init.
    if (do.assessment)  do.a.full.SMS.run(label="run_",pause=F,Screen.show=F, do.run=T,
                           do.single=T,do.multi.1=T,do.multi.2=T,do.multi.2.redo=T,
                           do.hessian=T,do.MCMC=T,mcmc=1,mcsave=1,do.prediction=T,
                           ADMB.options="-gbs 1000000000",deleteFiles=deleteFiles,format.options=F)
   if (any(i<-grep("stom_geo_77_83", dir))) new_rec(changeRec=T,dir=dir) else new_rec(changeRec=F,dir=dir)
   SMS.option<-"-ind run_ms3.dat"
   if (do.forecast) source(file.path(prog.path,"HCR_batch_Baltic-FWK.r"))
  }
  if (T & (any(i<-grep("_ss_", dir)))) {   #re-run all single species
    data.path<-file.path(root,dir)
    print(dir)
    source(file.path(prog.path,"Init.R"))  # remember first to remove data.path and rm statement from init.
    if (do.assessment) do.a.full.SMS.run(label="run_",pause=F,Screen.show=T, do.run=T,
                           do.single=T,do.multi.1=F,do.multi.2=F,do.multi.2.redo=F,
                           do.hessian=T,do.MCMC=T,mcmc=1,mcsave=1,do.prediction=T,
                           ADMB.options="-gbs 1000000000",deleteFiles=deleteFiles,format.options=F)
   if (any(i<-grep("ss_geo_77_83", dir))) new_rec(changeRec=T,dir=dir) else new_rec(changeRec=F,dir=dir)
   SMS.option<-"-ind run_ms0.dat"
   if (do.forecast) source(file.path(prog.path,"HCR_batch_Baltic-FWK.r"))
  }
}}

######################################

ssb.out<-'HCR_SSB.dat'
yield.out<-'HCR_yield.dat'
F.out<-'HCR_F.dat'
prob.out<-'HCR_prob.dat'

Init.function() # get SMS.contol object  including sp.names

# read data and options into FLR objects
control<-read.FLSMS.control()
sp.name<-control@species.names

a<-0
for (dir in dirs) {
  a<-a+1
  ssb<-read.table(file.path(root,dir,ssb.out),header=TRUE)
  yield<-read.table(file.path(root,dir,yield.out),header=TRUE)
  prob<-read.table(file.path(root,dir,prob.out),header=TRUE)
  fi<-read.table(file.path(root,dir,F.out),header=TRUE)

  b<-merge(ssb,yield)
  b<-merge(b,prob)
  b<-merge(b,fi)
  b<-data.frame(dir=dirs[a],label=labels[a],b)
  if (dir==dirs[1]) all.dat<-b  else all.dat<-rbind(all.dat,b)
}
all.dat$Species<-sp.name[all.dat$Species.n]
all.dat$cluF<-NULL

write.csv(all.dat, file = file.path(data.path,'ASCII_overview.csv'),row.names = FALSE)
all.dat<-subset(all.dat,Species.n %in% incl.sp)

if (T)  {
 # user options
 pic<<-0
 pic2<<-0
 if (paper) dev<-"wmf" else dev<-"screen"

 by(all.dat,list(all.dat$Species.n,all.dat$dir), function(a){
     sp<-a[1,"Species.n"]
    
    if ((pic %% (nox*noy))==0) {
       if (paper) cleanup()
       newplot(dev,nox,noy,dir=data.path,filename=paste("HCR_",formatC(pic2,width=3,flag="0"),sp,sep=''),Portrait=Portrait);
       #bottom, left, top, right
      par(mar=c(4,4,3,5)+.1)
      pic<<-0
    }    
    pic<<-pic+1
    pic2<<-pic2+1
    
    s<-a$SSB500/1000
    y<-a$y500/1000
    x<-a$codF
    if (pic<=noy) y.lab<-'SSB & Yield (1000 t)' else y.lab<-" "
    if (pic %% noy==0) x.lab<-'Fishing mortality' else x.lab<-" "
  
    plot(x,s,ylab=y.lab,xlab=x.lab ,ylim=c(min(s,y,0),max(s,y)),lty=1,type='l',lwd=2,col=1,main=paste(pic2,'. ',a[1,"label"],sep=''))
  
    if (F) {
      if (include.probability ) {
        legend("topright",
           c('SSB','Yield', paste('p(SSB<',T1[sp],'t)'), paste('p(SSB<',T2[sp],'t)') ),
           pch="  12",lty=c(1,2,3,4),lwd=rep(2,4),col=c(1,2,3,4))
      } else {
        legend(min(x),0.85*max(s,y),
           c('SSB','Yield','F'),
           pch="   ",lty=c(1,2,5),lwd=rep(2,3))
      }
    }
    lines(x,y,lty=2,lwd=2,col=2)
  
    par(new=T)
    plot(x,a$F500,axes=F,xlab=x.lab, ylab=' ',lty=5,lwd=2,,ylim=c(0,1),type='n')
    if (include.probability) {
      lines(x,a$p.T1,lty=3,lwd=2,type='b',pch="1",col=3)
      lines(x,a$p.T2,lty=4,lwd=2,type='b',pch="2",col=4)
      abline(h=0.05)
    }
    axis(side=4)
    
    if (pic >= nox*(noy-1)) mtext(side=4,line=3.0,"Probability(SSB<limit)",cex=0.75)
    par(xaxs="r")
  
  })
} #end do.plots

 if (paper) cleanup()
