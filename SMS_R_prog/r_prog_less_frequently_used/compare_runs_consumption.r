
nox<-2; noy<-3;
paper<-F        # graphics on paper=file (TRUE) or on screen (FALSE)

sumQuarerly<-T  # use sum of quarterly M2, or calc M2 from annual numbers dead.


run.ID<-'M2'         # file id used for paper output
cleanup()

first.year.on.plot<-1974
last.year.on.plot<-2013
doGrid<-T

#incl.sp<-c("Cod")                      # species to be included. Name(s) or "all"
incl.sp<-"all"

first.pch<-1    # first pch symbol
first.color<-1   # first color

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only


if (F) {    # sometimes the dir and labels are defined outside this script !

  dirs<-c("baltic-2012-keyRun-updated","baltic-2012-keyRun-4-areas")
  labels<-c("Key run 2012 updated","4 areas 2012")

}


dirs<-c("NS_key-2014-ver16_Xo","NS_key-2014-ver16_Xo_ab","NS_key-2014-ver16_Xo_abw")
labels<-c("fixed cons.","variable cons.","weighted var")


dirs<-c("Baltic-2019-V02b_new_stom","Baltic-2012-keyRun-results")
labels<-c("key 2019","key 2012")


Init.function() # get SMS.contol object  including sp.names



  for (dir in dirs) {
     Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
     a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot &  ration>0 ),
               select=c(Species, Year,Age, ration))
     ration<-data.frame(scen=labels[which(dirs==dir)],vari='ration',a)
     names(ration)<-c("scenario","Variable","Species","Year","Age","Value")

      

   if (dir==dirs[1]) all<-rbind(ration) else all<-rbind(all,ration)
  }
  values<-tapply(all$Value,list(all$Year,all$scenario,all$Species,all$Variable,all$Age),sum)

y<-as.numeric(dimnames(values)[[1]])


if (paper) dev<-"wmf" else dev<-"screen"
if (incl.sp=="all") sp.plot<-sp.names else sp.plot<-incl.sp

len.dir<-length(dirs)


 plotvar<-function(sp=sp,vari='Ration',ylab='') {
   if (sp %in% dimnames(values)[[3]]) {
    v<-values[,,sp,vari,]
    #print(v)
    maxval<-max(v,na.rm=T)
    if (maxval>0) {
      if ((gi %% (nox*noy))==0  | gi==0) {
        newplot(dev,nox,noy,filename=paste("com_",run.ID,'_',sp,sep=''),Portrait=F);

         par(mar=c(0,0,0,0))
        # make legends
        if (paper) lwds<-2
        else  lwds<-2
        plot(10,10,axes=FALSE,xlab=' ',ylab=' ',xlim=c(0,1),ylim=c(0,1))
        legend("center",legend=labels,col=first.color:(first.color+len.dir-1),
              pch=first.pch:(first.pch+len.dir-1),cex=2,title=sp)
        gi<<-gi+1

        par(mar=c(3,3,3,1)) # c(bottom, left, top, right)
      }
   
      gi<<-gi+1

      ages<-dimnames(v)[[3]][1:min(nox*noy-1,dim(v)[[3]])]
      for (ag in ages) {
        maxval<-max(v[,,ag],na.rm=T)
        minval<-min(0,v[,,ag],na.rm=T)
        if (maxval>0) {
          plot(y,v[,labels[1],ag],main=paste("age",ag),xlab="",ylab=ylab,type='b',lwd=lwds,ylim=c(minval,maxval),
                    col=first.color,pch=first.pch)
          if (doGrid) grid()
          for (i in (2:len.dir)) {
            if (paper) lwds<-1
            else  lwds<-2;
            lines(y,v[,labels[i],ag],col=first.color+i-1,pch=first.pch+i-1,type='b',lwd=lwds)
           }
        }
       }
     }
   }
  }
  
 for (sp in (sp.plot)) {
  gi<-0
  plotvar(sp=sp,vari="ration",ylab="ration")
}

if (paper) cleanup();
Init.function()

