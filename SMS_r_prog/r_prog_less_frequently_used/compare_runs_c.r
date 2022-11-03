
nox<-3; noy<-3;
paper<-F        # graphics on paper=file (TRUE) or on screen (FALSE)



run.ID<-'retro-N'         # file id used for paper output
cleanup()

first.year.on.plot<-1975
last.year.on.plot<-2012

doGrid<-T
#incl.sp<-c("Cod")                      # species to be included. Name(s) or "all"
incl.sp<-"all"

first.pch<-1    # first pch symbol
first.color<-1   # first color

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only


if (F) {    # sometimes the dir and labels are defined outside this script !

  dirs<-c("baltic-2012-keyRun-WGSAM2012","baltic-2012-keyRun-updated")
  labels<-c("Key run 2012","Updated")
  
  
  dirs<-c("bal-1-area-run-01-final","baltic-2012-keyRun-updated","bal-4-areas-run-01-final","baltic-2012-keyRun-4-areas")
  dirs<-c("bal-1-area-final - afterMeeting","baltic-2012-keyRun-updated","bal-4-areas-run-01-final","baltic-2012-keyRun-4-areas")
  labels<-c("WKmultbal","Key run 2012 updated","4 areas 2011","4 areas 2012")
  
  
  dirs<-c("baltic-2012-keyRun-updated","baltic-2012-keyRun-4-areas")
  labels<-c("Key run 2012 updated","4 areas 2012")
  
  
  dirs<-c("NS_63-10-Sep-2014","NS_74-13-Oct-2014")
  labels<-c("Old","New")
  

}

dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-keyRun")
labels<-c("2012 key run","2019 key run")

for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 

Init.function() # get SMS.contol object  including sp.names


for (dir in dirs) {
   a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
   a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot  & Age<=8),
             select=c(Species, Year,Quarter,Age,C.obs))
   M2<-data.frame(scen=labels[which(dirs==dir)],vari='C.obs',a)
   names(M2)<-c("scenario","Variable","Species","Year","Quarter","Age","Value")
 if (dir==dirs[1]) all<-rbind(M2) else all<-rbind(all,M2)
}
#all<-droplevels(subset(all,Species=='Sprat'))
values<-tapply(all$Value,list(all$Year,all$scenario,all$Species,all$Variable,all$Age),sum)
values<-values/1000
y<-as.numeric(dimnames(values)[[1]])


if (paper) dev<-"wmf" else dev<-"screen"
if (incl.sp=="all") sp.plot<-sp.names else sp.plot<-incl.sp

len.dir<-length(dirs)


 plotvar<-function(sp=sp,vari='M2',ylab='') {
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
  plotvar(sp=sp,vari="C.obs",ylab="C")
}

if (paper) cleanup();
