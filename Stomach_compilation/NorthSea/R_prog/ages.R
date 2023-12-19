
myPlot<-function(dev,fileN,dir=output_dir,nplot=1,mfrow=NULL,width=8,height=8,mar=NULL,pointsize=12){
  if (dev=='screen') X11(width=width, height=height, pointsize=12)
  if (dev=='wmf') { fileN<-file.path(dir,paste0(fileN,'.wmf')); win.metafile(filename = fileN, width=width, height=height, pointsize=pointsize) }
  if (dev=='png') { fileN<-file.path(dir,paste0(fileN,'.png')); png(         filename = fileN, width =100*width, height =100*height, units = "px", pointsize =pointsize, bg = "white")}
  if (dev=='pdf') { fileN<-file.path(dir,paste0(fileN,'.pdf')); pdf(         file = fileN , width =width, height =height, pointsize = pointsize,onefile=TRUE)}
  if (dev=='trellis.png') trellis.device(device = "png",color = T, width=width*100, height=height*100,pointsize =pointsize, units = "px", new = TRUE, retain = FALSE)
  if (dev=='trellis') { fileN<-file.path(dir,paste0(fileN,'.png'));
  trellis.device(device = "windows",color = T, width=width, height=height, new = TRUE, retain = FALSE)
  #axis.text <- trellis.par.get("axis.text")
  #axis.text$cex<-1.5
  #trellis.par.set("axis.text",axis.text)
  tfontsize<-trellis.par.get('fontsize')
  tfontsize$text<-20
  trellis.par.set("fontsize",tfontsize)
  }

  # trellis.par.get()
  # str(trellis.par.get(),1)


  par(mar=c(3,3,2,3)) # c(bottom, left, top, right 2
  if (is.null(mfrow)) { if (nplot==1) par(mfrow=c(1,1)) else if (nplot==2) par(mfrow=c(2,1)) else if (nplot<=4) par(mfrow=c(2,2)) else if (nplot<=9) par(mfrow=c(3,3)) else if (nplot<=12) par(mfrow=c(4,3))  else  par(mfrow=c(5,4))} else par(mfrow=mfrow)
  if (is.null(mar)) {if (nplot>=4) par(mar=c(1,1,2,2)) else par(mar=c(5, 4, 4, 2)) } else par(mar=mar)

  fl<-file.path(dir,'fileList.dat')
  if (file.exists(fl))  write(fileN,file=fl, append=T) else  write(fileN,file=fl)
}


putAgesOn<-function(d,ages=0:5,testOut=FALSE,doPlot=TRUE,usepredALK=TRUE) {
  ####################   Age at age
  #  test      d<-dd.ysplit[[1]]; d<-tst; ages=0:5; testOut=FALSE; doPlot=FALSE; usepredALK=TRUE

  d[['CA']]<-subset(d[['CA']],Age>=0)
  d[['HH']]$N.old<-d[['HH']]$N

  ## Check for enough age data in all years
  # xtabs(NoAtALK~Quarter+Age,data=d[['CA']])

  ll<-list()
 # Name<-x[['HH']][1,'Name']


  ## Function to add one sample of age max with maximum observed length to years-Quarter combinations with no max-age observations.
  fixAgeMax<-function(x,splitFactor=paste(x$Year,x$quarter),ageMax){
    d=split(x,splitFactor)
    maxLength=max(x[[3]]$LngtCm,na.rm=TRUE)
    for(y in 1:length(d)){
      if(!any(d[[y]][[1]]$Age==ageMax,na.rm=TRUE)) {
        d[[y]][[1]]=rbind(d[[y]][[1]][1,],d[[y]][[1]]);
        d[[y]][[1]][1,"Age"]=ageMax;
        d[[y]][[1]][1,"LngtCm"]=maxLength;
        d[[y]][[1]][1,"NoAtALK"]=1;
      }
    }
    dd <- do.call("c",d)
    return(dd)
  }

  for (i in (3:0)) d<-fixAgeMax(d,ageMax=max(ages)-i) # add dummy age observations, if missing


  ## Function to add one sample of age 0 with minimum observed length to years with no 0-age observations.
  fixAge0<-function(x,splitFactor=x$Year,age0=0){
    d=split(x,splitFactor)
    minLength=min(x[[3]]$LngtCm,na.rm=TRUE)
    for(y in 1:length(d)){
      if(!any(d[[y]][[1]]$Age==age0,na.rm=TRUE)) {
        d[[y]][[1]]=rbind(d[[y]][[1]][1,],d[[y]][[1]]);
        d[[y]][[1]][1,"Age"]=age0;
        # d[[y]][[1]][1,"LngtCm"]=minLength;
        d[[y]][[1]][1,"LngtCm"]=5;
        d[[y]][[1]][1,"NoAtALK"]=1;
      }
    }
    return(do.call("c",d))
  }
  d<-fixAge0(d) # add dummy age observations, if missing


  ## Check for enough age data in all years
  print(xtabs(NoAtALK~Year+Age,data=d[['CA']]))

  d[['CA']]$LngtCm<-trunc(d[['CA']]$LngtCm)
  #xtabs(NoAtALK~LngtCm+Age,data=d[['CA']])

  d=addSpectrum(d,cm.breaks=seq(4,max(d[['HL']]$LngtCm,na.rm=T),by=1))
  cms=attr(d,"cm.breaks");
  checkSpectrum(d)

  #head(d[['HH']])


  plotAgeObs<-function(w) {
    # test w<- d.ysplit[[1]]
    if (as.character(w[['HH']][1,'Year'])>='1991') {
      w[['CA']][ w[['CA']]$Age>max(ages),'Age']<-max(ages)
      aw<-aggregate(NoAtALK~Age+LngtCm,data=w[['CA']],sum)
      aw2<-data.frame(LngtCm=min(aw$LngtCm):max(aw$LngtCm),Age=0,NoAtALK=0)
      aw<-aggregate(NoAtALK~Age+LngtCm,data=rbind(aw,aw2),sum)

      aw2<-tapply(aw$NoAtALK,list(aw$Age,aw$LngtCm),sum)
      aw2[is.na(aw2)]<-0
      myPlot(dev='png',file=paste0('ObservedAges_',w[['CA']][1,'Survey'],w[['CA']][1,'Species'],w[['CA']][1,'Year'],w[['CA']][1,'Quarter']),mfrow=c(1,1))
      barplot(aw2,col=1:7,legend = rownames(aw2),main=NULL)
      cleanup()
    }
  }
  if (doPlot) {
    plotAgeObs(d)
  }


  arr2dfny <- function(arr,name='y') {
    if(is.null(dimnames(arr))){dimnames(arr)<-lapply(dim(arr), FUN=function(x)1:x)}
    dn <- dimnames(arr)
    #if (any(unlist(lapply(dn,is.null)))) stop('Length of dimnames must equal length of dimension.')
    for (i in 1:length(dim(arr))) if (is.null(dn[[i]])) dn[[i]]<-as.character(1:(dim(arr)[i]))
    if(is.null(names(dn))){names(dn)<-paste('index', 1:length(dim(arr)), sep=".")}
    ans <- cbind(expand.grid(dn),as.vector(arr))
    colnames(ans)<-c(colnames(ans)[-ncol(ans)],name)
    return(as.data.frame(ans))
  }

  doALK<-function(b,minAge=min(ages),maxAge=max(ages),verbose=testOut){
    #test b<-d ; ages=0:5; minAge=min(ages);maxAge=max(ages);verbose=FALSE; doPlot=TRUE

    #
    y<-b[['HL']][1,'Year']; q<-b[['HL']][1,'Quarter']; sp<-b[['HL']][1,'Species']
    cat(as.character(y),as.character(q),as.character(b[['HL']][1,'Survey']),as.character(sp))
    xt<-xtabs(NoAtALK~Year+Age+Country,data=b[['CA']])
    if (dim(xt)[1]>0) {
      cat(' **********************************************************\n')
      if (verbose) print(xt)
      i<<-i+1
      if (i %% 20 ==1 & doPlot) myPlot(dev='png',file=paste('ALK',i %/% 20,sep='_'))

      colors<-(minAge:maxAge)+1

      alk <- fitALK(b,minAge=minAge,maxAge=maxAge,method=1,verbose = verbose)
      # b[['HH']]$Nage = predict(alk,type=c("Nage",'ALK')[1])

      okNames<-c(as.character(minAge:(maxAge-1)),paste(maxAge,'+',sep=''))
      okNames<-c(as.character(minAge:(maxAge)))

      if (usepredALK) {
         kk<-predict(alk,type=c("Nage",'ALK')[2])
         pred.ALK<-kk[[1]]

         if (dim(pred.ALK)[[2]]<(maxAge-minAge+1)) {
           for (j in  ((dim(pred.ALK)[[2]]+1):(maxAge-minAge+1)))  pred.ALK<-cbind(pred.ALK,0);
           dimnames(pred.ALK)[[2]]<-okNames
         }
         pred.ALK[is.na(pred.ALK)]<-0

      } else {
        raw.ALK<-rawALK(b,minAge=minAge,maxAge=maxAge)

        if (dim(raw.ALK)[[2]]<(maxAge-minAge+1)) {
          for (j in  ((dim(raw.ALK)[[2]]+1):(maxAge-minAge+1)))  raw.ALK<-cbind(raw.ALK,0);
          dimnames(raw.ALK)[[2]]<-okNames
        }
        raw.ALK[is.na(raw.ALK)]<-0

      }

      # plotALKraw(b, minAge=min(ages), maxAge=max(ages), truncAt = 0, type = "l", ylab = "Proportion", xlab = "Cm", main=a)
      #plotALKfit(alk,row=1,add=FALSE,main=paste(b[[2]][1,'Year'],b[[2]][1,'Quarter']),col=colors,lty=colors)

      # colSums(raw.ALK); colSums(pred.ALK);

      # save alk
      #alk<-NageByHaul(x=alk, row=1,returnALK = TRUE)
      #dimnames(alk)<-dimnames(raw.ALK)
      #ll[[as.character(b[['HH']][1,'Year'])]]<<-alk

      #b[['HH']]$NageRaw<-b[['HH']]$N %*% raw.ALK

      # length distribution by age
      if (usepredALK) ALK<-pred.ALK else ALK<-raw.ALK


      LL<-colSums(b[['HH']]$N) * ALK
      #LL<-LL/rep(colSums(LL),each=dim(LL)[[1]])
      #colSums(LL)
      colnames(LL)<-okNames
      rownames(LL)<-head(cms,-1)
      dimnames(LL)
      LL<-arr2dfny(arr=LL,name='nl')
      colnames(LL)<-c("sizeGroup", "Age","nl")

    } else {
      cat(' Missing age information **********************************************\n')
      Nage<-matrix(99,nrow=nrow(b[['HH']]),ncol=maxAge-minAge+1)
      colnames(Nage)<-c(as.character(minAge:(maxAge-1)),paste(maxAge,'+',sep=''))

      b[['HH']]$Nage <-Nage
      rownames(Nage)<-b[['HH']]$haul.id
      b[['HH']]$NageRaw<-Nage
      LL<-data.frame(sizeGroup=0, Age=0,nl=0)
    }
    LL$species<-sp;LL$Year<-y;LL$Quarter<-q

    return(LL)
  }  # end doALK function
   doALK(b=d)

}  # end function PutAgesOn

