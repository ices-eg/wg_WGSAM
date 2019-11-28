
###############################################################
nox<-2; noy<-2;
paper<-F        # graphics on paper=file (TRUE) or on screen (FALSE)
if ("scenario" %in% ls()) run.ID<-scenario else run.ID<-"M2"         # file id used for paper output
cleanup()


species.in.title<-T
first.year.on.plot<-1963
last.year.on.plot<-2007

percentile<-c(0.50,0.25,0.75,0.025,0.975)   #first value must be 50 and last value the highest
#percentile<-c(0.50,0.25,0.75)   #first value must be 50 and last value the highest
#percentile<-c(0.50,0.25,0.75,0.05,0.95)   #first value must be 50 and last value the highest
percentile<-c(0.50,0.025,0.975)   #first value must be 50 and last value the highest

incl.sp<-c('Cod','Whiting','Haddock','Herring','Nor. pout','Sandeel')  # species names to be included


do.plot.forecastM2<-function(dir=data.path,source="summary",minM2Age=0,maxM2Age=1,sp.plot="Cod") {
#########################################
    control<-read.FLSMS.control(file=file.path(dir,'SMS.dat'))
    sp.names<<-control@species.names
    HCR<-read.FLSMS.predict.control(control=control,file=file.path(dir,'HCR_options.dat'))
    years<-c(control@first.year,control@last.year.model)

    if (source=='summary') {
      M2<-Read.summary.MCMC.data(dir=dir)
      M2<-data.frame(M2,Iteration=1)
    }
    else if (source=='MCMC') {
      M2<-Read.MCMC.detailed.M2(dir=dir,del.zero=T)
    }
    M2<-subset(M2,(Year>=first.year.on.plot & Year<=last.year.on.plot & Age<=maxM2Age & Age>=minM2Age) ,drop=T)
    M2<-subset(M2, Species %in% incl.sp)

    if (paper) dev<-"wmf" else dev<-"screen"
    len.per<-length(percentile)
  
   if (T) {
    b<-tapply(M2$M2,list(M2$Species,M2$Year,M2$Age,M2$Repetion,M2$Iteration),sum)
    b1<-apply(b,c(1,2,3), function(x) quantile(x,probs =percentile,na.rm=TRUE))
    aaa<<-b1
   }
   
    
  for (sp in (sp.plot)) {
    newplot(dev,nox,noy,filename=paste(run.ID,'_',sp.names[sp],sep=''),Portrait=TRUE);
    par(mar=c(3,5,3,2))

    a1<-subset(M2,Species==sp)
    b<-tapply(a1$M2,list(a1$Year,a1$Age,a1$Repetion,a1$Iteration),sum)
    b1<-apply(b,c(1,2), function(x) quantile(x,probs =percentile,na.rm=TRUE))
    
    y<-as.numeric(dimnames(b1)[[2]])

    for (age in ((minM2Age:maxM2Age)+1)) {
      if (age==minM2Age) titl<-paste(sp,"age",age-1)
      else titl<-paste("age",age-1)

      if (sum(b1)>=0.1) {
        plot(y,b1[1,,age],main=paste(sp,"age",age-1),xlab="",ylab='M2',
                type='l',lwd=2,col=1,ylim=c(0,max(b1)))
        for (i in (1:len.per)) lines(y,b1[i,,age],lty=1+i%/%2,col=1+i%/%2,lwd=1.5)
      }
    }
    if (paper) cleanup()
  }
}

do.plot.forecastM2(sp.plot=incl.sp,source="MCMC")
#do.plot.forecastM2(sp.plot=incl.sp ,source="summary")

if (paper) cleanup()


# Special plot to SMS paper, with uncertanties from both the Hessian and from MCMC 

source<-"summary"
M2age<-1 


first.year.on.plot<-1963
last.year.on.plot<-2007

incl.sp<-c('Cod','Whiting','Haddock','Herring','Nor. pout','Sandeel')  # species names to be included


#########################################
    control<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))
    sp.names<<-control@species.names
    HCR<-read.FLSMS.predict.control(control=control,file=file.path(data.path,'HCR_options.dat'))
    years<-c(control@first.year,control@last.year.model)

    if (source=='summary') {
      M2<-Read.summary.MCMC.data(dir=data.path)
      M2<-data.frame(M2,Iteration=1)
    }
    if (source=='MCMC') {
      M2<-Read.MCMC.detailed.M2(dir=data.path,del.zero=T)
    }
    M2<-subset(M2,(Year>=first.year.on.plot & Year<=last.year.on.plot & Age==M2age) ,drop=T)
    M2<-subset(M2, Species %in% incl.sp)


   
    percentile<-c(0.025,0.975)   #first value must be 50 and last value the highest
     len.per<-length(percentile)
 
   if (T) {
    b<-tapply(M2$M2,list(M2$Species,M2$Year,M2$Age,M2$Repetion,M2$Iteration),sum)
    b1<-apply(b,c(1,2,3), function(x) quantile(x,probs =percentile,na.rm=TRUE))
    aaa<<-b1
   }
   
   mcmc<-arr2dfny(aaa)
   names(mcmc)<-list("Percentile","Species","Year","Age","M2")
   mcmc$Age<-NULL

 
  # read SMS.std
  a<-read.table(file.path(data.path,"sms.std"),comment.char = "#",header=FALSE,skip=1)
  tmp<-data.frame(index=a$V1,name=a$V2, value=a$V3,  std=a$V4)
  
  b<-read.table(file.path(data.path,"par_exp.out"),comment.char = "#",header=T)
  tmp<-merge(tmp,b,by.x="index",by.y="parNo",all.x=T)

  var.name<-c("M2_sd0","M2_sd1","M2_sd2")
  a<-subset(tmp,name %in% var.name & age==M2age)
  a$Species=sp.names[a$species]
  a<-subset(a, Species %in% incl.sp)

  a$minus<-a$value-2*a$std
  a$plus<-a$value+2*a$std

  a1<-data.frame(Species=a$Species,Year=a$year,M2=a$value,Percentile="h50")
  a2<-data.frame(Species=a$Species,Year=a$year,M2=a$minus,Percentile="h2.5")
  a3<-data.frame(Species=a$Species,Year=a$year,M2=a$plus,Percentile="h97.5")
 
 aa<-rbind(a1,a2,a3,mcmc)
 
 cleanup()
 trellis.device(device = "windows",
               color = T, width=8, height=6,pointsize = 12,
               new = T, retain = FALSE)
 

myCol<- c(1,1,1,2,2)
myLwd<-c(2,2,2,2,2)
myLty<-c(1,3,3,3,3)
myPch<-c(1,NA,NA,NA,NA)

print(xyplot( M2~Year|Species,groups=Percentile, data=aa, subset=(Species != "Saithe"), 
 as.table=T,   ylab='Predation mortality',
   layout=c(3,2),      between = list(y = c(1, 1),x = c(1, 1)),     
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1), main=NA,
    xlim=c(1976,2000),   # to allow relation='free' and having (0,0) included
   scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='free'),
  # key = list(space = "bottom", points = F, lines = T,cex=1, columns = 2,title='væk',col=myCol, lty=myLty,lwd=myLwd,pch=NA, text = list(lab = as.character(unique(aa$Percentile)),col=1) ),
        panel = function(x, y, subscripts, groups) {
         panel.superpose(x, y,subscripts=subscripts, groups=groups,type='l',
                          col=myCol,lwd=myLwd,lty=myLty,pch=myPch)  
         }
)) 




