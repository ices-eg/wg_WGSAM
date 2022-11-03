
compare_runs_M<-function(
  dirs=dirs,
  labels=labels,
  sumQuarterly=FALSE,  # calc M as sum of quarterly M
  nox=3, noy=2,
  paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
  run.ID='SMS_M',         # file id used for paper output
  doGrid=TRUE,
  extent.SSB=FALSE,  # plot SSB for the year after last assessment year
  first.year.on.plot=1975,
  last.year.on.plot=2020,
  include.assess.forcast.line=FALSE,      # vertical line at last assessment year
  include.F.reference.points=FALSE,
  include.SSB.reference.points=FALSE,
  include.1.std=FALSE,                   # Include values plus/minus 1 times the standard deviation
  include.2.std=FALSE,
  #incl.sp=c("Cod","Whiting","Haddock","Saithe",'Herring',"Sprat",'Nor. pout'),                      # species number to be included. Numbers or "all"
  incl.sp="all",
  first.pch=0,    # first pch symbol
  first.color=1,   # first color
  palette="default"               # good for clolor full plots
  #palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only
) {


#####################  
for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 

Init.function() # get SMS.contol object  including sp.names


if (sumQuarterly) { # calc M as sum of quarterly M1+M2
  for (dir in dirs) {
     Init.function(dir=file.path(root,dir)) # get SMS.control object  including sp.names
     a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & Z>0),
               select=c(Species, Year,Age,M1,M2))
     a$M<-a$M1+a$M2;  a$M1<-NULL; a$M2<-NULL
     M<-data.frame(scen=labels[which(dirs==dir)],vari='M',a)
     names(M)<-c("scenario","Variable","Species","Year","Age","Value")

      

   if (dir==dirs[1]) all<-rbind(M) else all<-rbind(all,M)
  }
  values<-tapply(all$Value,list(all$Year,all$scenario,all$Species,all$Variable,all$Age),sum)

} else {  # calc M on an annual basis
  for (dir in dirs) {
     Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
     a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & Z>0))
     a<-data.frame(scenario=labels[which(dirs==dir)],Variable="M",Year=a$Year, Species=a$Species, Age=a$Age,Z=a$Z,
           dead=a$N.bar*a$Z,deadm=a$N.bar*(a$M2+a$M1))
     if (dir==dirs[1]) alld<-a else alld<-rbind(alld,a)
  }

  list.ZF<- list(alld$Year,alld$scenario,alld$Species,alld$Variable,alld$Age)
  dead<-tapply(alld$dead,list.ZF,sum,na.rm=T)
  deadM<-tapply(alld$deadm,list.ZF,sum)
  Z<-tapply(alld$Z,list.ZF,sum)
  values<-deadM/dead*Z
}


if (incl.sp=="all") sp.plot<-sp.names else sp.plot<-incl.sp
out<-arr2df(values)
colnames(out)<-c('Year','scenario','Species','variable','Age','value')
out<-subset(out,Species %in% sp.plot & !is.na(value))
write.csv(out,file=paste0("M_",if_else(sumQuarterly,'sumQ_','annu_'),'.csv'),row.names=FALSE)

y<-as.numeric(dimnames(values)[[1]])


if (paper) dev<-"png" else dev<-"screen"
if (incl.sp=="all") sp.plot<-sp.names else sp.plot<-incl.sp

len.dir<-length(dirs)


 plotvar<-function(sp=sp,vari='M',ylab='') {
   if (sp %in% dimnames(values)[[3]]) {
    v<-values[,,sp,vari,]
    #print(v)
    maxval<-max(v,na.rm=T)
    if (maxval>0) {
      if ((gi %% (nox*noy))==0  | gi==0) {
        filename<-paste0("compareM_",run.ID,'_',sp)
        if (makeAllGraphs) filename=file.path(compare.dir,filename)
        all_files<<-c(all_files,filename)
        newplot(dev,nox,noy,filename=filename,Portrait=F);

         par(mar=c(0,0,0,0))
        # make legends
        if (paper) lwds<-2 else  lwds<-2
        plot(10,10,axes=FALSE,xlab=' ',ylab=' ',xlim=c(0,1),ylim=c(0,1))
        legend("center",legend=labels,col=first.color:(first.color+len.dir-1),
              pch=first.pch:(first.pch+len.dir-1),cex=2,title=paste0('M1+M2: ',sp))
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
            if (paper) lwds<-2
            else  lwds<-2;
            lines(y,v[,labels[i],ag],col=first.color+i-1,pch=first.pch+i-1,type='b',lwd=lwds)
           }
        }
       }
     }
   }
 }
 
 all_files<-NULL 
 for (sp in (sp.plot)) {
  gi<-0
  plotvar(sp=sp,vari="M",ylab="M")
 }

if (paper) cleanup();
Init.function()
return(all_files)
}
