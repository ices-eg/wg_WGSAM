
#Function to plot catch residuals

# use over.all.max to set the maximum size of the reference buble. A value of 0 scales bubles individually  

plot.catch.residuals<-function(dev,nox=1,noy=1,Portrait=TRUE,use.ref.dot=TRUE,add.title=TRUE,over.all.max=1.5) 
{

file<-file.path(data.path,"catch_residuals.out") 
res<-scan(file,sep=',',comment.char = "#") 
res[res==-99.9]<-NA

a<-res
a[a>99]<-NA
max.buble<-max(abs(a),na.rm=TRUE)

Init.function() # get SMS.contol object  including sp.names

nyr<-SMS.control@last.year.model-SMS.control@first.year+1

ir<-1    #counter in residuals file
nox.noy<-nox*noy
plot.no<-nox.noy-1


for (sp in first.VPA:nsp) {
  #ages<-SP.settings[sp,1:4]
  ages<-SMS.control@species.info[sp,"last-age-likelihood"]
    ages<-SMS.control@species.info[sp,"last-age"]

  nag<-ages[1]-SMS.control@first.age+1
  sp.name<-SMS.control@species.names[sp]
  
  if (SMS.control@combined.catches==1) lastSeason<-1  else  lastSeason<-SMS.control@last.season
  
 for (season in 1:lastSeason) {

    plot.no<-plot.no+1

    if (plot.no%%nox.noy==0){
      newplot(dev,nox,noy,Portrait=Portrait,filename=paste(sp.name,plot.no))
      par(mar=c(3,4,3,2))
      if (dev=="wmf") par(mar=c(2,4,2,2))
    }

    tmp<-matrix(res[ir:(ir+(nag+3)*nyr-1)],ncol=nyr,nrow=nag+3)
    la.like<-SMS.control@species.info[sp,"last-age-likelihood"]
    nag.like<-la.like-SMS.control@first.age+1
    tmp<-tmp[4:(nag.like+3),]
    #print(tmp)
    if (!all(is.na(tmp))) {
      max.buble<-max(abs(tmp),na.rm=TRUE)

      xpos <- SMS.control@first.year:SMS.control@last.year.model
      ypos <- fa:la.like
      if (SMS.control@last.season>1 & SMS.control@combined.catches==0) title<- paste(sp.name,", Season ",season,sep="") else  title<- sp.name
     # if (over.all.max>0) maxsize<-0.25/over.all.max*max.buble else maxsize<-0.25
    
      if (!add.title) title<-''
      if (over.all.max>0) residplot(tmp,xpos,ypos,main=title,refdot=use.ref.dot,maxn=over.all.max)
      else  residplot(tmp,xpos,ypos,main=title,refdot=use.ref.dot)
    }
    ir<-ir+nyr*(nag+3)
  }
 }
}



plot.catch.residuals2<-function(dev,nox=1,noy=1,Portrait=TRUE,use.ref.dot=TRUE,add.title=TRUE,over.all.max=1.5)
{
a<-Read.catch.survey.residuals()
res<-subset(a,data=='catch',select=c(Species,Species.n,Year,Quarter,Age,residual))
res[res$residual==-99.9,'residual']<-NA

max.buble<-max(abs(res$residual),na.rm=TRUE)

Init.function() # get SMS.contol object  including sp.names

nox.noy<-nox*noy
plot.no<-nox.noy-1


for (sp in first.VPA:nsp) {
   sp.name<-SMS.control@species.names[sp]
  plot.no<-nox.noy-1
  if (SMS.control@combined.catches==1) lastSeason<-1  else  lastSeason<-SMS.control@last.season

   aa<-subset(res,Species.n==sp)

  over.all.max<-max(aa$residual,na.rm=T)
 for (season in 1:lastSeason) {
    a<-subset(aa,Quarter==season)
    tmp<-tapply(a$residual,list(a$Year,a$Age),sum,na.rm=T)
     print(tmp)
    if (!all(is.na(tmp))) {
    plot.no<-plot.no+1

    if (plot.no%%nox.noy==0){
      newplot(dev,nox,noy,Portrait=Portrait,filename=paste(sp.name,plot.no))
      par(mar=c(3,4,3,2))
      if (dev=="wmf") par(mar=c(2,4,2,2))
    }

      max.buble<-max(abs(tmp),na.rm=TRUE)

      xpos <- sort(unique(a$Year))
      ypos <- sort(unique(a$Age))
      if (SMS.control@last.season>1 & SMS.control@combined.catches==0) title<- paste(sp.name,", Season ",season,sep="") else  title<- sp.name
     # if (over.all.max>0) maxsize<-0.25/over.all.max*max.buble else maxsize<-0.25

      if (!add.title) title<-''
      if (over.all.max>0) residplot(tmp,xpos,ypos,main=title,refdot=use.ref.dot,maxn=over.all.max)
      else  residplot(tmp,xpos,ypos,main=title,refdot=use.ref.dot)
    }
  }
 }
}

#clear graphical windows
# cleanup()

# use over.all.max to set the maximum size of the reference buble. A value of 0 scales bubles individually between plots (e.g by quarter)

#plot.catch.residuals2(dev='screen',nox=1,noy=2,Portrait=F, use.ref.dot=TRUE,add.title=T,over.all.max=6.0)
