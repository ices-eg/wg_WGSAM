######


partial_M2<-function(
  
  use.prediction.M2=FALSE,
  use.OP.prediction.M2=FALSE,
  last.y=2035,
  dev=c("screen",'png')[2],
  nox=2,
  noy=3,
  pred_condense_file="pred_format.dat",
  MyPalette=c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' ),
  makeAllGraphs=FALSE,
  PartialM2.dir='.',
  op.dir='.'
 
) {

  a<-Read.part.M2.data()
  a<-subset(a, !(Prey.age==0 & Quarter<=2))

  ## add prediction M2
  if (use.prediction.M2) {
    b<-Read.part.M2.prediction.data(dir=op.dir)
    a<-rbind(a,b)
  }
  #
   
  ## add prediction M2
  if (use.OP.prediction.M2) {
    b<-Read.part.M2.OP.prediction.data(dir=op.dir)
    b$Area<-NULL      #  to be changed
    a<-rbind(a,b)
  }
  #
  
  a<-subset(a,Year<=last.y)
  
  
  # annual part M2
  b<-tapply(a$Part.M2,list(a$Year,a$Predator,a$Predator.age,a$Prey,a$Prey.age),sum)
  b[is.na(b)]<-0
  
  #  print(round(b[,'Cod','1','Cod','0'],digits=3), zero.print = ".")
  #print(round(b[,'Cod',,'Cod',1:3],digits=3), zero.print = ".")
  #print(round(b[,'Whiting',,'Whiting',1:3],digits=3), zero.print = ".")
  ##########################################
  #a<-subset(a,Part.M2>0 & Prey=='Sandeel' & Prey.age<=4)
  a<-subset(a,Part.M2>0 &  Prey.age<=4)
  
  cleanup()
  
  # Special, condense of predators with annual M2 output
  do.plot<-function(){
    file<-file.path(data.path,pred_condense_file)
    if (file.exists(file)) {
      s<-read.table(file,header=TRUE)
      pformat<-unique(subset(s,newno<99,select= -old))
      pformat<-pformat[order(pformat$newno),]
      pformat<-as.character(pformat$new)
      s<-merge(x=a,y=s,by.x='Predator',by.y='old',all.y=T)
    }  
    #subset(s,is.na(Part.M2))
    s<- subset(s,!is.na(Part.M2))
    a<-aggregate(s$Part.M2,list(s$new,s$Year,s$newno,s$Prey,s$Prey.no,s$Prey.age),sum)
    names(a)<-c("Predator","Year","Predator.no","Prey","Prey.no","Prey.age","Part.M2")
  
    i<-0
    all.pred.col<-sort(unique(a$Predator.no),decreasing = TRUE)
    all.prey.col<-sort(unique(a$Prey.no),decreasing = TRUE)
    all.names<-rep('aaa',length(all.pred.col))
    new.names<-pformat
   for (s in (1:length(all.pred.col))) all.names[s]<-new.names[all.pred.col[s]]

   palette(MyPalette)
 
   by(a,list(a$Prey),function(x) {
      #x<-subset(a,Prey=='Cod')
        b<- tapply(x$Part.M2,list(x$Predator.no,x$Year),sum)
        b[is.na(b)]<-0
        pred.no<-as.numeric(dimnames(b)[[1]])
        v<-pred.no
        for (l in (1:length(v))) pred.no[l]<-v[length(v)-l+1]
        pred.names<-rep('aaa',length(pred.no))
        v<-pred.names
        for (l in (1:length(v))) pred.names[l]<-v[length(v)-l+1]
        for (s in (1:length(pred.no))) pred.names[s]<-new.names[pred.no[s]]
    i<-0
    by(x,list(x$Prey.age,x$Prey),function(x) {
     # x<-subset(x,Prey.age==1)
        b<- tapply(x$Part.M2,list(x$Predator.no,x$Year),sum) 
        b[is.na(b)]<-0
        pred.no.age<-as.numeric(dimnames(b)[[1]])
        pred.names.age<-rep('aaa',length(pred.no.age))
        for (s in (1:length(pred.no.age))) pred.names.age[s]<-new.names[pred.no[s]]
  
  
        length.names<-dimnames(b)[[2]] 
        if ((i %% (nox*noy-1))==0) {
          #newplot(dev,nox,noy,Portrait=TRUE);
          #newplot(dev,nox,noy,w8=12,w11=16,Portrait=F,pointsize=12);
          if (dev =='png') poi<-20 else poi<-12
          filename<-paste0("Partial_M2_annual_",x[1,'Prey'])
          if (makeAllGraphs) filename=file.path(PartialM2.dir,'Annually',filename)
          newplot(dev,nox,noy,w8=12,w11=16,Portrait=F,pointsize=poi,filename=filename);
          
  
           par(mar=c(3,2,2,1))  #  c(bottom, left, top, right)
          plot.new(); legend(x=0,y=1,pred.names,fill=pred.no,cex=1.2,col=pred.no,ncol=2,title='Predators')
          }    
        i<<-i+1
  
        barplot(b,space=0.4,names=length.names,col=pred.no.age)
        title(main=paste(x[1,]$Prey,"age:",x[1,]$Prey.age),cex=1.5)
    })
    })

  } 
  tt<-do.plot()
  if (dev %in% c('png','print')) cleanup()
  
  
  # Special, condense of predators with quarterly M2 output
  
  cleanup()
  nox<-2
  noy<-3
  
  do.plot.q<-function(){
  
    file<-file.path(data.path,'pred_format.dat')
    s<-read.table(file,header=TRUE)
    pformat<-unique(subset(s,newno<99,select= -old))
    pformat<-pformat[order(pformat$newno),]
    pformat<-as.character(pformat$new)
    s<-merge(x=a,y=s,by.x='Predator',by.y='old',all.y=T)
  
    #subset(s,is.na(Part.M2))
    s<- subset(s,!is.na(Part.M2))
    a<-aggregate(s$Part.M2,list(s$new,s$Year,s$Quarter,s$newno,s$Prey,s$Prey.no,s$Prey.age),sum)
    names(a)<-c("Predator","Year","Quarter","Predator.no","Prey","Prey.no","Prey.age","Part.M2")
   
    i<-0
    all.pred.col<-sort(unique(a$Predator.no),decreasing = TRUE)
    all.prey.col<-sort(unique(a$Prey.no),decreasing = TRUE)
    all.names<-rep('aaa',length(all.pred.col))
    new.names<-pformat
    for (s in (1:length(all.pred.col))) all.names[s]<-new.names[all.pred.col[s]]
    
    palette(MyPalette)    
    
    
    cleanup()
    by(a,list(a$Prey,a$Quarter),function(x) {
      b<- tapply(x$Part.M2,list(x$Predator.no,x$Year),sum)
      b[is.na(b)]<-0
      pred.no<-as.numeric(dimnames(b)[[1]])
      v<-pred.no
      for (l in (1:length(v))) pred.no[l]<-v[length(v)-l+1]
      pred.names<-rep('aaa',length(pred.no))
      v<-pred.names
      for (l in (1:length(v))) pred.names[l]<-v[length(v)-l+1]
      for (s in (1:length(pred.no))) pred.names[s]<-new.names[pred.no[s]]
      i<-0
      by(x,list(x$Prey.age,x$Prey),function(x) {
        
        b<- tapply(x$Part.M2,list(x$Predator.no,x$Year),sum) 
        b[is.na(b)]<-0
        pred.no.age<-as.numeric(dimnames(b)[[1]])
        pred.names.age<-rep('aaa',length(pred.no.age))
        for (s in (1:length(pred.no.age))) pred.names.age[s]<-new.names[pred.no[s]]
        
        
        length.names<-dimnames(b)[[2]] 
        if ((i %% (nox*noy-1))==0) {
            #newplot(dev,nox,noy,w8=12,w11=16,Portrait=F,pointsize=12);
          if (dev =='png') poi<-20 else poi<-12
          filename<-paste0("Partial_M2_",x[1,'Prey'],"_Q",x[1,'Quarter'])
          if (makeAllGraphs) filename=file.path(PartialM2.dir,"Quarterly",filename)
          
          newplot(dev,nox,noy,w8=12,w11=16,Portrait=F,pointsize=poi,filename=filename);
          
          par(mar=c(3,2,2,1))  #  c(bottom, left, top, right)
          plot.new(); legend(x=0,y=1,pred.names,fill=pred.no,cex=1.2,col=pred.no,ncol=2,title='Predators')
        }    
        i<<-i+1
        
        barplot(b,space=0.4,names=length.names,col=pred.no.age)
        title(main=paste(x[1,]$Prey," Quarter:",x[1,]$Quarter," age:",x[1,]$Prey.age),cex=1.5)
      })
    })
    
  }
  
  tt<-do.plot.q()
  if (dev %in% c('png','print')) cleanup()
  
  
  
   ###################
  if (FALSE) { 
    cleanup()
    dev<-"print"
    dev<-"screen"
    nox<-3
    noy<-2
    
    i<-0
    all.pred.col<-sort(unique(a$Predator.no),decreasing = TRUE)
    all.prey.col<-sort(unique(a$Prey.no),decreasing = TRUE)
    all.names<-rep('aaa',length(all.pred.col))
    for (s in (1:length(all.pred.col))) all.names[s]<-sp.names[all.pred.col[s]]
    
    by(a,list(a$Prey.age,a$Prey),function(x) {
        b<- tapply(x$Part.M2,list(x$Predator.no,x$Year),sum) 
        b[is.na(b)]<-0
    
        pred.no<-as.numeric(dimnames(b)[[1]])
        pred.names<-rep('aaa',length(pred.no))
        for (s in (1:length(pred.no))) pred.names[s]<-sp.names[pred.no[s]]
        print(names)
        length.names<-dimnames(b)[[2]] 
        if ((i %% (nox*noy-1))==0) {
          newplot(dev,nox,noy,Portrait=TRUE);
          par(mar=c(3,5,3,2)) 
          plot.new(); legend(x=0,y=1,pred.names,fill=pred.no,cex=1.0,ncol=2)
        }    
        i<<-i+1
        barplot(b,space=0.4,names=length.names,col=pred.no)
        title(main=paste(x[1,]$Prey,"age:",x[1,]$Prey.age))
    })
    
    
    
    cleanup()
    newplot(dev,nox,noy,Portrait=TRUE);
    
    ii<-1
    for (prey in (1:dim(c)[3])) {
     cat(paste("\nprey:",dimnames(c)[[3]][prey],'\n'))
     bb<-c[,,prey]
     bb<-rbind(bb,c2[prey,])
     dimbb<-dim(bb)[2]
     cut<-0
     for (i in (dimbb:1)) { if (c2[prey,i]==0) cut=cut+1 else break}
     out<-bb[,1:(dimbb-cut)] 
     print(round(out,digits=3), zero.print = ".")
     
     out<-out[1:(dim(out)[1]-1),]
     
     psum<-apply(out,1,sum)
     
     d<-dim(out)[1]
     for (i in (d:1)) if (psum[i]==0) out<-out[-i,]
    
     if ((ii %% (nox*noy))==0) {
          newplot(dev,nox,noy,Portrait=TRUE);
     }    
     barplot(out,xlab='age',ylab='M2',col=1:dim(out)[2],names=dimnames(out)[[2]])
     legend(x="topright",legend=dimnames(out)[[1]],fill=1:dim(out)[2])
     title(main=paste(dimnames(c)[[3]][prey]))
     ii<-ii+1
    } 
    
    
    
    a<-Read.part.M2.data()
    # annual part M2
    b<-tapply(a$Part.M2,list(a$Year,a$Predator,a$Predator.age,a$Prey,a$Prey.age),sum)
    b[is.na(b)]<-0
    dimnames(b)
    
    c<-apply(b,c(4,5,2,3),mean)
    
    d<-c['Cod',,,]
    dimnames(d)
    
    d<-d[1:3,,]
    dimnames(d)
    
    e<-apply(d,c(2,3,1),sum)
    print(round(e,digits=4), zero.print=".")
                                            
    # special cod 
    a<-Read.part.M2.data()
    # quarterly part M2
    b<-tapply(a$Part.M2,list(a$Year,a$Predator,a$Predator.age,a$Prey,a$Prey.age,a$Quarter.1),sum)
    b[is.na(b)]<-0
    dimnames(b)
    
    c<-apply(b,c(6,4,5,2,3),mean)
    dimnames(c)
    
    #d<-c[,'Cod',,c('Cod','Whiting'),]
    d<-c[,'Cod',,c('Cod'),]
    dimnames(d)
    
    d<-d[,1:3,]
    dimnames(d)
    
    print(round(d,digits=4), zero.print=".")
                                            
    
    
    
    # sandeel;
    a<-Read.part.M2.data()
    a<-subset(a,Prey=='Nor. pout' & Year>=1983)
    a$halfyear<-2
    a$halfyear[a$Quarter==1 | a$Quarter==2]<-1
    b<-tapply(a$Part.M2,list(a$Year,a$Prey.age,a$halfyear),sum)
    b[is.na(b)]<-0
    b
    
    
    cleanup()
    nox<-2
    noy<-3
    dev<-"screen"
    
    newplot(dev,nox,noy,Portrait=TRUE);
    par(mar=c(3,5,3,2)) 
    for (age in (0:3)) {
     barplot(t(b[,age+1,]),xlab='Year',ylab='M2',col=1:2 )
      title(main=paste('age ',age,':\nfirst=',formatC(mean(b[,age+1,1]),digits=2),', second=',formatC(mean(b[,age+1,2]),digits=2),sep=''))
    }
    
    
    
    # sandeel;
    a<-Read.part.M2.data()
    a<-subset(a,Prey=='Nor. pout' & Year>=1983)
    a$halfyear<-2
    a$halfyear[a$Quarter==1 | a$Quarter==2]<-1
    b<-tapply(a$Part.M2,list(a$Year,a$Prey.age,a$halfyear),sum)
    b[is.na(b)]<-0
    b
    
      palette(rainbow(9))
    cleanup()
    nox<-2
    noy<-3
    dev<-"screen"
    b<-b+0.1
    b[,1,1]<-0 # first half, age 0
    
    newplot(dev,nox,noy,Portrait=TRUE);
    par(mar=c(3,5,3,2)) 
    for (age in (0:3)) {
     barplot(t(b[,age+1,]),xlab='Year',ylab='M1+M2',col=1:2 )
      title(main=paste('age ',age,':\nfirst=',formatC(mean(b[,age+1,1]),digits=2),', second=',formatC(mean(b[,age+1,2]),digits=2),sep=''))
    }
    
    
    
    b<-tapply(a$Part.M2,list(a$Year,a$Prey.age,a$Quarter),sum)
    b[is.na(b)]<-0
    b
    
    #cleanup()
    X11()
    nox<-2
    noy<-3
    
    newplot(dev,nox,noy,Portrait=TRUE);
    par(mar=c(3,5,3,2)) 
    for (age in (0:4)) {
     barplot(t(b[,age+1,]),xlab='Year',ylab='M2',col=1:4 )
      title(main=paste('age ',age,':\nQ1=',formatC(mean(b[,age+1,1]),digits=2),
                                  ', Q2=',formatC(mean(b[,age+1,2]),digits=2),
                                  ', Q3=',formatC(mean(b[,age+1,3]),digits=2),
                                  ', Q4=',formatC(mean(b[,age+1,4]),digits=2),sep='')
                                  )
    }
    
    aa<-subset(a,Year>=1982)
    b<-tapply(aa$Part.M2,list(aa$Year,aa$Prey.age),sum)
    bb<-apply(b,2,mean)
  }
}  


if (FALSE) partial_M2(  #HTML output
  use.prediction.M2=FALSE,
  use.OP.prediction.M2=FALSE,
  last.y=2035,
  dev=c("screen",'png')[2],
  nox=2,
  noy=3,
  pred_condense_file="pred_format.dat",
  MyPalette=c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' ),
  makeAllGraphs=TRUE,
  PartialM2.dir= PartialM2.dir
) 


if (FALSE) partial_M2(  
  use.prediction.M2=FALSE,
  use.OP.prediction.M2=F,
  last.y=2050,
  dev=c("screen",'png')[1],
  nox=2,
  noy=3,
  pred_condense_file="pred_format.dat",
  MyPalette=c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' ),
  makeAllGraphs=TRUE,
  PartialM2.dir= 'M2'
) 

