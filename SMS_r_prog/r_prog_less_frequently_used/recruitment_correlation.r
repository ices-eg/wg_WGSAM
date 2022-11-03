my.dev<-'screen'
my.dev<-'png'

letterSize<-1.5;  # use 3 for the Baltic (3x3 plot)and 0.5 for the North Sea (9x9)

Init.function() # get SMS.contol object  including sp.names

if (T)  {  # use only years that were uese to estimate stock recruitment  relation
  SSB.R.year.first<-SMS.control@SSB.R.year.first
  SSB.R.year.last <-SMS.control@SSB.R.year.last
  SSB.R.year.first[SSB.R.year.first==-1]<-SMS.control@first.year.model
  SSB.R.year.last[SSB.R.year.last==-1]<-SMS.control@last.year.model
} else {
  SSB.R.year.first<-SMS.control@SSB.R.year.first
  SSB.R.year.last <-SMS.control@SSB.R.year.last
  SSB.R.year.first[]<-SMS.control@first.year.model
  SSB.R.year.last[]<-SMS.control@last.year.model
}  

rec.age<-SMS.control@first.age
rec.quarter<- SMS.control@rec.season
simple.recruit.per.spawner.analysis<-F    # FALSE=used residuals from S/R, TRUE=use simple recruitment per SSB

use.only.years.included.in.the.fit<-T


if (F) {
  rec.age<-1  # just testing
  rec.quarter<- 1
  simple.recruit.per.spawner.analysis<-T
}

# extract SSB and Recruits
s<-Read.summary.data()
s1<-subset(s,Quarter==1 & Year<=SMS.control@last.year.model-rec.age & Species.n>=first.VPA)
ssb<-tapply(s1$SSB,list(s1$Year,s1$Species.n),sum)

rec<-subset(s,Quarter==rec.quarter & Species.n>=first.VPA,select=c(Year,Species.n,Age,N))

rec<-subset(rec,Age==rec.age & Year>=SMS.control@first.year.model+rec.age)
rec<-subset(rec,Year<=SMS.control@last.year.model)

rec<-tapply(rec$N,list(rec$Year,rec$Species.n),sum)
dim(ssb);dim(rec)
dim(ssb)==dim(rec)


if (use.only.years.included.in.the.fit) {
  # read recruiment years used
  recruit.years<-matrix(scan(file='recruitment_years.in',comment.char='#'),nrow=dim(rec)[2],byrow=T)
  colnames(recruit.years)<-c(as.character(seq(SMS.control@first.year,SMS.control@last.year)))
  recruit.years<-t(recruit.years)
  recruit.years<- recruit.years[dimnames(ssb)[[1]],]
  rec[recruit.years==0]<-NA
}



#read SSB/R parameters
 p<-Read.SSB.Rec.data()

model.name<-c('Ricker','Bev. & Holt','Geom. mean','Hockey stick')
 

 
SSB_R<-function(x,cutof=T) {
    delta<-0.5 
    y<-x  # copy structure
    #print(c(alfa,beta,exp(alfa+beta)))
    if (model==1) y<-alfa*x*exp(-beta*x)
    else if (model==51 | model==52) y<-alfa*x*exp(-beta*x+info1*info2)
    else if (model==2) y[]<-alfa*x/(1+x*beta)
    else if (model==3) y[]<-rep(exp(alfa),length(x))
    else if (model==4) {for (ii in (1:length(x))) y[i]<-exp(alfa)*min(x[i],exp(beta))}
    else if (model==5) {for (ii in (1:length(x))) {
        if (x[ii]<=(exp(beta)*(1-delta))) y[ii]<-exp(alfa)*x[ii]
        else if (x[ii]< exp(beta)*(1+delta)) y[ii]=exp(alfa)*(x[ii]-(((x[ii]-exp(beta)*(1+delta))^2)/(4*delta*exp(beta))))
        else if (x[ii]>=exp(beta)*(1+delta)) y[ii]<=exp(alfa+beta)
        else x[i]<-NA
        }}
    else if (model==100) {for (ii in (1:length(x))) y[ii]<-exp(alfa)*min(x[ii],beta)}
    
    if (SMS.control@SSB.R.year.first[s.index]> -1 & cutof==T) {
      y[as.character(SMS.control@first.year.model:(SMS.control@SSB.R.year.first[s.index]-1))] <-NA
     }
    return(y)
}

ss<-as.character(seq(first.VPA,nsp))


########################
# residuals
s.index<-0
est.rec<-rec # copy structure
i<-0

for (sp in ss) { 
  s.index<-s.index+1
  model<-p[s.index,'model']
  alfa<-p[s.index,'alfa']
  beta<-p[s.index,'beta'] 
  info1<-p[s.index,'info1']
  info2<-p[s.index,'info2']
  
  est.rec[,s.index]<-SSB_R(x=ssb[,s.index])
}

 rec.resid<-rec/est.rec   # Default
#rec.resid<-rec   # just testing



first.year<-max(SMS.control@SSB.R.year.first)


#this is the closest we get to the SMS estimate of std, 
#inp<-log(rec.resid[as.character(first.year:SMS.control@last.year.model),] )
inp<-log(rec.resid)
if (simple.recruit.per.spawner.analysis) inp<-rec/ssb
dimnames(inp)[[2]]<-sp.names[as.numeric(colnames(inp))]


    out<-matrix(1,nrow=length(dimnames(inp)[[2]]),ncol=length(dimnames(inp)[[2]]))
    dimnames(out)<-list(dimnames(inp)[[2]],dimnames(inp)[[2]])
   for (sp1 in  dimnames(inp)[[2]])   for (sp2 in  dimnames(inp)[[2]]) {
    if (sp1 !=sp2) {
      a<-cor.test(inp[,sp1],inp[,sp2])
      out[sp1,sp2]<-a$p.val
    }
   }
   round(out,3)
   out.old<-out 


V<-var(inp,use = "complete");V

write.table(V,file= file<-file.path(data.path,'covariance_rec.in'),row.names=F,col.names=F)

#mvrnorm(n=1, mu=rep(0,dim(V)[1]), Sigma=V)


sqrt(diag(V))      #this is the closest we get to the SMS estimate of  sd,  file SSB_R.out
COR<-cov2cor(V)
COR

i<-dim(inp)[2]
xy<-expand.grid(x=1:i,y=1:i)


plotC<-function(x,y) {
   a<- cor.test(inp[,x],inp[,y])
   plot(inp[,x],inp[,y],xlab=dimnames(inp)[[2]][x],ylab=dimnames(inp)[[2]][y],main=round(a$p.val,2))
    z <- lm(inp[,y] ~ inp[,x])
    abline(z) 
}

cleanup()
newplot(dev='screen',nox=i,noy=i);
par(mar=c(1,1,1,1))
for (a in (1:dim(xy)[1])) plotC(xy[a,'x'],xy[a,'y'])



make.template<-function(sp,tit='Yield',my.dev='png') {
 
  p<-0.05
  n<-length(sp)

  des<-matrix(c(
          p,1,1-p,1,   # 1 Yield title
          0,0.1,0,1-p, # 2 F title
          p,1,0,1-p)    # 3 figures
          , nrow=3, ncol=4, byrow=TRUE)
  split.screen(des)     # split display into three screens
  split.screen(c(2,1), screen = 1) # top split yield title in two lines, screen 4 & 5
  split.screen(c(1,n), screen = 5) # top split yield  screen 5 into n screens e.g. 6:13
  split.screen(c(n,1), screen = 2) # split into n.species rows

  split.screen(c(n,n), screen = 3) # Split plot area into n x n graphs

  screen(3); par(mar=c(0,0,1,0));  mtext(tit,side=3)
  for (i in (1:n)) { screen(i+5);par(mar=c(0,0,1.2,0)); mtext(paste(sp[i],'    '),3,cex=letterSize,adj=1) }
  if (my.dev!='png') for (i in (1:n)) { screen(i+n+5); par(mar=c(1,0,0,0));mtext(paste(sp[i],'      '),side=1,cex=0.8) } else {
     for (i in (1:n)) { screen(i+n+5); par(mar=c(1,0,0,0));mtext(paste(" ",substr(sp[i],1,3),'  '),side=1,adj = 0,cex=0.8) }
  }
}

new.screen<-function(coln=1,rown=1) {
  sc<-first.sc-1+(rown-1)*n.species+coln
  #cat('column:',coln,' row:',rown,' screen:',sc,'\n')
  screen(sc)
  par(mar=c(0.1,0,1,1))    #bottom, left, top, right
  par(cex.axis=0.1)
  #sc<<-sc+1
}

 
plotC2<-function(x,y) {
   new.screen(x,y) 
   a<- cor.test(inp[,x],inp[,y])
   plot(inp[,x],inp[,y],axes=F,xlab=NULL,ylab=NULL,cex = letterSize)
   box()
   col<-'black'; if (a$p.val<=0.05 & a$p.val>0 ) col<-"red" 
   title(paste('cor=',formatC(COR[x,y],digits = 2, format = "f"),' p-val=',formatC(a$p.val,digits = 2, format = "f"),sep=''), cex.main = letterSize/2,   col.main= col)
    z <- lm(inp[,y] ~ inp[,x])
    abline(z,col='blue',lwd=2) 
}

# acf(inp[,1])

cleanup() 


nox<-1; noy<-1;  
noxy<-nox*noy

newplot(dev=my.dev,filename='recruit_correlation',nox,noy,Portrait=T,w8=12,w11=14);

n.species<-dim(inp)[[2]]
first.sc<-n.species*2+6  # first screen for plot

make.template(dimnames(inp)[[2]],tit='')
for (aa in (1:dim(xy)[1])) plotC2(xy[aa,'x'],xy[aa,'y'])

if (my.dev=='png') cleanup()


if (F) {
    ###################  to test it works
    # 1. run the OP program to produce file  OP_condensed.out

    # estimate COV from output
    # extract SSB and Recruits
    s<-Read.OP.condensed(dir=file.path(data.path,scenario))
    ssb<-tapply(s$SSB,list(s$Year,s$Species.n),sum)
    rec<-tapply(s$recruit,list(s$Year,s$Species.n),sum)
    
    ########################
    # residuals
    s.index<-0
    est.rec<-rec # copy structure
    i<-0
    
    for (sp in ss) { 
      s.index<-s.index+1
      model<-p[s.index,'model']
      alfa<-p[s.index,'alfa']
      beta<-p[s.index,'beta'] 
      info1<-p[s.index,'info1']
      info2<-p[s.index,'info2']
      
      est.rec[,s.index]<-SSB_R(x=ssb[,s.index],cutof=F)
    }
    
    rec.resid<-rec/est.rec
    
    
    
    inp<-log(rec.resid)
    dimnames(inp)[[2]]<-sp.names[as.numeric(colnames(inp))]
    Vnew<-var(inp);
    
    cat(rep('\n',3))
    V
    Vnew
    
    cat(rep('\n',3))
    sqrt(diag(V))     #this is the closest we get to the SMS estimate of sd,  file SSB_R.out
    sqrt(diag(Vnew))     
    
    CORnew<-cov2cor(Vnew)
    cat(rep('\n',3))
    print(round(COR,2))
    print(round(CORnew,2))
    
    out<-matrix(1,nrow=length(dimnames(inp)[[2]]),ncol=length(dimnames(inp)[[2]]))
    dimnames(out)<-list(dimnames(inp)[[2]],dimnames(inp)[[2]])
   for (sp1 in  dimnames(inp)[[2]])   for (sp2 in  dimnames(inp)[[2]]) {
    if (sp1 !=sp2) {
      a<-cor.test(inp[,sp1],inp[,sp2])
      out[sp1,sp2]<-a$p.val
    }
   }
   print(round(out,2))
   print(round(out.old,2))
   out1<-out
   out2<-out.old
   out1[out1>0.05]<-99
   out2[out2>0.05]<-99
   round(out1,2)
   round(out2,2)
    
    ####
    if (F) {
      aa<-matrix(scan(file.path(data.path,'OP_testOut.out')),byrow=T,ncol=3)
      aa<-log(aa)
      cov(aa)
      (diag(cov(aa))) 
      cor(aa)
    }
 }