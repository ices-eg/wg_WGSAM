
dirs<-c("NS-2015-ver01", "NS-2015-ver02","NS-2015-ver04", "NS-2015-ver04","NS-2015-ver05")
labels<-c("NS-2015-ver01", "NS-2015-ver02","NS-2015-ver04", "NS-2015-ver04","NS-2015-ver05")

dev='screen'

SSBparm<-"out"   # read ssb-R parameters from SSB_R.in (="in") or SSB_R.out (="out")
cleanup()
a<-0
for (dir in dirs) {
  data.path<-file.path(root,dir)
    print(data.path) 
  setwd(data.path)

  Init.function() # get SMS.contol object  including sp.names
  
  SSB.R.year.first<-SMS.control@SSB.R.year.first
  SSB.R.year.last <-SMS.control@SSB.R.year.last
  SSB.R.year.first[SSB.R.year.first==-1]<-SMS.control@first.year
  SSB.R.year.last[SSB.R.year.last==-1]<-SMS.control@last.year.model

  a<-a+1                                                                      
  #read SSB/R parameters
  if (SSBparm=="in") {
     p<-Read.SSB.Rec.data.in(dir=file.path(root,dir))
     p<-data.frame(Species=sp.names[(first.VPA):SMS.control@no.species],Species.n=seq(first.VPA,SMS.control@no.species),p)
  }  else {
      p<-Read.SSB.Rec.data(dir=file.path(root,dir))
  }
  b<-data.frame(dir=dirs[a],label=labels[a],SSB.R.year.first=SSB.R.year.first,SSB.R.year.last=SSB.R.year.last,p)
  if (dir==dirs[1]) all.dat<-b  else all.dat<-rbind(all.dat,b)
}

parm.all<-subset(all.dat)


 
a<-0
for (dir in dirs) {
  data.path<-file.path(root,dir)
  setwd(data.path)
   
  Init.function() # get SMS.contol object  including sp.names
  
  SSB.R.year.first<-SMS.control@SSB.R.year.first
  SSB.R.year.last <-SMS.control@SSB.R.year.last
  SSB.R.year.first[SSB.R.year.first==-1]<-SMS.control@first.year
  SSB.R.year.last[SSB.R.year.last==-1]<-SMS.control@last.year.model
 
  a<-a+1

   # extract SSB and Recruits
#   s<-Read.summary.data(extend=include.terminal.year)
   s<-Read.summary.data()

   s1<-subset(s,Quarter==1 & Year<=SMS.control@last.year.model-SMS.control@first.age,select=c(Year,Species.n,Age,SSB))
   ssb<-aggregate(s1$SSB/1000,by=list(s1$Year,s1$Species.n),sum,na.rm=T)
   names(ssb)<-list("Year","Species.n","SSB")
    
   rec<-subset(s,Quarter==SMS.control@rec.season,select=c(Year,Species.n,Age,N))
   rec<-subset(rec,Age==SMS.control@first.age & Year>=SMS.control@first.year+SMS.control@first.age)
   rec<-subset(rec,Year<=SMS.control@last.year.model,select=c(Year,Species.n,N))

   p<-merge(ssb,rec)
 
   b<-data.frame(dir=dirs[a],label=labels[a],p)
  if (dir==dirs[1]) all.dat<-b  else all.dat<-rbind(all.dat,b)
}
ssbrec<-subset(all.dat)

###################################################


yy<-as.character(seq(SMS.control@first.year,SMS.control@last.year.model))
ss<-as.character(seq(first.VPA,nsp))

if (dev=="print") cleanup()
nox<-4
noy<-3

i<-0
newplot(dev,nox,noy,Portrait=FALSE,w11=11)

s.index<-0
i<-noxy


for (sp in (first.VPA:nsp)) {
  recssb<-subset(ssbrec,Species.n==sp)
  maxSSB<-max(recssb$SSB)
  maxRec<-max(recssb$N)/1000000
  print (sp)
  col<-0
  s.index<-s.index+1
  for (cur.dir in dirs) {

    p<-subset(parm.all,cur.dir==dir)
    recssb<-subset(ssbrec,cur.dir==dir)
      

    model<-p[s.index,'model']
    alfa<-p[s.index,'alfa']
    beta<-p[s.index,'beta'] 
    CV<-p[s.index,'std']
    first<-p[s.index,"SSB.R.year.first"]
    last<-p[s.index,"SSB.R.year.last"]
      
    if (i==noxy) {newplot(dev,nox,noy); i<-0 }
  
    x<-seq(0,maxSSB, by=maxSSB/100)*1000
    y<-x   #Copy structure
   cat(model,alfa,beta,'\n')
    if (model==1) y<-alfa*x*exp(-beta*x) 
    if (model==2) y<-alfa*x/(1+x*beta) 
    if (model==3) y<-rep(exp(alfa),length(x)) 
    if (model==4) {for (i in (1:length(x))) y[i]<-exp(alfa)*min(x[i],exp(beta))} 
    if (model==100) {for (i in (1:length(x))) y[i]<-exp(alfa)*min(x[i],beta)}
    
    x<-x/1000
    y<-y/1000000
    
    col<-col+1
    if (cur.dir == dirs[1]) {
      plot(x,y,xlab="SSB 1000t",ylab="recruits 10^9",
           main=sp.names[as.numeric(sp)],
           type='l',col=col,ylim=c(0,max(y)*1.5))
  
    } else  lines(x,y,col=col)
  
    
  
   
  
    i<-i+1
  }
}
