

#install.packages("FLCore", repo = "http://flr-project.org/R")
#library(devtools)
#install_github("ices-tools-prod/msy")

library(msy)

nsamp <- 100  # number of stochatic runs for each option

Mstk<-SMS2FLStocks(sumfile=file.path(data.path,'summary.out'),
                         bio.interact=F, read.input=TRUE, read.output=TRUE,control=read.FLSMS.control())
lapply(Mstk,function(x) x@name) # just check

SSB.R.year.first<-SMS.control@SSB.R.year.first
SSB.R.year.last <-SMS.control@SSB.R.year.last
SSB.R.year.first[SSB.R.year.first==-1]<-SMS.control@first.year.model
SSB.R.year.last[SSB.R.year.last==-1]<-SMS.control@last.year.model

# read recruiment years used
recruit.years<-matrix(head(scan(file='recruitment_years.in',comment.char='#'),-1),ncol=SMS.control@last.year.model-SMS.control@first.year.model+1 ,byrow=T)
colnames(recruit.years)<-c(as.character(seq(SMS.control@first.year,SMS.control@last.year)))

dev<-'png'
nox<-3; noy<-3;  
noxy<-nox*noy
i<-noxy

FITs<-list()


for (s in (1:length(Mstk))) {
  i<-i+1  
  stk<-Mstk[[s]]
  class(stk)<-'FLStock'

  # move recruitment to first Quarter
  stk@stock.n[1,,,1,,]<-as.vector(stock.n(stk)[1,,,3,,])
 
  stk<-trim(stk,season=1,year=SSB.R.year.first[s]:(SSB.R.year.last[s]-1))  # Delete the most recent year (as driven by used S/R relation). SSB and recruit are there, but the rest of data is crap (first half-year only)
  
  harvest.spwn(stk)<-0
  m.spwn(stk)<-0
  #stk@stock.n<-stk@stock.n/1000
  cat('\n',sp.names[s+first.VPA-1],'\n SSB:\n');print(ssb(stk))
  
  models<- c("Ricker", "Segreg", "Bevholt")
  if ( Mstk[[s]]@name  %in% c('Herring')) models<- c("Ricker", "Segreg") # does not work with Ricker ?

  
  if (0==length(excl.years<-as.numeric(dimnames(recruit.years)[[2]][0==recruit.years[s,]]))) excl.years<-NULL
  FIT<-eqsr_fit(stk, nsamp = nsamp, models = models,
                    method = "Buckland", 
                    id.sr = paste(sp.names[s+first.VPA-1],', ',SSB.R.year.first[s],'-',SSB.R.year.last[s],sep=''), 
                    remove.years = excl.years)
  
  FITs[[s]]<-FIT
  if (i>=noxy) {
    if (dev=='png') cleanup()
    newplot(dev,filename=paste('equisim',s,sep='_'),nox,noy,Portrait=T);
    i<-0
  }
  eqsr_plot(FIT,Scale=0.001)
}

if (dev=='png') cleanup()

names(FITs[[1]])
FITs[[1]]$sr.det
FITs[[1]]$id.sr
lapply(FITs,function(x){ print(x$sr.det) })

out<-file.path(data.path,'op_eqsim.in');unlink(out)
sr<-function(x) {
  a<-x$sr.det
  dummy<-data.frame(model=c("Ricker", "Segreg", "Bevholt"))
  a<-merge(x=a,y=dummy,all.y=T)
  a[is.na(a$a),'a']<- 1
  a[is.na(a$b),'b']<- 1
  a[is.na(a$cv),'cv']<- 1
  a[is.na(a$n),'n']<- 0
  a[is.na(a$prop),'prop']<- 0
  print(a)
  
  a$model2<-ifelse(a$model=='Ricker',1,ifelse(a$model=='Segreg',3,ifelse(a$model=="Bevholt",2,NA)))
  a$Species.n<-first.VPA-1+match(substr(x$id.sr,1,3),substr(sp.names[first.VPA:nsp],1,3))
  a$n<-NULL
  write.table(a,file=out,append=T,col.names=F,row.names=F)

}
a<-lapply(FITs,sr)
a<-read.table(file=out,header=F)
names(a)<-c('m','a','b','cv','prop','model','Species.n')


a<-a[order(a$Species.n,a$model),]
a[a$m=='Segreg','a']<-log(a[a$m=='Segreg','a'])  # to fit to SMS
head(a)

unlink(out)
cat('### parameters from Eqsim (a, b,cv, proportion)  (1=Ricker, 2=Beverton&Holt, 3 = segmented regression)\n',file=out)
for (i in (1:dim(a)[[1]])) {
  cat(a[i,'a'],a[i,'b'],a[i,'cv'],a[i,'prop'],'#',sp.names[a[i,'Species.n']],as.character(a[i,'m']),'\n',file=out,append=T)
} 


###  stochastic
head(FITs[[1]]$sr.sto)
dim((FITs[[1]]$sr.sto))

out<-file.path(data.path,'op_eqsim_stoch.in');unlink(out)
sr<-function(x) {
  a<-x$sr.sto
  a$model2<-ifelse(a$model=='Ricker',1,ifelse(a$model=='Segreg',100,ifelse(a$model=="Bevholt",2,NA)))
  a$iter<-1:nsamp
  a$Species.n<-first.VPA-1+match(substr(x$id.sr,1,3),substr(sp.names[first.VPA:nsp],1,3))
  #print(head(a))
  write.table(a,file=out,append=T,col.names=F,row.names=F)
}
a<-lapply(FITs,sr)
a<-read.table(file=out,header=F)
names(a)<-c('a','b','cv','m','model','iter','Species.n')


a<-a[order(a$Species.n,a$iter),]
a[a$m=='Segreg','a']<-log(a[a$m=='Segreg','a'])  # to fit to SMS
head(a)

unlink(out)

cat('### parameters from Eqsim\n',nsamp,'### number of samples\n### parameters,(a, b,cv, model)  (1=Ricker, 2=Beverton&Holt, 100 = segmented regression)\n',file=out)
a$label<-paste(" #",sp.names[a$Species.n], a$m,a$iter)
write.table(subset(a,select=c(a,b,cv,model,label)),file=out,append=T,col.names=F,row.names=F,quote=F)


