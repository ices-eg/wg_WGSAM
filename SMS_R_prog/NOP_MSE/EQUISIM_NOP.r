

#install.packages("FLCore", repo = "http://flr-project.org/R")
#library(devtools)
#install_github("ices-tools-prod/msy")

library(msy)

nsamp <- 1000  # number of stochatic runs for each option

Mstk<-SMS2FLStocks(sumfile=file.path(data.path,'summary.out'),
                         bio.interact=F, read.input=TRUE, read.output=TRUE,control=read.FLSMS.control())




SSB.R.year.first<-SMS.control@SSB.R.year.first
SSB.R.year.last <-SMS.control@SSB.R.year.last
SSB.R.year.first[SSB.R.year.first==-1]<-SMS.control@first.year.model
SSB.R.year.last[SSB.R.year.last==-1]<-SMS.control@last.year.model

# read recruiment years used
recruit.years<-matrix(head(scan(file='recruitment_years.in',comment.char='#'),-1),ncol=SMS.control@last.year.model-SMS.control@first.year.model+1 ,byrow=T)
colnames(recruit.years)<-c(as.character(seq(SMS.control@first.year,SMS.control@last.year)))

dev<-'png'
nox<-1; noy<-1;  
noxy<-nox*noy
i<-noxy

  stk<-Mstk
  class(stk)<-'FLStock'

  # move recruitment to first Quarter
  stk@stock.n[1,,,1,,]<-as.vector(stock.n(stk)[1,,,3,,])
 
  stk<-trim(stk,season=1,year=SSB.R.year.first[1]:(SSB.R.year.last[1]-1))  # Delete the most recent year (as driven by used S/R relation). SSB and recruit are there, but the rest of data is crap (first half-year only)
  
  harvest.spwn(stk)<-0
  m.spwn(stk)<-0
  #stk@stock.n<-stk@stock.n/1000
  cat('\n',sp.names[1+first.VPA-1],'\n SSB:\n');print(ssb(stk))
  
  save(stk,file=file.path(data.path,'NOP_FLR.Rdata'))
  
  models<- c("Ricker", "Segreg", "Bevholt")
 
  
  if (0==length(excl.years<-as.numeric(dimnames(recruit.years)[[2]][0==recruit.years[1,]]))) excl.years<-NULL
  FIT<-eqsr_fit(stk, nsamp = nsamp, models = models,
                    method = "Buckland", 
                    id.sr = paste(sp.names[1+first.VPA-1],', ',SSB.R.year.first[1],'-',SSB.R.year.last[1],sep=''), 
                    remove.years = excl.years)
  

  if (i>=noxy) {
    if (dev=='png') cleanup()
    newplot(dev,filename=paste('equisim',1,sep='_'),nox,noy,Portrait=T);
    i<-0
  }
  eqsr_plot(FIT,Scale=0.001,n=nsamp)
dev.off()

if (dev=='png') cleanup()


 print(FIT$sr.det)

