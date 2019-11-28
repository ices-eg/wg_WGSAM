
first.year<- 2000                #first year on plot, negative value means value defined by data
last.year<- 2200               #last year on plot
  op.dir<-file.path(data.path,"HCR_1_deter_noadjust_test_01_HCR1_0_Rec0__2030")


my.dev<-'screen'   # output device:  'screen', 'wmf', 'png', 'pdf'
#my.dev<-'png'
  
palette("default")
if (makeAllGraphs)  my.dev<-'png'               
#cleanup()
file.name<-'plot_summary_N_Check'


#dev<-"dummy"
 nox<-2; noy<-2;
 noxy<-nox*noy


Init.function()

dat<-Read.summary.data(dir=op.dir,infile="op_summary.out",read.init.function=F)
dat<-subset(dat,Year<=last.year )


par(mfcol=c(nox,noy))
sp<-16 
s<-subset(dat,Species.n==sp & Quarter==3,select=c(Species, Year, Quarter, Area, Species.n, Age, N,Nbar,NbarStom ) )
    
    par(mar=c(3,4,3,2))
    for (a in (0:3)) {
      ss<-subset(s,Age==a)
      plot(ss$Year,ss$N, main=paste(sp.names[sp],' age',a),ylim=c(0,max(ss$N)),type='l')
      lines(ss$Year,ss$Nbar,col='red')
      lines(ss$Year,ss$NbarStom,col='blue')
    }
