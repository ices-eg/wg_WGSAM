

read_vals<-function(indir) {
  a<-read.table(file=file.path(root,indir,'sms.std'),skip=1) 
  a<-data.frame(index=a$V1,name=a$V2, value=a$V3,  std=a$V4,cv=a$V4/a$V3)
  a<-subset(a,!is.na(cv))
  a[abs(a$cv) >1 ,'cv']<-NA
  return(a)
}

sel<-3
if (sel==1) {
  runs<-c('baltic-test-run_01','baltic-test-run_02','baltic-test-run_04') # used for WD
  myLegends<-c("run 01", "run 02", "run 04")
  pngfile<-'cv_compare.png'
  
  myVars<-c("avg_F","hist_SSB","M2_sd0","M2_sd1","M2_sd2")
  firstDerivedParameter<-"avg_F"
  
}
if (sel==2) {
  runs<-c('baltic-test-run_00','baltic-test-run_05') # used for WD 
  myLegends<-c("run 00", "run 05")
  pngfile<-'cv_compare_00_05.png'
  
  myVars<-c("avg_F","hist_SSB","M2_sd0","M2_sd1","M2_sd2")
  firstDerivedParameter<-"avg_F"
  
}

if (sel==3) {
 runs<-c("NS_2022_test_noNoise","NS_2022_test")
 myLegends<-c("NS no noise", "NS noise")
 pngfile<-'cv_compare_NS.png'
 myVars<-c("M2_sd0","M2_sd1","M2_sd2")
 firstDerivedParameter<-"M2_sd0"
 
 
}

if (sel==4) {
  runs<-c('baltic-test-run_00','baltic-test-run_01','baltic-test-run_05') # used for WD 
  myLegends<-c("run 00", "run 01","run 05")
  pngfile<-'cv_compare_00_01_05.png'
  
  myVars<-c("avg_F","hist_SSB","M2_sd0","M2_sd1","M2_sd2")
  firstDerivedParameter<-"avg_F"
  
}

myColors<-c('black', 'red', 'green')

a<-lapply(runs,read_vals)
idx<-lapply(a,function(x) head(x[grepl(firstDerivedParameter,x$name),'index'],1))
b<-mapply(function(x,y){ b<-subset(x,index>=y); b<-subset(b,name %in% myVars); b<-b[order(b$index),]; b$index<-1:dim(b)[[1]]; b }, x=a,y=idx,SIMPLIFY=FALSE)

lapply(b,function(x) head(x,2))

X11();par(mar=c(1,4,1,1))
png(filename=pngfile,width=1200,height=700,pointsize=20,bg='white',unit='px' ) ;par(mar=c(1,4,1,1))

plot(x=b[[1]]$index,y=b[[1]]$cv,type='l', ylim=c(0,0.3), ylab='CV',xlab='index',lwd=2,col=myColors[1])
aa<-subset(b[[1]],!duplicated(b[[1]]$name), select=c(index,name))
abline(v=aa$index,lwd=2,col='blue')
text(x=aa$index+50,y=0.25,labels=aa$name)
for (i in (2:length(b))) {
  lines(x=b[[i]]$index,y=b[[i]]$cv,type='l',pch=3,col=myColors[i],lwd=2)
}
legend(5, 0.04, myLegends, col = myColors,
       lty = c(1,1,1), pch = c(NA, NA, NA),
       merge = TRUE, bg = "gray90")
cleanup()
