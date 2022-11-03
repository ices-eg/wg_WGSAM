source.dir        <-  file.path("C:","mv","SMS")
retro<-"Retro analysis"     # defined from retrospctoive analysis
last.y<-"2008"

dirs<-c("San_Sxsa","San_Sxsa_North","San_Sxsa_south")
label<-c("Combined","North","South")
##################################################

retro.dirs      <-  as.list(file.path(source.dir,dirs,retro))


#Import data from Excel Sheet
a<-lapply(retro.dirs,function(d) {
  xls.filename  <-  file.path(d,"Retro Data.xls")
  xls.channel   <-  odbcConnectExcel(xls.file=xls.filename,readOnly=TRUE)
  cat("read ",xls.filename,"\n")
  SSB<-sqlFetch(xls.channel, sqtable="SSB", colnames = F, rownames = F)
  SSB$id<-"SSB"
  Recruits<-sqlFetch(xls.channel, sqtable="Recruits", colnames = F, rownames = F)
  Recruits$id<-"Recruits"
  MeanF<-sqlFetch(xls.channel, sqtable="Mean_F12", colnames = F, rownames = F)
  MeanF$id<-"MeanF"
  close(xls.channel)
  a<-rbind(SSB,Recruits,MeanF)
  a$SXSA<-label[which(d==retro.dirs)]
  a
})

a<-do.call(rbind,a)
head(a)
names(a)<-(sub(last.y,"vari",names(a)))
a<-subset(a,select=c(year ,vari,id,SXSA))
head(a)

b<-subset(a,SXSA!=label[1])
b<-aggregate(list(vari=b$vari),list(year=b$year,id=b$id),sum)
b$SXSA<-"Sum North+South"
b<-subset(b,id!='MeanF')
head(b)
a<-rbind(a,b)

graphics.off()
trellis.device(device = "windows",
               color = T, width=10, height=12,pointsize = 12,
               new = T, retain = FALSE)

myCol<- seq(1,length(dirs)+1)
myLty<- c(rep(1,length(dirs)),3)

print(
  xyplot(vari~year| id,groups=SXSA,data=a, ylab=NULL,  xlab=NULL,
       layout=c(1,3), between = list(y = c(1, 1),x = c(1, 1)),
       scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='free'),
      key = list(space = "bottom", points = F, lines = T,cex=1, columns = 2,title='SXSA',col=myCol, lwd=3,pch=NA,lty=myLty,
               text = list(lab = as.character(unique(a$SXSA)),col=myCol )),

        panel = function(x, y, subscripts, groups) {
           panel.superpose(x, y,subscripts=subscripts, groups=groups,type='l',col=myCol,lwd=2,lty=myLty )
        }
))




