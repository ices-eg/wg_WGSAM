source.dir        <-  file.path("C:","mv","SMS")

add.sum<-T   # sum SSB, Yield and Recruits
last.y<-"2010"

dirs<-c("SAN-area-1","SAN-area-2","San-area-3")
label<-c("area 1","area 2","area 3")

##################################################

retro.dirs      <-  as.list(file.path(source.dir,dirs))


#Import data 
a<-lapply(retro.dirs,function(d) {
  file<-file.path(d,'summary_table_raw.out')
   aa<-read.table(file,header=TRUE)
  aa$SMS<-label[which(d==retro.dirs)]
  aa
})

a<-do.call(rbind,a)
head(a)

a$Recruits<-a$Rec/1000000
a$SSB<-a$SSB/1000
a$MeanF<-a$mean.F
a$Yield<-a$SOP/1000

a<-subset(a,select=c(Year,Recruits,SSB,MeanF,Yield,SMS))

if (add.sum) {
  b<-a
  b<-aggregate(list(SSB=b$SSB, Recruits=b$Recruits, Yield=b$Yield),list(Year=b$Year),sum)
  
  b$SMS<-"Sum"
  b$MeanF<-NA
  a<-rbind(a,b)
}
  
graphics.off()
trellis.device(device = "windows",
               color = T, width=10, height=13,pointsize = 12,
               new = T, retain = FALSE)

myCol<- seq(1,length(dirs)+1)
myLty<- c(rep(1,length(dirs)),3)

print(
  xyplot(SSB+Recruits+MeanF+Yield~Year,groups=SMS,data=a, ylab=NULL,  xlab=NULL,  subset=Year>1981,
       layout=c(1,4), between = list(y = c(1, 1),x = c(1, 1)),
       scales = list(x = list( cex=0.8,relation='same'), y= list(cex=0.8),alternating = 1,relation='free'),
      key = list(space = "bottom", points = F, lines = T,cex=1, columns = 3,title='Sandeel',col=myCol, lwd=3,pch=NA,lty=myLty,
               text = list(lab = as.character(unique(a$SMS)),col=myCol )),

        panel = function(x, y, subscripts, groups) {
         panel.superpose(x, y,subscripts=subscripts, groups=groups,type='l',col=myCol,lwd=2,lty=myLty )
        }
))
