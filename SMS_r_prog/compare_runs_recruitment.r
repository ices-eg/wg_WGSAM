

first.year.on.plot<-1975
last.year.on.plot<-2020


palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only


 
  dirs<-c("NS_old-key","NS_74-13-Oct-2014")
 labels<-c("2011","2014")
 
 dirs<-c("NS-2015-ver01", "NS-2015-ver02","NS-2015-ver04", "NS-2015-ver04","NS-2015-ver05")
 labels<-c("NS-2015-ver01", "NS-2015-ver02","NS-2015-ver04", "NS-2015-ver04","NS-2015-ver05")
 

dirs<-c("NS_63-10-sep-2014", "v05-NS-2015")
labels<-c("2011-run", "2015-run")

 
for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 

Init.function() # get SMS.contol object  including sp.names

  for (dir in dirs) {
     a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & Age==SMS.control@first.age & Quarter==SMS.control@rec.season & M>0),  
    select=c(Species,Species.n, Year,N) ,drop=T)
      a$label<-labels[ which(dirs==dir)]
    a$N<-a$N/1000000
   if (dir==dirs[1]) all<-a else all<-rbind(all,a)
}

all<-subset(all,Species!='Saithe')  

a<-aggregate(all$N,list(Species=all$Species),mean)
all<-merge(a,all,sort=F)
all$relRec<-all$N/all$x


all<-all[order(all$label,all$Species.n,all$Year),]
               
cleanup() 
#trellis.device(device = "windows",   color = F, width=9, height=9,pointsize = 2, new = TRUE, retain = FALSE)

print(xyplot( N~Year|Species,groups=label, data=all,
  type='b',lwd=1 , layout=c(3,3), ylab='Recruitment (10^9)',
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.7),
   auto.key = list(space = "bottom", points = T, lines = F,cex=1, columns = 2) ,

    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='free')
))


print(xyplot( relRec~Year|Species,groups=label, data=all,
  type='b',lwd=1 , layout=c(4,3), ylab='Standardised recruitment',  lty=1,
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.7),
   auto.key = list(space = "bottom", points = T, lines = F,cex=1, columns = 2) ,

    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='same')
))
