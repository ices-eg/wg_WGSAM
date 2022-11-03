my.dev<-'screen'   # output device:  'screen', 'wmf', 'png', 'pdf'

#my.dev<-'png'
                
cleanup()

dat<-Read.summary.data()
dat <-subset(dat, select=c(Species,Year,Quarter,Species.n,Age,C.obs,weca,Yield,CWsum))
dat$Year<-as.character(dat$Year)

cleanup()
plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=8, height=8, pointsize=12)
  if (dev=='wmf') win.metafile(filename = file.path(output.dir,paste(out,'.wmf',sep='')), width=8, height=10, pointsize=12)
  if (dev=='png') png(filename =file.path(output.dir,paste(out,'.png',sep='')), width = 1200, height = 1400,units = "px", pointsize = 30, bg = "white")
  if (dev=='pdf') pdf(file =file.path(output.dir,paste(out,'.pdf',sep='')), width = 8, height = 10,pointsize = 12,onefile=FALSE)
}


for (sp in (first.VPA:nsp)){
  sp.name<-sp.names[sp]
  s<-subset(dat,Species.n==sp)
  s<-aggregate(Yield~Species+Year+Age,data=s,sum)
  plotfile(dev=my.dev,out=paste(file.name,'_',sp.name,sep=''));
  #par(mar=c(3,4,3,2))
  
  print(barchart(Yield ~ Year|Species,group=Age,stack = TRUE,
  scales = list(x = list(rot = 45)),auto.key = list(space = "right"),data=s))
  
  
}
if (my.dev %in% c('png','wmf','pdf')) dev.off()

ftable(round(tapply(dat$Yield/1000,list(dat$Year,dat$Species),sum)))

ftable(round(tapply(dat$CWsum/1000,list(dat$Year,dat$Species),sum)))

ftable(round(tapply(dat$CWsum/1000,list(dat$Species,dat$Year,dat$Quarter),sum)))

ftable(round(tapply(dat$Yield/1000,list(dat$Species,dat$Year,dat$Quarter),sum)))

