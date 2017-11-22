cleanup()
Portrait<-T                 # graphical output orientation

first.year<- -1975                #first year on plot, negative value means value defined by data
last.year<- 2080               #last year on plot

output.dir<-data.path

##########################################################################
my.dev<-'screen'   # output device:  'screen', 'wmf', 'png', 'pdf'

#my.dev<-'png'

file.name<-'OP_other_var'
#dev<-"dummy"

nox<-2; noy<-2;
noxy<-nox*noy

Init.function()

dat<-Read.op.other.sp.var()
dat<-subset(dat,Year<=last.year )
 

plotfile<-function(dev='screen',out) {
  if (dev=='screen') X11(width=11, height=8, pointsize=12)
  if (dev=='wmf') win.metafile(filename = file.path(output.dir,paste(out,'.wmf',sep='')), width=8, height=10, pointsize=12)
  if (dev=='png') png(filename =file.path(output.dir,paste(out,'.png',sep='')), width = 1200, height = 1400,units = "px", pointsize = 30, bg = "white")
  if (dev=='pdf') pdf(file =file.path(output.dir,paste(out,'.pdf',sep='')), width = 8, height = 10,pointsize = 12,onefile=FALSE)
}


for (sp in (unique(dat$Species.n))) {
    sp.name<-sp.names[sp]

    plotfile(dev=my.dev,out=paste(file.name,'_',sp.name,sep=''));
    par(mfcol=c(nox,noy))
    par(mar=c(3,4,3,2))

    s<-subset(dat,Species.n==sp)
    s1<-droplevels(subset(s,Quarter==1))
    maxw<-max(s1$west)
  
    ages<-sort(unique(subset(s1,west>0 & N>0)$Age))
    age<-ages[1]  
    plot(y=s1[s1$Age==age,'west'],x=s1[s1$Age==age,'Year'],ylab='Mean weight (kg), Q1',type='b',pch=as.character(age),ylim=c(0,maxw))
    for (age in ages[-1]) lines(y=s1[s1$Age==age,'west'],x=s1[s1$Age==age,'Year'],type='b',pch=as.character(age))
    
    maxw<-max(s1$N); age<-ages[1] 
    plot(y=s1[s1$Age==age,'N'],x=s1[s1$Age==age,'Year'],ylab='Stock numbers, Q1',type='b',pch=as.character(age),ylim=c(0,maxw))
    for (age in ages[-1]) lines(y=s1[s1$Age==age,'N'],x=s1[s1$Age==age,'Year'],type='b',pch=as.character(age))
    
  
     if (my.dev %in% c('png','wmf','pdf')) cleanup()
}
