paper<-T   # graphs on file (paper=TRUE) or on screen (paper=FALSE)

if (makeAllGraphs) paper<-TRUE

first.year.on.plot<-1974
last.year.on.plot<-2016

vari<-"west"  # value to be plotted: M1 M2 M F Z N propmat west weca
vari<-'propmat'

maxAge<-5     # age > maxage are put in a seperate plot 

nonFish<-c('Fulmar','Gannet','GBB. Gull','Grey seal','Guillemot','H. porpoise','Her. Gull','Kittiwake','Puffin','Razorbill')

dirs<-c("NS_key-2014-ver17","NS_key-2017-ver03")
labels<-c("2014-run", "2017-run")

if (makeAllGraphs) {
  dirs<-c("NS_key-2014-ver17","NS_key-2017-ver05")
  labels<-c("2014-run","2017-run")
}

for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 

Init.function() # get SMS.contol object  including sp.names

a2<-NULL
for (dir in dirs) {
    Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
    print(dir)
    Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
    a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
    a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot ))
   a$label<-labels[ which(dirs==dir)]
    
    if (dir==dirs[1]) a2<-a else a2<-rbind(a2,a)
}

a2<-droplevels(subset(a2,!(Species %in% nonFish)))

isPredVPA<-data.frame(Species=names(SMS.control@species.info[,'predator']),
                   isPred=ifelse(SMS.control@species.info[,'predator']>=1,TRUE,FALSE),
                   isVPA=ifelse(SMS.control@species.info[,'predator']!=2,TRUE,FALSE))
a2<-merge(x=a2,y=isPredVPA,all.x=TRUE)
head(a2)

do_plot_val<-function(vari='M1') {
  ######################
  if (vari=="M1") a2$value<-a2$M1
  if (vari=="M2") a2$value<-a2$M2
  if (vari=="M") a2$value<-a2$M
  if (vari=="F") a2$value<-a2$F
  if (vari=="N") a2$value<-a2$N/1000
  if (vari=="c.obs") a2$value<-a2$C.obs/1000
  if (vari=="west") a2$value<-a2$west
  if (vari=="weca") a2$value<-a2$weca
  if (vari=="propmat") a2$value<-a2$propmat
  if (vari=="ration") a2$value<-a2$ration
  if (vari=="Z") a2$value<-a2$Z
  
  formatC(2,digits=0,flag='0',width=2)
  
  a3<-aggregate(value~label+Species+Year+Quarter+Age+isVPA+isPred,data=a2,sum) 
  if (vari %in% c("M1","M2","M","F","c.obs","weca",'propmat','Z')) a3<-subset(a3,isVPA)
  if (vari %in% c("ration")) a3<-subset(a3,isPred)
  
  pp<-unique(a3$Species)
  
  if (paper) cleanup()
  
  for (p in pp){
    a<-droplevels(subset(a3,Species==p))
    a<-tapply(a$value,list(a$label,a$Species,a$Year,a$Quarter,a$Age),sum) # to get the same number of quarter in all ages
    a<-arr2dfny(a)
    colnames(a)<-c('label','Species','Year','Quarter','Age','value')
    a$Year<-as.numeric(as.character(a$Year))
    a$Age<-as.numeric(as.character(a$Age))
  
    a$yo<-ifelse(a$Age<=maxAge,'young','older')
    
    if (length(unique(a$label))>1) for (ages in c('young','older')) {
      b<-droplevels(subset(a,yo==ages))  
      if (dim(b)[[1]]>0 ) { 
        nn<-max(b$Age)-min(b$Age)+1 
        print(paste(p,ages))
        filename=file.path(paste0('Compare_',vari,'_',p,'_',ages,'.png'))
        if (makeAllGraphs) filename=file.path(my.dir,filename)
      
        if (!paper) trellis.device(device = "windows", color = T, width=9, height=9,pointsize = 2,new = TRUE, retain = FALSE)
        if (paper)trellis.device(device='png',file=filename,width = 1000, height = 800)
        trellis.par.set(my.trellis.settings)
        
        print(xyplot( value~Year |paste(Species,' Q',Quarter,' Age:',formatC(Age,width=2,digits=0,format='f'),sep=''),groups=label, data=b,
          type='b',lwd=2 , layout=c(nn,4), ylab=vari,
           strip = strip.custom( bg='white'),par.strip.text=list(cex=1.25, lines=1.7),
           auto.key = list(space = "bottom", points = T, lines = F,cex=1.5, columns = 2) ,
            scales = list(x = list( cex=1), y= list(cex=1),alternating = 1,relation='same')     # ,relation='free'
        ))
    }}
  }
  if (paper) cleanup()


  Init.function()
}

#do_plot_val(vari='west') 

