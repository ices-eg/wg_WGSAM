paper<-T   # graphs on file (paper=TRUE) or on screen (paper=FALSE)

if (makeAllGraphs) paper<-T

first.year.on.plot<-1974
last.year.on.plot<-2016

nonFish<-c('Fulmar','Gannet','GBB. Gull','Grey seal','Guillemot','H. porpoise','Her. Gull','Kittiwake','Puffin','Razorbill')
#select.sp<-c(7:17)


dirs<-c("NS_63-10-sep-2014", "v05-NS-2015")
labels<-c("2011-run", "2015-run")



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
    print(sp.names)
    a<- Read.other.predator(dir=file.path(root,dir),read.init.function=F)
    print(unique(a$Predator))
    a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & N>0),  select=c(Predator,Species.n, Year,Quarter, Age, N,west) ,drop=T)
    a$label<-labels[ which(dirs==dir)]
    
    if (dir==dirs[1]) a2<-a else a2<-rbind(a2,a)
}


a2$BIO<-a2$N*a2$west/1000


a2[a2$Predator %in% nonFish,'BIO']<- a2[a2$Predator %in% nonFish,'N']
a2$BIO[a2$BIO<=0]<-0

 head(a2)
a3<-aggregate(BIO~label+Predator+Year+Quarter,data=a2,sum) 
#a2<-a2[order(a2$label,a2$Predator,a2$Year,a2$Quarter),]
 
pp<-unique(a3$Predator)
               
cleanup()
for (p in pp){
  a<-droplevels(subset(a3,Predator==p))
   
  trellis.device(device = "windows", color = T, width=9, height=9,pointsize = 2,new = TRUE, retain = FALSE)
  
  filename<-file.path(paste0('Compare_OtherPred_BIO_',p,'.png'))
  if (makeAllGraphs) filename=file.path(otherPred.dir,filename)
  
  if (!paper) trellis.device(device = "windows", color = T, width=9, height=9,pointsize = 2,new = TRUE, retain = FALSE)
  if (paper)trellis.device(device='png',file=filename,width = 1000, height = 800)
  trellis.par.set(my.trellis.settings)
  
  print(xyplot( BIO~Year|paste(Predator,' Q',Quarter,sep=''),groups=label, data=a,
    type='b',lwd=2 , layout=c(2,2), ylab='stock size',
     strip = strip.custom( bg='white'),par.strip.text=list(cex=1.25, lines=1.7),
     auto.key = list(space = "bottom", points = T, lines = F,cex=1, columns = 2) ,
  
      scales = list(x = list( cex=1), y= list(cex=1),alternating = 1,
      relation='same'
      )
  ))
}

######################
if (paper) cleanup()

 a3<-aggregate(N~label+Predator+Year+Quarter+Age,data=a2,sum) 
              

for (p in pp){
  a<-droplevels(subset(a3,Predator==p))
  nn<-max(a$Age)-min(a$Age)+1 
  print(nn)
  
  if (!paper) trellis.device(device = "windows", color = T, width=9, height=9,pointsize = 2,new = TRUE, retain = FALSE)
  if (paper)trellis.device(device='png',file=file.path(paste0('Compare_OtherPred_N_',p,'.png')),width = 1000, height = 800)
  trellis.par.set(my.trellis.settings)
  
  print(xyplot( N/1000~Year |paste(Predator,' Q',Quarter,' Age:',Age,sep=''),groups=label, data=a,
    type='b',lwd=2 , layout=c(nn,4), ylab='stock size',
     strip = strip.custom( bg='white'),par.strip.text=list(cex=1.25, lines=1.7),
     auto.key = list(space = "bottom", points = T, lines = F,cex=1.5, columns = 2) ,
      scales = list(x = list( cex=1), y= list(cex=1),alternating = 1,relation='same')     # ,relation='free'
  ))
}
if (paper) cleanup()


Init.function()

