compare_runs_other_predators<-function(
  dirs=c("NorthSeaKeyRun_2017", "NorthSeaKeyRun_2020"),  # directory files to be compared
  labels=c("2017 keyrun","2020 keyrun"),  # output legends
  paper=TRUE,   # graphs on file (paper=TRUE) or on screen (paper=FALSE)
  first.year.on.plot=1975,
  last.year.on.plot=2020,
  otherPred.dir=data.path,
  compare.dir=data.path,
  makeAllGraphs=FALSE,
  quarterN=1, # Quarters for N plot
  nonFish=c('Fulmar','Gannet','GBB. Gull','Grey seal','Guillemot','H. porpoise','Her. Gull','Kittiwake','Puffin','Razorbill') #biomass make no sense for non-fish
 ){
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

   head(a2)
   a3<-subset(a2,!(Predator %in% nonFish))
  a3<-aggregate(BIO~label+Predator+Year+Quarter,data=a3,sum) 

  pp<-unique(a3$Predator)
                 
  cleanup()
  
  for (p in pp){
    a<-droplevels(subset(a3,Predator==p))
     
    trellis.device(device = "windows", color = T, width=9, height=9,pointsize = 2,new = TRUE, retain = FALSE)
    
    filename<-file.path(otherPred.dir,paste0('Compare_OtherPred_',p,'_BIO.png'))

    print(filename)
    if (!paper) trellis.device(device = "windows", color = T, width=9, height=9,pointsize = 2,new = TRUE, retain = FALSE)
    if (paper) trellis.device(device='png',file=filename,width = 1000, height = 800)
    trellis.par.set(my.trellis.settings)
    
    print(xyplot( BIO~Year|paste(Predator,' Q',Quarter,sep=''),groups=label, data=a,
      type='b',lwd=2 , layout=c(2,2), ylab='Biomass',
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
   pp<-unique(a3$Predator)             
  
  for (p in pp){
    a<-droplevels(subset(a3,Predator==p & Quarter %in%  quarterN))
    nn<-max(a$Age)-min(a$Age)+1 
    ql<-length(quarterN)
    if (ql>1) my.layout<-c(nn,4) else {
      if (nn<=4) my.layout<-c(nn,1) 
      if (nn<=8) my.layout<-c(4,2) 
      if (nn<=12) my.layout<-c(4,3) else my.layout<-c(4,4)
    }
    if (!paper) trellis.device(device = "windows", color = T, width=9, height=9,pointsize = 2,new = TRUE, retain = FALSE)
    #filename<-paste0(otherPred.dir,'Compare_OtherPred_',p,'_N.png')
    filename<-file.path(otherPred.dir,paste0('Compare_OtherPred_',p,'_N.png'))
     
    if (paper) trellis.device(device='png',file=filename,width = 1000, height = 800)
    trellis.par.set(my.trellis.settings)
  
    print(xyplot( N/1000~Year |paste(Predator,' Q',Quarter,' Age:',Age,sep=''),groups=label, data=a,
      type='b',lwd=2 , layout=my.layout, ylab='stock numbers (millions)',
       strip = strip.custom( bg='white'),par.strip.text=list(cex=1.25, lines=1.7),
       auto.key = list(space = "bottom", points = T, lines = F,cex=1.5, columns = 2) ,
        scales = list(x = list( cex=1), y= list(cex=1),alternating = 1,relation='same')     # ,relation='free'
    ))
  }
  if (paper) cleanup()
  
  
  Init.function()
}  
