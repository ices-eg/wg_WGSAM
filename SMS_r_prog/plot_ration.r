

Init.function()

dat<-Read.summary.data(extend=F,read.init.function=F)
unique(dat$Species)

#dat<-subset(dat,Species %in% c("Cod","Whiting","Haddock","Saithe","Mackerel","W.horse mac","N.horse mac","R. radiata","G. gurnards"))
head(dat)

dat$dayli<-dat$ration/dat$west/91*100; #dayly ration i % of body mass
r<-subset(dat,select=c(Species,Quarter,west,dayli))
r<-unique(r)
subset(r,Species=="N.horse mac")
subset(r,Species=="W.horse mac")


print(xyplot(dayli~west|
               Species,groups=Quarter,data=r,type='a',scales = "free",xlab='Mean weight (kg)',ylab='Dayly Ration (% of body mass)',lwd=2,
             auto.key =   list(space = "right", points = FALSE, lines = TRUE)))

by(r,r$Species,function(x) {
  #trellis.device(device='png',file=file.path(OutputDir,paste0('CatchN_',x[1,'species'],'.png')),width = 1000, height = 800)
  print(xyplot(dayli~west|
                 Species,groups=Quarter,data=x,type='a',scales = "free",xlab='Mean weight (kg)',ylab='Dayly Ration (% of body mass)',lwd=2,
               auto.key =   list(space = "right", points = FALSE, lines = TRUE)))
 # cleanup()  
})
