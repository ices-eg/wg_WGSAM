## prog ini 1
# read data from ASCII file and do some graphical initialisations
# you have to do this after every new SMS run
#################################################################

stom<-Read.stomach.data()

a<-droplevels(subset(stom,Predator %in% c('Grey seal','H. porpoise')))

b<-tapply(a$stom.input,list(Predator=a$Predator,Year=a$Year,PredatorSize=a$Predator.length,Prey=a$Prey,
   PreySize=a$Prey.length.class,Quarter=a$Quarter.no),sum)
ftable(round(b*100,0.01))


