####
#xyplot settings

#show.settings()
my.col<-c('black','red','blue','orange','darkgreen','brown','yellow')
mt<-trellis.par.get()
mt$superpose.symbol$col<-my.col
mt$superpose.symbol$pch<-1:7
mt$superpose.line$col<-my.col
mt$superpose.line$lwd<-2
trellis.par.set(mt)
#show.settings()

my.trellis.settings<-mt
rm(mt)
#####
