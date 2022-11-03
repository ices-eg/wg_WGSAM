
SMS<-read.FLSMS.control()
sum(SMS@species.info[,'predator']==1)

OP<-FLOP.control(
  first.year=1974,
  last.year=2019,
  no.species=SMS@no.species,
  no.VPA.predators=sum(SMS@species.info[,'predator']==1),
  no.other.predators=sum(SMS@species.info[,'predator']==2),
  species.names=SMS@species.names 
)
write.FLOP.control(OP,file='op_new.dat',nice=T)
##
