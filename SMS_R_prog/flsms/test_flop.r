
##  test   #########################


OPtrigger<-new("FLOPtrigger.control")
print(OPtrigger)


OPtrigger<-FLOPtrigger.control(
        first.year=2008,
        last.year=2015,
        first.VPA=17,
        no.species=10    )
print(OPtrigger)

OPtrigger<-read.FLOPtrigger.control(file=file.path(data.path,'OP_trigger.dat'),n.VPA=10,n.other.pred=16)
print(OPtrigger)

write.FLOPtrigger.control(OPtrigger,file='a.dat',nice=T)

##
OP<-new("FLOP.control")
print(OP)


OP<-FLOP.control(
        first.year=2008,
        last.year=2015,
        no.species=3,
        no.VPA.predators=1,
        no.other.predators=1,
        species.names=c('Birds','Cod','Herring')
    )
print(OP)

 OP<-read.FLOP.control(file=file.path(data.path,'OP.dat'),n.VPA=11,n.other.pred=17,n.pred=21)

OP<-FLOP.control(
        first.year=2008,
        last.year=2011,
        no.species=4,
        no.VPA.predators=1,
        no.other.predators=1,
        species.names=c('Birds','Cod','Herring','Sprat')
    )
print(OP)

write.FLOP.control(OP,file=file.path(data.path,'a.dat'),nice=T)


SMS<-read.FLSMS.control()
nsp<-SMS@no.species
n.other.pred<-sum(SMS@species.info[,'predator']==2)
n.vpa.pred<-sum(SMS@species.info[,'predator']==1)
n.vpa=nsp-n.other.pred
n.pred<-n.other.pred+n.vpa.pred
OP<-read.FLOP.control(file=file.path(data.path,'OP.dat'),n.VPA=n.vpa,n.other.pred=n.other.pred,n.pred=n.pred)
print(OP)
 
write.FLOP.control(OP,file='a.dat',nice=T)

###

SMS<-read.FLSMS.control()
nsp<-SMS@no.species
n.other.pred<-sum(SMS@species.info[,'predator']==2)
n.vpa.pred<-sum(SMS@species.info[,'predator']==1)
n.vpa=nsp-n.other.pred
n.pred<-n.other.pred+n.vpa.pred

OPtrigger<-read.FLOPtrigger.control(file=file.path(data.path,'OP_trigger.dat'),n.VPA=n.vpa,n.other.pred=n.other.pred)
print(OPtrigger)

write.FLOPtrigger.control(OPtrigger,file='a.dat',nice=T)


## NS initialisation
SMS<-read.FLSMS.control()


OP<-FLOP.control(
        first.year=1974,
        last.year=2016,
        no.species=SMS@no.species,
        no.VPA.predators=sum(SMS@species.info[,'predator']==1),
        no.other.predators=sum(SMS@species.info[,'predator']==2),
        species.names=SMS@species.names 
    )

write.FLOP.control(OP,file='OP_new.dat',nice=T)

