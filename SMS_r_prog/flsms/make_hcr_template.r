  SMS.dat<-read.FLSMS.control(file='SMS.dat')
    HCR<-FLSMS.predict.control(
        first.prediction.year=2014,
        last.prediction.year=2015,
        no.species=28,
        no.other.predators=17,
        species.names=sp.names
    )


       write.FLSMS.predict.control(HCR,SMS=SMS.dat,file='aHCR.dat')