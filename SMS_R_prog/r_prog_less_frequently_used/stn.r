a<-1.14
b<-4.399E-7
cPar<-2.903E-1
Temp<- 16.369
X11()
stn<-function(SSB) {
  return(a*SSB*exp(-b*SSB+cPar*Temp))
}

SSB<-(0:50)*10^5

plot(SSB,stn(SSB))

ssb<-1.103E6
Rec<-stn(ssb)
ssb
Rec
Rec/1E6

