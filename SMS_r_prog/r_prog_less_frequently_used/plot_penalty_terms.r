
cleanup()
OPTG
ref<-Read.reference.points()

    par(mfcol=c(4,3))
    par(mar=c(2,2,2,2))  # c(bottom, left, top, right)

    OPTG@SSBpenalty
for (s in(first.VPA:nsp)) {
  SSB<-seq(0,ref[s,'Bpa']*1.5,100)
  L50<-OPTG@SSBpenalty['SSB50',s-first.VPA+1]
  L75<-OPTG@SSBpenalty['SSB75',s-first.VPA+1]
  factor<-OPTG@SSBpenalty['factor',s-first.VPA+1]
  S1<-L50*log(3.0)/(L75-L50)

  cat(sp.names[s],"\tS1: ",S1," L50%:",round(L50)," L75%:",round(L75),'\n')
  penalty<- factor*(1-(1/(1+exp(S1-S1/L50*SSB))))
  SSB<-SSB/1000
  plot(SSB,penalty,main=sp.names[s])
  abline(v=L50/1000,lty=2,col='red')
  abline(v=ref[s,'Bpa']/1000,lty=2,col='blue')
  abline(v=ref[s,'Blim']/1000,lty=2,col='blue')
}

X11()
  par(mfcol=c(4,3))
    par(mar=c(2,2,2,2))  # c(bottom, left, top, right)

for (s in(first.VPA:nsp)) {
  SSB<-seq(0,ref[s,'Bpa']*1.5,100)/1000
  L50<-OPTG@SSB50['init',s-first.VPA+1]
  S1<-OPTG@S1['lower',s-first.VPA+1]

  cat(sp.names[s],"\tS1: ",S1," L50%:",round(L50),'\n')
  sel<-(1/(1+exp(S1-S1/L50*SSB)))
  plot(SSB,sel,main=sp.names[s],ylim=c(0,1))
  abline(v=L50,lty=2,col='red')
  abline(v=ref[s,'Bpa']/1000,lty=2,col='blue')
  abline(v=ref[s,'Blim']/1000,lty=2,col='blue')
  
  S1<-OPTG@S1['higher',s-first.VPA+1]
  cat(sp.names[s],"\tS1: ",S1," L50%:",round(L50),'\n')
  sel<-(1/(1+exp(S1-S1/L50*SSB)))
  points(SSB,sel,main=sp.names[s],col='blue')

  
}

