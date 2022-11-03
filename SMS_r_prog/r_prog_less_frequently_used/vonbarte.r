# cod North Sea

firstAge<-1
maxAge<-20 
nSeasons<-4
FbarAge<-c(2,4)  # expanded relative to normal

dicardLength<-40   # Knife edge selection for discard 

#################


f.season<-1
l.season<-nSeasons

#### Natural moratlity  
M<-c(0.8,0.35,0.25,rep(0.2,maxAge-firstAge))                  # annual values
M<-rep(M/nSeasons,each=nSeasons)                              # quarterly values, assume equaly split
propMat<-c(0.01,0.05,0.23,0.62,0.86,rep(1,maxAge-firstAge))   # annual values
propMat<-rep(propMat,each=nSeasons)


####  von Bertalanffy growth  parameters
K<-0.219
loo<-126
CV<-0.15  # CV of length distribution within an age

vonBertalanffy<-function(tid,loo,K,t0=0) {
  return(loo*(1-exp(-K*(tid-t0))))
}


#### length weight relation.  Fishbase North Sea Netherlands
a<-0.0068E-3
b<-3.101
lw<-function(len){a*len^b}
#########

# first try, No fishing
age<-seq(firstAge,maxAge,1/nSeasons)
age<-age[1:(length(age)-1)]
age
M<-M[1:length(age)]
propMat<-propMat[1:length(age)]

len<-vonBertalanffy(tid=age,loo=loo,K=K)
W<-lw(len)
N<-M #copy structure
N[1]<-1
for (i in 2:length(N)) N[i]<-N[i-1]*exp(-M[i])
biomass<-N*W
SSB<-biomass*propMat

nox<-2; noy<-2;
  newplot(dev='screen',nox,noy);
optAge<-age[which(biomass==max(biomass))]

plot(age,len,ylab="cm",main='Length at age')
abline(v=optAge)
plot(age,W,ylab="kg", main="Weight at age")
abline(v=optAge)
plot(age,N,ylab=" ", main="Stock number at age")
abline(v=optAge)

plot(age,biomass,ylab="kg", main="TSB and SSB at age")
lines(age,SSB)
abline(v=optAge)

#############################

# "North Sea Cod" data from Niels Madsen
mesh<-c(70,80,90,100,110,120,130,140,150)
L50<-c(21.22,24.99,28.75,32.52,36.28,40.05,43.81,47.58,51.35)
SR<-c(3.98,4.69,5.40,6.10,6.81,7.52,8.22,8.93,9.64)
selection<-data.frame(mesh=mesh,L50=L50,SR=SR)
# plot(mesh,L50); abline(lm(L50~mesh))
# plot(mesh,SR); abline(lm(SR~mesh))
L50par<-lm(L50~mesh,  data=selection)
SRpar<-lm(SR~mesh,  data=selection)
selection<-data.frame(L50a=L50par$coefficients[1],L50b=L50par$coefficients[2],SRa=SRpar$coefficients[1],SRb=SRpar$coefficients[2])

meshSelection<-function(mesh,len) {
  L50<-selection$L50a[1]+mesh*selection$L50b[1]
  SR<- selection$SRa[1]+mesh*selection$SRb[1]
  S1<-L50*log(3)/(SR/2)
  S2<-S1/L50
  SL<-1/(1+exp(S1-S2*len))
  return (list(SL,L50))
}

l<-seq(10,100)  # fish length in cm
col<-1
firstMesh<-70
a<- meshSelection(mesh=firstMesh,len=l)
plot(l,a[[1]],type='l',col=col,xlab="length (cm)",ylab="selection")
abline(h=0.5)
abline(v=a[[2]],col=col)
text(a[[2]],0.55,formatC(firstMesh,width=3,digits=3),col=col)
for (mesh in (seq(firstMesh+20,200,20))) {
  col<-col+1
  a<-meshSelection(mesh=mesh,len=l)
  lines(l,a[[1]],col=col)
  text(a[[2]],0.55,formatC(mesh,width=3,digits=3),col=col)
  abline(v=a[[2]],col=col)
}

##########################


 YPR<-function(EffortMult=1,len=len, mesh=mesh,minSize=40)
# Yield Per Recruit function using size dependent F, where F(a,length)=SL(length)*effort
# effortMult is effort multiplier
# len is length at age vector
# mesh is mesh size of trawl
# minSize is minimum landing size, Knife edge select: below means discard, above means landed
# Return values:
#  yield: landed weigth per recruir
#  discard: discarded weight per recruit
#  meanWLanded: mean weight of landed fish
#  Fbar: average F over specified ages  
 {
      Fmort<-meshSelection(mesh,len)[[1]]*EffortMult
      
      N<-Fmort # copy structure
      N[]<-0
      N[1]<-1
   
      Z<-M+Fmort
      
      Z[is.na(Z)]<-0
      C<-Z  #copy structure
      for (t in (2:length(Z))) {
        N[t]<-N[t-1]*exp(-Z[t])     # exponential decay
      }
      #plot(age,N)
      Nbar<-N*(1-exp(-Z))/Z   # average N within time step
      #lines(age,Nbar)
      C<-Nbar*Fmort       # total catch numbers
      Clanded<-C          # copy structure
      Clanded[len<minSize]<-0;        # extract landed (length>minimum size)
      mw<-sum(Clanded*W)/sum(Clanded)
      Cdiscard<-C         # copy structure
      Cdiscard[len>=minSize]<-0;

      deadM<-Nbar*M     # number dead of natural causes
      dead<-deadM+C     # total number dead

      yield<-sum(Clanded*W)    # yield weight
      discard<-sum(Cdiscard*W) # discarded weight

      # TSB and SSB in start of the first season
      TSB<-matrix(N,nrow=nSeasons)[1,]*matrix(W,nrow=nSeasons)[1,]
      SSB<-sum(TSB*matrix(propMat,nrow=nSeasons)[1,])
      TSB<-sum(TSB)
      
      C<-apply(matrix(C,nrow=nSeasons),2,sum) #sum into C at age
      dead<-apply(matrix(dead,nrow=nSeasons),2,sum) # sum total dead at age
      Z<-apply(matrix(Z,nrow=nSeasons),2,sum) #age Z
      Fmort<-Z*C/dead  # annual F at age 
      #print(Fmort)
      Fbar<-mean(Fmort[FbarAge[1]:FbarAge[2]])
      return(list(yield=yield,meanWLanded=mw,discard=discard,Fbar=Fbar,TSB=TSB,SSB=SSB))
 }

 YPR(EffortMult=0.1,len=len, mesh=90,minSize=dicardLength)


 # the next exercise is to check the model and to define the range of effort multiplier (myMult)
 myMult<-seq(0.0,0.5,0.001)
 myMesh<-seq(40,180,20)
 design<-data.frame(myMesh=rep(myMesh,length(myMult)),myMult=rep(myMult,each=length(myMesh)))
 
a<- by(design,list(design$myMesh,design$myMult),function(x) {
      YPR(EffortMult=x$myMult,len=len, mesh=x$myMesh,minSize=dicardLength)
 })
 
b<-data.frame(as.matrix(design),matrix(unlist(a),ncol=6,byrow = T))
names(b)<-list("myMesh", "myMult", "Yield" ,    "meanWLanded" ,    "discard" ,    "Fbar", "TSB","SSB"  )

#b<-b[order(b$myMesh,b$myMult),]



levelplot(Yield~myMesh*myMult, data=b,xlab="Mesh size (mm)",ylab="effort multiplier",main="Yield per recruit",contour=T)

levelplot(Yield~myMesh*Fbar, data=b,xlab="Mesh size (mm)",ylab=paste("Fbar( ",FbarAge[1],"-",FbarAge[2],")",sep=''),main="Yield per recruit",contour=T)

xyplot( Yield~Fbar|as.factor(paste (formatC(myMesh,,wid=3,flag="0"),'mm')), data=b,ylab='Yield (kg)', xlab=paste("Fbar( ",FbarAge[1],"-",FbarAge[2],")",sep=''),xlim=c(0,1))

xyplot( Yield~Fbar, data=b, xlim=c(0,1),ylab='Yield (kg) per recruit', xlab=paste("Fbar( ",FbarAge[1],"-",FbarAge[2],")",sep=''))

xyplot( discard~Fbar, data=b,  xlim=c(0,1),ylab='Discard (kg) per recruit',xlab=paste("Fbar( ",FbarAge[1],"-",FbarAge[2],")",sep=''))


xyplot( SSB~Fbar, data=b,  xlim=c(0,1),ylab='SSB (kg) per recruit',xlab=paste("Fbar( ",FbarAge[1],"-",FbarAge[2],")",sep=''))

xyplot( TSB~Fbar, data=b,  xlim=c(0,1),ylab='TSB (kg) per recruit',xlab=paste("Fbar( ",FbarAge[1],"-",FbarAge[2],")",sep=''))

xyplot( meanWLanded~Fbar, data=b, xlim=c(0,1),ylab="mean landings weight (kg)",xlab=paste("Fbar( ",FbarAge[1],"-",FbarAge[2],")",sep=''))

