
referencePoints<-function(sp.no=1,refYear=1999,plotGraphYPR=F,plotGraphSSBrec=F,PlotMaxF=2,do.fixed.weight=NA,do.fixed.F=NA) {

  a<-Read.summary.data()


  a<-subset(a,Species.n ==sp.no & Year %in% refYear)
  lll<- list(a$Age,a$Quarter)

  M1<-tapply(a$M1,lll,mean)
  M2<-tapply(a$M2,lll,mean)
  M<-tapply(a$M,lll,mean)
  if (SMS.control@VPA.mode>0) M<-M1+M2

  west<-tapply(a$west,lll,mean)
  Mwest<-mean(west)
  weca<-tapply(a$weca,lll,mean)
  propmat<-tapply(a$propmat,lll,mean)
  Nass<-tapply(a$N,lll,mean)
  Fmort<-tapply(a$F,lll,mean)

  if ( (!is.na(do.fixed.weight)) |  (!is.na(do.fixed.F)) ) {

      if (!is.na(do.fixed.weight)) {
        a<-Read.summary.data()
        a<-subset(a,Species.n ==sp.no & Year %in% do.fixed.weight)
        lll<- list(a$Age,a$Quarter)
        west<-tapply(a$west,lll,mean)
        Mwest<-mean(west)
        weca<-tapply(a$weca,lll,mean)
       }
      if (!is.na(do.fixed.F)) {
        a<-Read.summary.data()
        a<-subset(a,Species.n ==sp.no & Year %in% do.fixed.F)
        lll<- list(a$Age,a$Quarter)

        Fmort<-tapply(a$F,lll,mean)
       }
  }
  f.season<-1
  l.season<-SMS.control@last.season
  rec.season<-SMS.control@rec.season

  if (l.season>1)  Fbar<-mean(apply(Fmort[as.character(SMS.control@avg.F.ages[sp.no,"first-age"]:SMS.control@avg.F.ages[sp.no,"last-age"]),],1,sum))
  if (l.season==1) Fbar<-mean(Fmort[as.character(SMS.control@avg.F.ages[sp.no,"first-age"]:SMS.control@avg.F.ages[sp.no,"last-age"]),])


  f.age<-1   # index, (not real age)
  l.age<- SMS.control@species.info[sp.no,"last-age"]-SMS.control@first.age+1
  plus.group<- SMS.control@species.info[sp.no,"+group"]
  #plus.group<-0

        
   #read SSB/R parameters
   recPar<-Read.SSB.Rec.data()
   recPar<-subset(recPar,Species.n==sp.no)
   model<-recPar[1,'model']
   alfa<-recPar[1,'alfa']
   beta<-recPar[1,'beta'] 

   model.name<-c('Ricker','Bev. & Holt','Geom. mean','Hockey stick')
   
  YPR<-function(Fmult=1){
      N<-M # copy structure
      N[]<-0
      N[1,SMS.control@rec.season]<-1
      Z<-M+Fmult*Fmort
      Z[is.na(Z)]<-0
      C<-Z  #copy structure
      for (a in (f.age:(l.age-1))){                                    
        for (q in (f.season:l.season)) {
          if (q<l.season & l.season>1) {if (!(q<rec.season & a==f.age)) N[a,q+1]<-N[a,q]*exp(-Z[a,q])}
          else N[a+1,f.season]<-N[a,l.season]*exp(-Z[a,l.season])
        }
      }
      if (l.season>1) for (q in (f.season:(l.season-1))) N[l.age,q+1]<-N[l.age,q]*exp(-Z[l.age,q])

      if (plus.group==1) {
        if (l.season>1) {
          NN<-N[l.age,]
          for (y in (1:(3*SMS.control@species.info[sp.no,"last-age"]))) {
            NN[f.season]<-NN[l.season]*exp(-Z[l.age,l.season])
            if (l.season>1) for (q in (f.season:(l.season-1))) NN[q+1]<-NN[q]*exp(-Z[l.age,q])
            N[l.age,]<- N[l.age,]+NN
          }
        } else {  # annual data
            N[l.age,1]<- N[l.age,1]+N[l.age,1]*(-exp(-Z[l.age,1]))/(exp(-Z[l.age,1])-1)    # plusgroup N
        }
      }
      SSB<-sum(N[,1]*west[,1]*propmat[,1],na.rm=T) 
      yield<-sum(Fmort*Fmult/Z*N*(1-exp(-Z))*weca,na.rm=T)   
      list(yield=yield,SSB=SSB)
  }

    SSBrec<-function(x) {
            if (model==1) y<-alfa*x*exp(-beta*x)      # Ricker
       else if (model==2) y<-alfa*x/(1+x*beta)        # Beveton & Holt
       else if (model==3) y<-exp(alfa)                # geometric mean
      # else if (model==100)  y<-exp(alfa)*min(x,beta)
       else if (model==100)  {
          gamma<-1
          SS<-beta  # inflextion point
          bet<-exp(alfa)/2  # slope divided by 2
          y<-bet*(x+sqrt( SS^2+gamma^2/4)- sqrt( (x-SS)^2+gamma^2/4))
       }
       y
    }
          
    #Equilibrium SSB from SPR (Spawner per Recruit)   
    EquiSSBrec<-function(x) {     
            if (model==1) y<- -log(1/(alfa*x))/beta    # Ricker
       else if (model==2) y<-alfa*x/(1+x*beta)         # Beverton & Holt IKKE ÆNDRET  fejl !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       else if (model==3) y<-exp(alfa)                 # geometric mean   
       #else if (model==100)  y<-exp(alfa)*min(x,beta)  # Hockey stick IKKE ÆNDRET  fejl !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       else if (model==100)  {
          gamma<-1
          SS<-beta  # inflextion point
          K<-sqrt(SS^2+gamma^2/4)
          bet<-exp(alfa)/2  # slope divided by 2
          lambda<-x
          y<-(2*K /(lambda*bet)-2*SS-2*K) / (1/(lambda^2*bet^2)- 2/(lambda*bet))
       }
      y     #return value
    } 
 
  YPRSSB<-function(Fmult=1){      
      res<-YPR(Fmult=Fmult)  
      SSBperR<-res$SSB       # Spawner per recruit
      SSBe<-EquiSSBrec(SSBperR) # equlibrium SSB per recruit
      RECe<- SSBe/SSBperR       #
      #RECe<-SSBrec(SSBe)
      cat("Fmult: ",Fmult," F:",Fbar*Fmult," Spawner per recruit: ",SSBperR,"  Equilibrium SSB: ",SSBe/1000," Equilibrium R: ",RECe/1E6, "  Fcrash:", (1/SSBperR>2*exp(alfa)/2),"\n")

      if (1/SSBperR>2*exp(alfa)/2 & T) {
        yield<-0 
        SSBe<-0
       }
       else yield<-res$yield*RECe
      list(yield=yield,SSB=SSBe)
  }



  # Fmax
  f <- function (x) {res<- -YPR(Fmult=x)$yield}
  xmin <- optimize(f, c(0, 10), tol = 0.000001)
  Fmax<-xmin$minimum*Fbar
  ref<-YPR(xmin$minimum)
  SSBmax<-ref$SSB
  Ymax<-ref$yield

  # F35%SPR
  SSB0<-YPR(Fmult=0)$SSB
  SSB35<-SSB0*0.35
  f <- function (x) {res<- abs(SSB35-YPR(Fmult=x)$SSB)}
  xmin <- optimize(f, c(0, 10), tol = 0.0001)
  F35SPR<-xmin$minimum*Fbar
  ref<-YPR(xmin$minimum)
  Y35SPR<-ref$yield


  # F0.1
  # FAO technical manual 393
  f <- function (x) {res<- YPR(Fmult=x)$yield-SSB0*0.1*x*Fbar}
  xmin <- optimize(f, c(0, 10), tol = 0.0001,maximum=T)
  F01<-xmin$maximum*Fbar
  ref<-YPR(xmin$maximum)
  SSB01<-ref$SSB
  Y01<-ref$yield
  
  if (T) {
    # FMSY
    f <- function (x) {res<- YPRSSB(Fmult=x)[["yield"]]}
    xmin <- optimize(f, c(0, 10), tol = 0.0001,maximum=T)
    FMSY<-xmin$maximum*Fbar
    ref<-YPR(xmin$maximum)
    SSBMSY<-ref$SSB
    YMSY<-ref$yield
  }
    
  # plot
  if (plotGraphYPR) {
    mult<-seq(0,PlotMaxF/Fbar,PlotMaxF/Fbar/100)
    SSB<-mult
    yield<-mult
    fishMort<-mult*Fbar 
    for (i in (1:length(mult))) {
       res<-YPR(mult[i])
       SSB[i]<-res$SSB
       yield[i]<-res$yield
    }

    fac1<-1000
    par(mar=c(4.2,4,3,5)+.1)   # c(bottom, left, top, right)
    plot(fishMort,yield*fac1,xlab=paste('Fishing mortality, ages (',SMS.control@avg.F.ages[sp.no,"first-age"],'-',SMS.control@avg.F.ages[sp.no,"last-age"],')',sep=''),
        ylab='Yield (gram)',lty=4,type='l',lwd=2,main='Yield per recruit')
    abline(v=Fmax,col='red',lwd=2)
    text(Fmax,max(yield)/2*fac1,label='Fmax',col='red',pos=1)
    abline(v=F35SPR,col='blue',lwd=2)
    text(F35SPR,max(yield)/1.7*fac1,label='F35%SPR',col='blue',pos=1)
    abline(v=F01,col='green',lwd=2)
    text(F01,max(yield)/1.5*fac1,label='F0.1',col='green',pos=1)
    #abline(0,SSB0*0.1)

    par(new=T)
    plot(fishMort,SSB*fac1,axes=F,xlab='',ylab='',lty=1,type='l',lwd=2)
    axis(side=4)
    mtext(side=4,line=3.0,"SSB (gram)")
  }
  
  if (plotGraphSSBrec) {
    mult<-seq(0,PlotMaxF/Fbar,PlotMaxF/Fbar/100)
    SSB<-mult
    yield<-mult
    fishMort<-mult*Fbar
    ll<-length(mult)
    for (i in (1:ll)) {
       res<-YPRSSB(mult[i])
       SSB[i]<-res$SSB
       yield[i]<-res$yield
       #cat("i:",i,"out of:",ll,"\n")
    }
    fac2<-0.001
    #par(mar=c(5,4,4,5)+.1)
    plot(fishMort,yield*fac2,xlab=paste('Fishing mortality, ages (',SMS.control@avg.F.ages[sp.no,"first-age"],'-',SMS.control@avg.F.ages[sp.no,"last-age"],')',sep=''),
        ylab='Yield (1000 t)',lty=4,type='l',lwd=2, main='MSY')
    if (F) {
    abline(v=Fmax,col='red')
    text(Fmax,max(yield)/2*fac2,label='Fmax',col='red',pos=1)

    abline(v=F35SPR,col='blue',lwd=2)
    text(F35SPR,max(yield*fac2)/1.7,label='F35%SPR',col='blue',pos=1)
    abline(v=F01,col='green',lwd=2)
    text(F01,max(yield*fac2)/1.5,label='F0.1',col='green',pos=1)
    #abline(0,SSB0*0.1)
    
    }
    abline(v=FMSY,col='brown',lwd=2)
    text(FMSY,max(yield*fac2)/1.3,label='FMSY',col='brown',pos=1)

    par(new=T)
    plot(fishMort,SSB*fac2,axes=F,xlab='',ylab='',lty=1,type='l',lwd=2)
    axis(side=4)
    mtext(side=4,line=3.0,"SSB (1000 t)")
  }
  
  list(Fbar=Fbar,Fmax=Fmax,SSBmax=SSBmax,Ymax=Ymax,F01=F01,SSB01=SSB01,Y01=Y01,
       F35SPR=F35SPR,SSB35SPR=SSB35,Y35SPR=Y35SPR,FMSY=FMSY,SSBMSY=SSBMSY,YMSY=YMSY)
}
  
  cleanup()
  nox<-1; noy<-2; 
  newplot(dev='screen',nox,noy)
   
 sp.no<-1
  a<-referencePoints(sp.no=sp.no,refYear=seq(2010,2012),plotGraphYPR=T,plotGraphSSBrec=T,PlotMaxF=2)
  print(unlist(a))
  write.csv(a,file=file.path(data.path,"reference_points.csv"))

if (F) {
  a<-unlist(referencePoints(sp.no=sp.no,refYear=2000,plotGraphYPR=F))
  nYear<-SMS.control@last.year-SMS.control@first.year+1
  result<-matrix(NA,nrow=nYear,ncol=length(a),dimnames=list(as.character(SMS.control@first.year:SMS.control@last.year),names(a)))

  for (y in (SMS.control@first.year:(SMS.control@last.year-1))) {
    #result[as.character(y),]<- unlist(referencePoints(sp.no=sp.no,refYear=c(y-1,y,y+1),plotGraphYPR=F,plotGraphSSBrec=F))
    #result[as.character(y),]<- unlist(referencePoints(sp.no=sp.no,refYear=c(y-1,y,y+1),plotGraphYPR=F,plotGraphSSBrec=F,do.fixed.weight=c(2010,2011,2012)))
    result[as.character(y),]<- unlist(referencePoints(sp.no=sp.no,refYear=c(y-1,y,y+1),plotGraphYPR=F,plotGraphSSBrec=F,do.fixed.F=c(2010,2011,2012)))

  }
  a<-as.data.frame(result)
  a$Year<-as.numeric(dimnames(result)[[1]])

  nline<-4
  details<-Rows(trellis.par.get("superpose.line"),c(1:nline))
  details$lty<-c(1:nline)
  details$lwd<-rep(2,nline)
  X11()
  print(  xyplot(Fmax +F35SPR+F01+FMSY~ Year,data=a,type='b',ylab="F", lty= c(1:nline),lwd=rep(1.5,nline), pch=c(1:nline),
         key = list(lines =details, points=list(pch=c(1:nline),col=details$col),
               columns = nline,  text = list(lab = c("Fmax", "F35SPR","F01","FMSY")),
               title = paste(SMS.control@species.names[sp.no],", F reference points",sep='') )))
  X11()
  print(  xyplot(Fmax~ Year,data=a,type='b',ylab="Fmax"))
 
  X11()
  print(  xyplot(F01~ Year,data=a,type='b',ylab="F0.1"))
 
  X11()
  par(mar=c(5,4,4,5)+.1)
  plot(a$Year,a$F01,ylab='F0.1',xlab='Year',type='b',lty=1,lwd=2,col=1,ylim=c(0.17,0.24))
   legend(x='topleft', legend=c('F0.1','Yield'),
         pch=c(1,2),lty=c(1,2),col=c(1,4),lwd=rep(2,2))  
    par(new=T)
    plot(a$Year,a$Y01*1000,axes=F,xlab='',type='b',ylab='',lty=2,lwd=2,col=4,pch=2)
    axis(side=4)
    mtext(side=4,line=3.0,"Yield per recruit (gram)")

 
  X11()
  print(  xyplot(SSBmax +SSB35SPR+SSB01+SSBMSY~ Year,data=a,type='b',ylab="SSB/R", lty= c(1:nline),lwd=rep(1.5,nline), pch=c(1:nline),
         key = list(lines =details, points=list(pch=c(1:nline),col=details$col),
               columns = nline,  text = list(lab = c("SSBmax", "SSB35SPR","SSB01","SSBMSY")),
               title = paste(SMS.control@species.names[sp.no],", SSB reference points",sep='') )))
  X11()             
  print(  xyplot(Ymax +Y35SPR+Y01+YMSY~ Year,data=a,type='b',ylab="Yield/R", lty= c(1:nline),lwd=rep(1.5,nline), pch=c(1:nline),
         key = list(lines =details, points=list(pch=c(1:nline),col=details$col),
               columns = nline,  text = list(lab = c("YieldMax", "Yield35SPR","Yield01","YieldMSY")),
               title = paste(SMS.control@species.names[sp.no],", Yield reference points",sep='') )))
               
 }
