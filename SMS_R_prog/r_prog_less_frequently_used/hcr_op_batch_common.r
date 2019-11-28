make.OP.dat<-function(my.area='North Sea',my.last.year=2000,first.year.output=2000,do.indicators=F,stochastic.recruitment=0,recruit.adjust.CV=0) {
  SMS<-read.FLSMS.control(file='SMS.dat',dir=data.path)
  nsp<-SMS@no.species
  n.other.pred<-sum(SMS@species.info[,'predator']==2)
  n.pred<-n.other.pred+sum(SMS@species.info[,'predator']==1)
  n.vpa<-nsp-n.other.pred
  n.vpa.pred<-sum(SMS@species.info[,'predator']==1)
  #if (n.other.pred==0) n.other.pred<-1

  #OP<-read.FLOP.control(file=file.path(data.path,'OP.dat'),n.VPA=n.vpa,n.other.pred=n.other.pred,n.pred=n.pred)
  OP<-FLOP.control(
        first.year=SMS@last.year.model+1,
        last.year=SMS@last.year.model+5,
        no.species=SMS@no.species,
        no.VPA.predators=n.vpa.pred,
        no.other.predators= n.other.pred,
        species.names=SMS@species.names
    )
  
      OP@last.year<-my.last.year
      OP@first.year.out<-first.year.output

      if (do.indicators) OP@indicator<-1 else OP@indicator<-0

      y.end<-2013
      y.first<-2004
      OP@years.wsea['first-year',]<-y.first; OP@years.M['first-year',]<-y.first; OP@years.propmat['first-year',]<-y.first; OP@years.weca['first-year',]<-y.first; OP@years.ration['first-year',]<-y.first;
      OP@years.wsea['last-year',] <-y.end;   OP@years.M['last-year',] <-y.end;   OP@years.propmat['last-year',] <-y.end;   OP@years.weca['last-year',] <-y.end;   OP@years.ration['last-year',] <-y.end;

      OP@years.stock.distrib['first-year',] <-y.end;
      OP@years.stock.distrib['last-year',]  <-y.end;
      
      OP@years.F['first-year',] <-y.end;
      OP@years.F['last-year',]  <-y.end;

      OP@years.prop.landed['first-year',] <-y.end;
      OP@years.prop.landed['last-year',]  <-y.end;

      OP@years.other['first-year',] <-2008;
      OP@years.other['last-year',]  <-y.end;

      if (my.area=='North Sea') {
                                      #Cod  Whiting     Haddock    Saithe  Herring  Sandeel   Nor. pout Sprat  Plaice  Sole
 #         OP@rec.noise['lower',]<-c(  -2,      -2.0,    -2.5,     -2.5,    -2.5,      -3,      -2.5,    -2.5,     -2,   -2)
#          OP@rec.noise['upper',]<-c(   2.0,     2.0,     2.0,      2.5,       2,       2,         2,     2.5,      2.5,    2)
 
                                     #Cod  Whiting     Haddock    Saithe  Herring  n Sandeel  Ssa   Nor. pout Sprat  Plaice  Sole
 #         OP@rec.noise['lower',]<-c(  -2,      -2.0,    -2.5,     -2.5,    -2.5,      -2,     -2,    -2.5,    -2.5,     -2,   -2)
        OP@rec.noise['lower',]<- -2
 #         OP@rec.noise['upper',]<-c(   2.0,     2.0,     2.0,      2.5,       2,       2,     2 ,   2,     2.5,      2.5,    2)
        OP@rec.noise['upper',]<- 2
          
       }    # end North Sea
      if (my.area=='Baltic Sea') {
                                     #Cod    Herring   Sprat 
          OP@rec.noise['lower',]<-c(  -2,      -2.5,    -1.5 )
          OP@rec.noise['upper',]<-c(   2.0,     2.0,     2.0 )

       }    # end Baltic Sea


    OP@recruit.adjust.CV[1,]<-recruit.adjust.CV     #adjust recruitment with half of the variance (factor exp(-(CV^2)/2).  0=no adjustment, 1=do adjustment,

    OP@stochastic.recruitment[]<-stochastic.recruitment



  OP@first.year<-SMS@last.year.model+1
  OP@M2.iterations<- 4    ## Maximum M2 iterations (option M2.iterations) in case of use.Nbar=1
  OP@max.M2.sum2<-1e-6    ## convergence criteria (option max.M2.sum2) in case of use.Nbar=1

  return(list(OP=OP,SMS=SMS))
}

calc.high5.yield<-function() {
  a<-read.table(file=file.path(data.path,"summary_table_raw.out"),header=T)
  a<-subset(a,!is.na(Yield))
  y.lag<-2
  
  yrange<-seq(SMS@first.year.model+y.lag,SMS@last.year-y.lag,1)
  bb<-NULL
  for (y in yrange) {
    b<-subset(a,Year>=y+y.lag & Year<=y+y.lag)
    b<-aggregate(Yield~Species.n,data=a,mean)
    b$Year<-y
    bb<-rbind(b,bb)
  }
  
  head(bb)
  
  b<-aggregate(Yield~Species.n,data=bb,max)
  b$Species<-sp.names[b$Species.n]
  b
}