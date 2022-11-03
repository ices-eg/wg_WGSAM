
scenario.dir<-file.path('c:','mv','sms','NS_63-10-OP','OP-constantF-6_stoc_rec')
  setwd(scenario.dir)
  
  
targetFs<-expand.grid(COD=seq(0.375,0.45,0.025),
                                                       WHG=seq(0.35,0.40,0.025),
                                                       HAD=seq(0.30,0.4,0.025),
                                                       POK=seq(0.40,0.40,0.025),
                                                       HER=seq(0.30,0.40,0.025),
                                                       SAN=seq(0.25,0.35,0.025),
                                                       NOR=seq(0.20,0.35,0.025),
                                                       SPR=seq(0.35,0.40,0.025),
                                                       PLE=seq(0.35,0.35,0.025),
                                                       SOL=seq(0.35,0.35,0.025))


  dim(targetFs)

  all.out<-'HCR_condensed.dat'
  
  res<-read.table(all.out,header=TRUE)
  b<-data.frame(iter=1:dim(targetFs)[[1]],targetFs)
  condensed<-merge(res,b)
  save(condensed, file =file.path(scenario.dir, "condensed.RData"))

