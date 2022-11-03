#belowBlimplotfile(dev=my.dev,out='abox-pel-derm')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right

 load(file =file.path(scenario.dir, "indicators.RData"))       
 
pl<-strmacro(SP,Sp,
   expr={
    ylab1<-paste("(1000 t)")
    main1<-"Demersal biomass"
    main2<-"Pelagic biomass"
    main3<-"Pelagic bio:Demersal biom"
    xlab1<-paste('F', "Sp")
    boxplot(bio.demer/1000~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main1)
    boxplot(bio.pelag/1000~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main2)
    boxplot(bio.pelag/bio.demer~SP,data=indi,xlab=xlab1,ylab=' ',main=main3)
  } )
pl(SP=COD,Sp=Cod)
pl(SP=HER,Sp=Herring)
pl(SP=SPR,Sp=Sprat)
if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-life_expec')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right

pl.life<-strmacro(SP,Sp,
   expr={
    ylab3<-"Years"
    main1<-"Community M2"
    main2<-"Community F"
    main3<-"Life expectancy"
    xlab1<-paste('F', "Sp")
    boxplot(comm.M2~SP,data=indi,xlab=xlab1,ylab=' ',main=main1)
    boxplot(comm.Fall~SP,data=indi,xlab=xlab1,ylab=' ',main=main2)
    boxplot(comm.life.expec~SP,data=indi,xlab=xlab1,ylab=ylab3,main=main3)
  } )
pl.life(SP=COD,Sp=Cod)
pl.life(SP=HER,Sp=Herring)
pl.life(SP=SPR,Sp=Sprat)
if (my.dev %in% c('png','wmf','pdf'))  dev.off()


plotfile(dev=my.dev,out='abox-LFI')
par(mfcol=c(3,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right

pl.LFI<-strmacro(SP,Sp,
   expr={
    ylab1<-"%"
    main1<-"LFI (40 cm)"
    xlab1<-paste('F', "Sp")
    boxplot(LFI*100~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main1)
    #boxplot(comm.Fall~SP,data=indi,xlab=xlab1,ylab=' ',main=main2)
    #boxplot(comm.life.expec~SP,data=indi,xlab=xlab1,ylab=ylab3,main=main3)
  } )
pl.LFI(SP=COD,Sp=Cod)
pl.LFI(SP=HER,Sp=Herring)
pl.LFI(SP=SPR,Sp=Sprat)
if (my.dev %in% c('png','wmf','pdf'))  dev.off()



plotfile(dev=my.dev,out='abox-all-sys-indi')
par(mfcol=c(7,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right

pl.indi<-strmacro(SP,Sp,
   expr={

    ylab1<-paste("(1000 t)")
    main1<-"Demersal biomass"
    main2<-"Pelagic biomass"
    main3<-"Pelagic bio:Demersal biom"
    xlab1<-paste('F', "Sp")
    boxplot(bio.demer/1000~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main1)
    boxplot(bio.pelag/1000~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main2)
    boxplot(bio.pelag/bio.demer~SP,data=indi,xlab=xlab1,ylab=' ',main=main3)

    ylab3<-"Years"
    main1<-"Community M2"
    main2<-"Community F"
    main3<-"Life expectancy"
    xlab1<-paste('F', "Sp")
    boxplot(comm.M2~SP,data=indi,xlab=xlab1,ylab=' ',main=main1)
    boxplot(comm.Fall~SP,data=indi,xlab=xlab1,ylab=' ',main=main2)
    boxplot(comm.life.expec~SP,data=indi,xlab=xlab1,ylab=ylab3,main=main3)

    ylab1<-"%"
    main1<-"LFI (40 cm)"
    xlab1<-paste('F', "Sp")
    boxplot(LFI*100~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main1)
    } )

pl.indi(SP=COD,Sp=Cod)
pl.indi(SP=HER,Sp=Herring)
pl.indi(SP=SPR,Sp=Sprat)
if (my.dev %in% c('png','wmf','pdf'))  dev.off()

  
plotfile(dev=my.dev,out='abox-sys-indi_paper')
par(mfcol=c(5,3))
par(mar=c(4,4,3,2)) #bottom, left, top, right

pl.indi<-strmacro(SP,Sp,
   expr={

    ylab1<-paste("(1000 t)")
    main1<-"Demersal biomass"
    main2<-"Pelagic biomass"
    main3<-"Pelagic bio:Demersal bio"
    xlab1<-paste('F', "Sp")
   # boxplot(bio.demer/1000~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main1)
   # boxplot(bio.pelag/1000~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main2)
    boxplot(bio.pelag/bio.demer~SP,data=indi,xlab=xlab1,ylab=' ',main=main3)

    ylab3<-"Years"
    main1<-"Community M2"
    main2<-"Community F"
    main3<-"Life expectancy"
    xlab1<-paste('F', "Sp")
    boxplot(comm.M2*100~SP,data=indi,xlab=xlab1,ylab='%',main=main1)
    boxplot(comm.Fall*100~SP,data=indi,xlab=xlab1,ylab='%',main=main2)
    boxplot(comm.life.expec~SP,data=indi,xlab=xlab1,ylab=ylab3,main=main3)

    ylab1<-"%"
    main1<-"LFI (40 cm)"
    xlab1<-paste('F', "Sp")
    boxplot(LFI*100~SP,data=indi,xlab=xlab1,ylab=ylab1,main=main1)
    } )

pl.indi(SP=COD,Sp=Cod)
pl.indi(SP=HER,Sp=Herring)
pl.indi(SP=SPR,Sp=Sprat)
if (my.dev %in% c('png','wmf','pdf'))  dev.off()

