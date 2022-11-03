
if (F) {	#Will fishing top predatory species (cod and saithe) at current single species estimate of FMSY allow prey stocks above precautionary biomass reference points?
    a<-droplevels(subset(a,(COD<=0.4 & POK<=0.4) & SPR<0.6))
    out.file='option1'
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)
}

if (F) { # 3.4.2.5	Which combinations of fishing mortality will lead to biomasses of all modelled species above Blim?
    a<-droplevels(subset(a,(COD>=0.5 & POK>=0.45) & SPR<0.6))
    out.file='option2'
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)

    out.file='option2b'
    goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombBlim(a,last.sp=no.MSY.sp); if (goOn) a<-a2} ; sink()
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)
}

if (F) { # 3.4.2.6	For which fishing mortalities is close-to-maximum average yield obtained for each of the modelled species when species interactions are included in a model
     a<-droplevels(subset(a,(COD==0.5 & POK==0.45) & SPR<0.6))
    #a<-droplevels(subset(a,(COD==0.5 & POK==0.45) ))

    out.file='option3'
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)

    out.file='option3a'
    goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombMSY.one.left(a); if (goOn) a<-a2}; sink()
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)


    rm(a)
    load(file =file.path(scenario.dir, "a.RData"))
    a<-droplevels(subset(a,SPR<0.6))
    out.file='option3c'
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)

    out.file='option3d'
    goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombMSY.one.left(a); if (goOn) a<-a2}; sink()
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)

       rm(a)
    load(file =file.path(scenario.dir, "a.RData"))
    a<-droplevels(subset(a,COD==0.5 & POK==0.45 & WHG==0.3 & SPR<0.6))
    out.file='option3e'
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)

    out.file='option3f'
    goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombMSY.one.left(a); if (goOn) a<-a2}; sink()
    plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
    table.MSY(a,out.file=out.file)


}


if (scenario=="Baltic_Fixed-F_stoc_rec") {
  out.file='no1'
  table.MSY(a,out.file=out.file)
  out.file='final'
  goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombMSY.one.left(a); if (goOn) a<-a2}; sink()
  out.file='no4'
  table.MSY(a,out.file=out.file)


  #start with a "fresh" a
  a<-droplevels(subset(a,COD==0.48))
    out.file='no2'
  table.MSY(a,out.file=out.file)
  out.file='final2'
  goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombMSY.one.left(a); if (goOn) a<-a2}; sink()
      out.file='no3'
  table.MSY(a,out.file=out.file)
}

plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
plot.matrix(tit='Recruitment',  type='Recruit',spNames=sp.names[17:24],my.dev='png',out.file=out.file)
plot.matrix(tit='Realized Fbar',  type='Fbar',spNames=sp.names[17:24],my.dev='png',out.file=out.file)



a<-subset(a,COD==0.5 & WHG==0.3 & HAD==0.35 & POK==0.45 & HER==0.55 & SAN==0.55 & NOR==0.6 & SPR==0.55)
out.file='no2'
table.MSY(a,out.file=out.file)

if (F) {
   # find MSY for predator only (saithe)
   aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
   dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
   round(aa)
   a<-droplevels(subset(a,(POK>0.39 & POK<0.41)) )
   aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
   dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
   round(aa)

  out.file='no2'
  plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  table.MSY(a,out.file=out.file)


  out.file='no3'
  goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombBlim(a,last.sp=no.pred); if (goOn) a<-a2} ; sink()
  plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  table.MSY(a,out.file=out.file)



  out.file='no4'
  goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep='')));  while (goOn) {a2<-reduceCombBlim(a,last.sp=no.MSY.sp); if (goOn) a<-a2} ; sink()
  plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  table.MSY(a,out.file=out.file)


  out.file='no5'
  goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep=''))); while (goOn) {a2<-reduceCombMSY(a); if (goOn) a<-a2}; sink()
  plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  table.MSY(a,out.file=out.file)


  out.file='no6'
  goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep=''))); while (goOn) {a2<-reduceCombBpa(a); if (goOn) a<-a2}; sink()
  plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  table.MSY(a,out.file=out.file)

  out.file='no7'
  goOn<-T; sink(file.path(scenario.dir,paste(out.file,'_','reductions','.out',sep=''))); while (goOn) {a2<-reduceCombMSYfinal(a); if (goOn) a<-a2}; sink()
  plot.matrix(tit='Yield',type=c('Yield','SSB')[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  plot.matrix(tit='SSB',  type=c('Yield','SSB')[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file)
  table.MSY(a,out.file=out.file)
}




