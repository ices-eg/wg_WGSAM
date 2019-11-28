cat("\nHEJ FMSY-matrix\n")


cat("\nscenario.dir:",scenario.dir,"\n")
if (my.area=='North Sea') {
  #source(file.path(prog.path,"FMSY_matrix_scenarios.r"))
  
  first.sc<-22  # first screen for plot
  
  #a<-droplevels(subset(a,(COD<=0.4 & POK<=0.4) & SPR<0.6))
  out.file='option1'
  out.types<-c('Yield','SSB','Recruit','Fbar','belowBlim', 'belowBpa')
  plot.matrix(tit='Probability (%) of SSB below Blim',  type=out.types[5],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir)

  
  plot.matrix(tit='Yield',type=out.types[1],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir) 
  plot.matrix(tit='SSB',  type=out.types[2],spNames=sp.names[17:24],my.dev='png',out.file=out.file,scenario.dir=scenario.dir)
  
              
  table.MSY(a,out.file=out.file)
}

 