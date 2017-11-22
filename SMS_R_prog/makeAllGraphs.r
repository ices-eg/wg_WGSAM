makeAllGraphs<-TRUE

if (makeAllGraphs) {
  
    in.dir<-'Input'
    out.dir<-'Output'
    inOut.dir<-'Input_Output'
    outputDir<-file.path(data.path,inOut.dir)
    
    if (file.exists(outputDir)) unlink(outputDir,recursive = T)
    dir.create(outputDir,showWarnings = FALSE)
    dir.create(file.path(data.path,inOut.dir,out.dir),showWarnings = FALSE)
    dir.create(file.path(data.path,inOut.dir,in.dir),showWarnings = FALSE)
    
    
    StockSummary.dir<-file.path(data.path,inOut.dir,out.dir,'StockSummary')
    dir.create(StockSummary.dir,showWarnings = FALSE)
    source(file.path(prog.path,'plot_summary_ices_multi.r'))
    
    PartialM2.dir<-file.path(data.path,inOut.dir,out.dir,'PartialM2')
    dir.create(PartialM2.dir,showWarnings = FALSE)
    PartialM2.dir<-file.path(inOut.dir,out.dir,'PartialM2')
    source(file.path(prog.path,'part_m2.r'))
    
    whoEatsWhom.dir<-file.path(data.path,inOut.dir,out.dir,'WhoEatsWhom')
    dir.create(whoEatsWhom.dir,showWarnings = FALSE)
    whoEatsWhom.dir<-file.path(inOut.dir,out.dir,'WhoEatsWhom')
    source(file.path(prog.path,'who_eats_whom.r'))
    
    compare.dir<-file.path(data.path,inOut.dir,out.dir,'Comparisons')
    dir.create(compare.dir,showWarnings = FALSE)
    compare.dir<-file.path(inOut.dir,out.dir,'Comparisons')
    source(file.path(prog.path,'compare_runs.r'))
    source(file.path(prog.path,'compare_runs_m2.r'))
    
    otherPred.dir<-file.path(data.path,inOut.dir,in.dir,'OtherPredators')
    dir.create(otherPred.dir,showWarnings = FALSE)
    otherPred.dir<-file.path(inOut.dir,in.dir,'OtherPredators')
    source(file.path(prog.path,'Compare_runs_other_predators.r'))
    
    
    source(file.path(prog.path,'compare_runs_W_PM_M.r'))
    plotData<-function(vari='west') {
      my.dir<-file.path(data.path,inOut.dir,in.dir,vari)
      dir.create(my.dir,showWarnings = FALSE)
      my.dir<<-file.path(inOut.dir,in.dir,vari)
      do_plot_val(vari=vari)
    }
    
    plotData(vari='west') 
    plotData(vari='propmat') 
    plotData(vari='c.obs')
    plotData(vari='ration')