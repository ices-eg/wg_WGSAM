
opt.table<-function(out.file,dirs,labels,first.y=2051,last.y=2051,stochastic=F,test=F,addFbar=F,tableIndicators=F,readIndicators=F,speciesIncl=18:25,useCondensed=TRUE,altCondensed=' ',altIndi=' '){
   #####################  
  for (dir in dirs) {
    if ( file.access(file.path(data.path,dir,"op.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
  } 
  
  Init.function() # get SMS.contol object  including sp.names
  
  ref<-Read.reference.points.OP(dir=data.path) 
  Blim<-as.vector(c(rep(-1, first.VPA-1),ref[,'Blim']))
  Bpa<-as.vector(c(rep(-1, first.VPA-1),ref[,'Bpa']))
  Fpa<-as.vector(c(rep(-1, first.VPA-1),ref[,'Fpa']))
  
  OP<-read.FLOP.control(file=file.path(data.path,dirs[1],"op.dat"),n.VPA=11,n.other.pred=17,n.pred=21)
  n.years<-OP@last.year - OP@first.year.out +1     

  b<-NULL; n<-1
  for (dir in dirs) {
     if (useCondensed) load(file=file.path(data.path,dir, "condensed.RData")) else {
       load(file=file.path(data.path,dir, altCondensed[n]),verbose=T)
       print(altCondensed[n])
       if ("condensed.RData"!=altCondensed[n]) {
          condensed<-subset(aa,select=c(value,yield,CWsum,Fcomb,Fbar,SSB,TSB,recruit,belowBlim,belowBpa,Species.n, run,iteration,COD,WHG,HAD,POK,HER,NSA,SSA,NOR,SPR,PLE,SOL))
          rm(aa)
       }
     } 
     a<-data.frame(scen=labels[n],condensed)
     b<-rbind(b,a)
     n<-n+1
  } 
  b$Species<-sp.names[b$Species.n]
 print(dim(b))
  a<-aggregate(cbind(Fbar,yield,CWsum,value,SSB)~scen+Species.n+Species,mean,data=b)
  asd<-aggregate(cbind(yield,value,SSB)~scen+Species.n+Species,sd,data=b)
  hh<-dimnames(asd)[[2]]
  hh<-sub('yield', 'yieldSD',hh)
  hh<-sub('value', 'valueSD',hh)
  hh<-sub('SSB', 'SSBSD',hh)
  dimnames(asd)[[2]]<-hh
  a2<-aggregate(list(Fbarmax=b$Fbar),list(scen=b$scen,Species.n=b$Species.n,Species=b$Species),max)
  
   
  b$belowBlim<-b$belowBlim*100/n.years
  b$belowBpa<-b$belowBpa*100/n.years
  risk<-aggregate(cbind(belowBlim,belowBpa)~scen+Species.n+Species,mean,data=b)


  aa<-merge(a,a2)
  aa<-merge(aa,asd)
  aa<-merge(aa,risk)

  aa<-subset(aa,Species !='Plaice' & Species !='Sole')

 
  label<-data.frame(scen=dirs,labels)
  #aa<-merge(aa,label)
  aa$labels<-aa$scen
  aa$Species<-paste(aa$Species.n-first.VPA+1,'. ',aa$Species,sep='')
  a<-tapply(aa$Fbarmax,list(aa$Species,aa$labels),sum)
 
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_Fmax",'.html',sep='')),
             dec=rep(2,dim(a)[[2]]), width='"100%"',units='FMSY (max)')
  
  if (addFbar) {
    a2<-tapply(aa$Fbar,list(aa$Species,aa$labels),sum)

   
  xtab(a, caption='', cornername='  ',
       file=file.path(paste(out.file,"_Fbar",'.html',sep='')),
       dec=rep(2,dim(a2)[[2]]), width='"100%"',units='FMSY (bar)')
  }
  
  
  a<-tapply(aa$belowBlim ,list(aa$Species,aa$labels),mean)
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_blim",'.html',sep='')),
             dec=rep(1,dim(a)[[2]]), width='"100%"',units=' ')

  a<-tapply(aa$belowBpa ,list(aa$Species,aa$labels),mean)
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_bpa",'.html',sep='')),
             dec=rep(1,dim(a)[[2]]), width='"100%"',units=' ')
 
    a<-tapply(aa$yield ,list(aa$Species,aa$labels),sum) /1000
      All<-tapply(aa$yield ,list(aa$labels),sum) /1000
    a<-rbind(a,All)  
   xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_yield",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')
 
     a<-tapply(aa$CWsum-aa$yield ,list(aa$Species,aa$labels),sum) /1000
      All<-tapply(aa$CWsum-aa$yield ,list(aa$labels),sum) /1000
    a<-rbind(a,All)  
   xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_discard",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')

  
 
 
     a<-tapply(aa$yield ,list(aa$Species,aa$labels),sum) 
     a2<-tapply(aa$yieldSD ,list(aa$Species,aa$labels),sum)
     a<-a2/a*100  
     xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_yield_CV",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')
 
     a<-tapply(aa$value ,list(aa$Species,aa$labels),sum) /1000
      All<-tapply(aa$value ,list(aa$labels),sum) /1000
    a<-rbind(a,All)  
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_value",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')  
             
      a<-tapply(aa$SSB ,list(aa$Species,aa$labels),sum) /1000
      All<-tapply(aa$SSB ,list(aa$labels),sum) /1000
    a<-rbind(a,All)  
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_SSB",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')      
             
                 
    a<-tapply(aa$SSB ,list(aa$Species,aa$labels),sum) 
    a2<-tapply(aa$SSBSD ,list(aa$Species,aa$labels),sum) 
    a<-a2/a*100
  
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_SSB_CV",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')                                     
   
                            

if (tableIndicators) {
  
  b<-NULL; n<-1
  for (dir in dirs) {
    if (useCondensed) load(file=file.path(data.path,dir, "indicators.RData")) else {
      load(file=file.path(data.path,dir, altIndi[n]),verbose=T)
      if ("indicators.RData"!=altIndi[n]) {
        indi<-indiNew
        rm(indiNew)
      }
    } 
    a<-data.frame(scen=labels[n],indi)
    b<-rbind(b,a)
    n<-n+1
  } 
  b$labels<-b$scen

  a<-aggregate(cbind(bio.demer,bio.small,bio.pelag,bio.forage,comm.Fland,comm.Fall,comm.M,comm.M2,comm.life.expec ,LFI) ~ labels,data=b,mean)

  b<-cbind(a$comm.M2*100,a$comm.Fall*100,a$LFI*100,a$comm.life.expec ,a$bio.forage/1000,a$bio.pelag/a$bio.demer)
  b<-t(b)

  dimnames(b)[[1]]<-c("Community M2 (%)","Community F (%)","LFI (%)","life expectancy (years)","Biomass forage (kt)","Biomass ratio pelagic:demersal")
  dimnames(b)[[2]]<-as.vector(a$labels)
  
   xtab3(b, caption='', cornername='  ',
             file=file.path(paste(out.file,"_sys_Indicators",'.html',sep='')),
             dec=c(0,0,0,2,0,2), width='"100%"',units=' ')    
 }
}

if (TRUE) {    # from simulation
  useCondensed<-FALSE
  altCondensed<-c("option1.RData","option2.RData","option3.RData","option3.RData","option4.RData")
  altIndi<-c('indi1.RData','indi2.RData','indi3.RData','indi3.RData','indi4.RData')
  opt.table(out.file='option1_4_compare',first.y=2043,last.y=2043,stochastic=F, test=F,addFbar=T,tableIndicators=T,useCondensed=useCondensed,altCondensed=altCondensed,altIndi=altIndi,
            dirs=c( "HCR_1_deter_had_adjust_wide_01_HCR1_0_Rec0_Recadj_2043",
                    "HCR_1_deter_had_adjust_wide_01_HCR1_0_Rec0_Recadj_2043",
                    "HCR_1_deter_had_adjust_wide_01_HCR1_0_Rec0_Recadj_2043",
                    "HCR_1_deter_had_adjust_narrow_01_HCR1_0_Rec0_Recadj_2043",
                    "HCR_1_deter_had_adjust_narrow_01_HCR1_0_Rec0_Recadj_2043"),
            labels=c("1. SSB above Blim",
                     "2. 95% MSY with SSB above Blim",
                     "3. 95% MSY with SSB above Bpa",
                     "4. 95% MSY with SSB above Bpa, small step",
                     "5. MSY with SSB above Bpa, small step" ))
}



#  

if (F) {    # from simulation
   opt.table(out.file='det_compare_HCR12_stock',first.y=2022,last.y=2211,stochastic=T, test=F,addFbar=T,tableIndicators=T,
   dirs=c("HCR_1_deter_adjust_point_01_HCR1_0_Rec0_Recadj_2051",
          "HCR_1_deter_adjust_proposed_range_03_HCR1_0_Rec0_Recadj_2051",
          "HCR_1_deter_adjust_narrow_01_HCR1_0_Rec0_Recadj_2051", "HCR_1_deter_adjust_wide_01_HCR1_0_Rec0_Recadj_2051"),
   labels=c("1)FMSY point estimate","2) 95% MSY range", "3)narrow F range", "4) wide F range" ))
}
if (FALSE) {
  dirs<-c(
    "S1a_HCR_1_1_Rec3__atAgeW____limF_110",
    "S1b_HCR_1_1_Rec3_penBlim_atAgeW____limF_110",
    "S1c_HCR_1_1_Rec3_penBpa_atAgeW____limF_110",
    "S1d_HCR_1_1_Rec3______limF_110",
    "S1e_HCR_1_1_Rec3_penBlim_____limF_110")
  
  opt.table(out.file='det_compare_HCR12_stock',first.y=2022,last.y=2204,stochastic=T, test=F,addFbar=T,tableIndicators=T,
            dirs=dirs,
            labels=dirs)
}
