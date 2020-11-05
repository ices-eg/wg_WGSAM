
opt.table<-function(out.file,dirs,labels,first.y=2051,last.y=2051,stochastic=F,test=F,addFbar=F,tableIndicators=T,relativ=F){
   #####################  
  for (dir in dirs) {
    if ( file.access(file.path(data.path,dir,"op.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
  } 
  
  Init.function() # get SMS.contol object  including sp.names
  
  ref<-Read.reference.points.OP(dir=data.path) 
  Blim<-as.vector(c(rep(-1, first.VPA-1),ref[,'Blim']))
  Bpa<-as.vector(c(rep(-1, first.VPA-1),ref[,'Bpa']))
  Fpa<-as.vector(c(rep(-1, first.VPA-1),ref[,'Fpa']))
  
  
  
  bb<-NULL
  for (dir in dirs) {
    a<-Read.OP.rep(dir=file.path(data.path,dir)) 
    a<-data.frame(scen=dir,objFunc=a$objFunc,npar=a$npar,maxGrad=a$maxGrad,Penalty=a$Penalty)
    bb<-rbind(bb,a)  
  } 
  bb<-data.frame(labels=labels,bb)
  print(bb)
  write.table(bb,file=file.path(paste(out.file,"_perform",'.out',sep='')),quote=F,row.names=F)
  
  
  b<-NULL
  for (dir in dirs) {
    a<-Read.OP.condensed(dir=file.path(data.path,dir)) 
    a<-droplevels(subset(a,Year>=first.y & Year<=last.y & Species !='Plaice' & Species !='Sole'))
    a<-data.frame(scen=dir,a)
    b<-rbind(b,a)
    
  } 
  
  
  
  
  b$belowBlim<-ifelse(b$SSB<Blim[b$Species.n],1,0)
  b$belowBpa<-ifelse(b$SSB<Bpa[b$Species.n],1,0)
  b$aboveFpa<-ifelse(b$Fbar>Fpa[b$Species.n],1,0)
 
  minY<-min(b$Year)
  maxY<-max(b$Year)
  ny<-maxY-minY+1
  
  
  a<-aggregate(cbind(Fbar,yield,CWsum,value,SSB)~scen+Species.n+Species,mean,data=b)
  asd<-aggregate(cbind(yield,value,SSB)~scen+Species.n+Species,sd,data=b)
  hh<-dimnames(asd)[[2]]
  hh<-sub('yield', 'yieldSD',hh)
  hh<-sub('value', 'valueSD',hh)
  hh<-sub('SSB', 'SSBSD',hh)
  dimnames(asd)[[2]]<-hh
  a2<-aggregate(list(Fbarmax=b$Fbar),list(scen=b$scen,Species.n=b$Species.n,Species=b$Species),max)
  
  
  risk<-aggregate(cbind(belowBlim,belowBpa,aboveFpa)~scen+Species.n+Species,sum,data=b)
  risk$belowBlim<-risk$belowBlim*100/ny
  risk$belowBpa<-risk$belowBpa*100/ny
  risk$aboveFpa<-risk$aboveFpa*100/ny

  aa<-merge(a,a2)
  aa<-merge(aa,asd)
  aa<-merge(aa,risk)

 
  label<-data.frame(scen=dirs,labels)
  aa<-merge(aa,label)
  aa$lab1<-substr(aa$labels,1,1) 
  aa$Species<-paste(aa$Species.n-first.VPA+1,'. ',aa$Species,sep='')
  a<-tapply(aa$Fbarmax,list(aa$Species,aa$labels),sum)
  if (addFbar) {
    a2<-tapply(aa$Fbar,list(aa$Species,aa$labels),sum)
    a<-cbind(a,a2)
  }
 
 
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_F",'.html',sep='')),
             dec=rep(2,dim(a)[[2]]), width='"100%"',units=ifelse (addFbar, c(rep('FMSY',length(dirs)),rep('Fbar',length(dirs))), rep('FMSY',length(dirs))))
  
  if (relativ) {
    for (i in (2:dim(a)[[2]])) a[,i]<-a[,i]/a[,1]*100
    xtab(a, caption='', cornername='  ',
         file=file.path(paste(out.file,"_F_relativ",'.html',sep='')),
         dec=c(2,rep(0,dim(a)[[2]]-1)), width='"100%"',units=c(' ',rep('(% of default.)',length(dirs)-1)))
  }  
    

  a<-tapply(aa$belowBlim ,list(aa$Species,aa$labels),sum)
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_blim",'.html',sep='')),
             dec=rep(1,dim(a)[[2]]), width='"100%"',units=' ')
  
    a<-tapply(aa$yield ,list(aa$Species,aa$labels),sum) /1000
      All<-tapply(aa$yield ,list(aa$labels),sum) /1000
    a<-rbind(a,All)  
   xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_yield",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')

  if (relativ) {
    for (i in (2:dim(a)[[2]])) a[,i]<-a[,i]/a[,1]*100
    xtab(a, caption='', cornername='  ',
         file=file.path(paste(out.file,"_yield_relativ",'.html',sep='')),
         dec=rep(0,dim(a)[[2]]), width='"100%"',units=c('(kt) ',rep('(% of default.)',length(dirs)-1)))
  }
  
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
   
  
  if (relativ) {
    for (i in (2:dim(a)[[2]])) a[,i]<-a[,i]/a[,1]*100
    xtab(a, caption='', cornername='  ',
         file=file.path(paste(out.file,"_SSB_relativ",'.html',sep='')),
         dec=rep(0,dim(a)[[2]]), width='"100%"',units=c('(kt) ',rep('(% of default.)',length(dirs)-1)))
  }
  
  
                 
    a<-tapply(aa$SSB ,list(aa$Species,aa$labels),sum) 
    a2<-tapply(aa$SSBSD ,list(aa$Species,aa$labels),sum) 
    a<-a2/a*100
  
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,"_SSB_CV",'.html',sep='')),
             dec=rep(0,dim(a)[[2]]), width='"100%"',units=' ')                                     
                                

 if (tableIndicators) {
  b<-NULL
  for (dir in dirs) {
    a<-Read.OP.community.indicator(dir=file.path(data.path,dir))
    a<-droplevels(subset(a,Year>=first.y & Year<=last.y ))
    a<-data.frame(scen=dir,a)
    b<-rbind(b,a)
  }
  label<-data.frame(scen=dirs,labels)
  aa<-merge(b,label)
  aaa<-aggregate(cbind(bio.demer,bio.small,bio.pelag,bio.forage,comm.Fland,comm.Fall,comm.M,comm.M2,community.life.expect,LFI) ~ labels,data=aa,mean)

  b<-cbind(aaa$comm.M2*100,aaa$comm.Fall*100,aaa$LFI*100,aaa$community.life.expect,aaa$bio.forage/1000,aaa$bio.pelag/aaa$bio.demer)
  b<-t(b)
 
 dimnames(b)[[1]]<-c("Community M2 (%)","Community F (%)","LFI (%)","life expectancy (years)","Biomass forage (kt)","Biomass ratio pelagic:demersal")
   dimnames(b)[[2]]<-aaa$labels
 
   xtab3(b, caption='', cornername='  ',
             file=file.path(paste(out.file,"_sys_Indicators",'.html',sep='')),
             dec=c(0,0,0,2,0,2), width='"100%"',units=' ')    
 }
}
#################


#dirs<-paste("D1f_HCR_1_1_Rec0_penBpa____CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D2f_HCR_2_0_Rec0_penBpa____CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D1f_HCR_1_1_Rec0_penBpa____CVadj_limF_110_Oth",1:10,sep='')
dirs<-paste("D1c_HCR_1_1_Rec0_penBpa_atAgeW___CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D1e_HCR_1_1_Rec0_penBlim____CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D1b_HCR_1_1_Rec0_penBlim_atAgeW___CVadj_limF_110_Oth",1:10,sep='')
labels<-c('01. Default','02. Birds','03. R. radiata','04. Gurnards','05. Mackerel','06. Horse Mackerel','07. Grey seal','08. Porpoise','09. Hake','10. All')
opt.table(out.file=file.path(data.path,'opti_compare_determ_HCR1_species_1c'),first.y=2043,last.y=2043,stochastic=F, test=T,
          dirs=dirs,relativ=T,
          labels=labels )

 
##############

dirs<-c(
  "D1a_HCR_1_1_Rec0__atAgeW___CVadj_limF_110_",
  "D1d_HCR_1_1_Rec0_____CVadj_limF_110_",
  "D1b_HCR_1_1_Rec0_penBlim_atAgeW___CVadj_limF_110_",
  "D1e_HCR_1_1_Rec0_penBlim____CVadj_limF_110_",
  "D1c_HCR_1_1_Rec0_penBpa_atAgeW___CVadj_limF_110_",
  "D1f_HCR_1_1_Rec0_penBpa____CVadj_limF_110_"
)

labels<-c(
  "Value",
  "Yield",
  "Value, penalize SSB < Blim",
  "Yield, penalize SSB < Blim",
  "Value, penalize SSB < Bpa",
  "Yield, penalize SSB < Bpa"
)


opt.table(out.file=file.path(data.path,'opti_compare_determ_Species_HCR1_all'),first.y=2034,last.y=2043,stochastic=F, test=T,
          dirs=dirs,
          labels=labels )



dirs<-c(
  "D2a_HCR_2_0_Rec0__atAgeW___CVadj_limF_110_",
  "D2d_HCR_2_0_Rec0_____CVadj_limF_110_",
  "D2b_HCR_2_0_Rec0_penBlim_atAgeW___CVadj_limF_110_",
  "D2e_HCR_2_0_Rec0_penBlim____CVadj_limF_110_",
  "D2c_HCR_2_0_Rec0_penBpa_atAgeW___CVadj_limF_110_",
  "D2f_HCR_2_0_Rec0_penBpa____CVadj_limF_110_"
)

labels<-c(
  "Value",
  "Yield",
  "Value, penalize SSB < Blim",
  "Yield, penalize SSB < Blim",
  "Value, penalize SSB < Bpa",
  "Yield, penalize SSB < Bpa"
)

opt.table(out.file=file.path(data.path,'opti_compare_determ_Species_HCR2'),first.y=2034,last.y=2043,stochastic=F, test=T,
          dirs=dirs,
          labels=labels )




