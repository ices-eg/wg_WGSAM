
opt.table<-function(out.file,dirs,labels,first.y=2051,last.y=2051,stochastic=F,test=F){
   #####################  
  for (dir in dirs) {
    if ( file.access(file.path(data.path,dir,"op.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
  } 
  
  Init.function() # get SMS.contol object  including sp.names
  
  ref<-Read.reference.points.OP(dir=data.path) 
  Blim<-as.vector(c(rep(-1, first.VPA-1),ref[,'Blim']))
  Bpa<-as.vector(c(rep(-1, first.VPA-1),ref[,'Bpa']))
  Fpa<-as.vector(c(rep(-1, first.VPA-1),ref[,'Fpa']))
  
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
  
  a<-aggregate(cbind(yield,value,SSB)~scen+Species.n+Species,mean,data=b)
  aa<-aggregate(cbind(yield,value,SSB)~scen,sum,data=a)
  
  risk<-aggregate(cbind(belowBlim,belowBpa,aboveFpa)~scen+Species.n+Species,sum,data=b)
  if (test)  saverisk<<-risk
  risk$belowBlim<-risk$belowBlim*100/ny
  risk$belowBpa<-risk$belowBpa*100/ny
  risk$aboveFpa<-risk$aboveFpa*100/ny
  risk.average<-aggregate(list(risk.average.Blim=risk$belowBlim),list(scen=risk$scen),mean)
  if (test)  saverisk2<<-risk
  risk$belowBlim<-ifelse(risk$belowBlim<5,0,1)
  risk$belowBlim10<-ifelse(risk$belowBlim<10,0,1)
  
  risk$belowBpa<-ifelse(risk$belowBpa<5,0,1)
  risk$aboveFpa<-ifelse(risk$aboveFpa>5,1,0)

  risk<-aggregate(cbind(belowBlim,belowBlim10,belowBpa,aboveFpa)~scen,sum,data=risk)
  if (test)  saverisk3<<-risk
  aa<-merge(aa,risk)
  aa<-merge(aa,risk.average)
  
  label<-data.frame(scen=dirs,labels)
  aa<-merge(aa,label)
  
  a<-cbind(
    t(t(tapply(aa$yield,aa$labels,sum)/1000)),
    t(t(tapply(aa$value,aa$labels,sum)/1000)),
    t(t(tapply(aa$SSB,aa$labels,sum)/1000)) ,
    t(t(tapply(aa$belowBlim,aa$labels,sum))),
    t(t(tapply(aa$belowBpa,aa$labels,sum))),
      t(t(tapply(aa$aboveFpa,aa$labels,sum))))
  if (stochastic) a<-cbind(a,t(t(tapply(aa$risk.average,aa$labels,sum)))) 
  my.dimnames<-c('Yield','Value','SSB','no. of stocks','no. of stocks','no. of stocks') 
  if (stochastic) my.dimnames<-c(my.dimnames,'Average (%)')
  
  dimnames(a)[[2]]<-my.dimnames 
  
  print(a)
  if (!stochastic) {
    my.units<-c('(kt)','(m Euro)','(kt)','below Blim','below Bpa ','above Fpa ')   
    my.dec<-c(0,0,0,0,0,0)
  } else {
    my.units<-c('(kt)','(m Euro)','(kt)','p(SSB < Blim) > 5%','p(SSB < Bpa) > 5%','above Fpa ','SSB < Blim')        
    my.dec<-c(0,0,0,0,0,0,0)
  }
  xtab(a, caption='', cornername='  ',
             file=file.path(paste(out.file,'.html',sep='')),
             dec=my.dec, width='"100%"',units=my.units)

  cat('\nOutput file: ',file.path(paste(out.file,'.html',sep='')))
}

#dirs<-paste("D1f_HCR_1_1_Rec0_penBpa____CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D2f_HCR_2_0_Rec0_penBpa____CVadj_limF_110_Oth",1:10,sep='')
dirs<-paste("D1f_HCR_1_1_Rec0_penBpa____CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D1c_HCR_1_1_Rec0_penBpa_atAgeW___CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D1e_HCR_1_1_Rec0_penBlim____CVadj_limF_110_Oth",1:10,sep='')
#dirs<-paste("D1b_HCR_1_1_Rec0_penBlim_atAgeW___CVadj_limF_110_Oth",1:10,sep='')
labels<-c('01. Default','02. Birds','03. R. radiata','04. Gurnards','05. Mackerel','06. Horse Mackerel','07. Grey seal','08. Porpoise','09. Hake','10. All')
opt.table(out.file=file.path(data.path,'opti_compare_determ_HCR1_all_other'),first.y=2034,last.y=2043,stochastic=F, test=T,
          dirs=dirs,
          labels=labels )


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

opt.table(out.file=file.path(data.path,'opti_compare_determ_HCR1'),first.y=2034,last.y=2043,stochastic=F, test=T,
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

opt.table(out.file=file.path(data.path,'opti_compare_determ_HCR2'),first.y=2034,last.y=2043,stochastic=F, test=T,
          dirs=dirs,
          labels=labels )


