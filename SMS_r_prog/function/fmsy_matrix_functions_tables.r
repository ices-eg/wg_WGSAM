del_not_used_comb<-function(speciesN=1,fval=0,a=a,a2=a2) {
  if (my.area=='North Sea'){

    if (KeyRunYear==2014) {
      if (speciesN==18) {a2<-droplevels(subset(a,!( COD1==fval))) }
      if (speciesN==19) {a2<-droplevels(subset(a,!( WHG1==fval))) }
      if (speciesN==20) {a2<-droplevels(subset(a,!( HAD1==fval))) }
      if (speciesN==21) {a2<-droplevels(subset(a,!( POK1==fval))) }
      if (speciesN==22) {a2<-droplevels(subset(a,!( HER1==fval))) }
      if (speciesN==23) {a2<-droplevels(subset(a,!( NSA1==fval))) }
      if (speciesN==24) {a2<-droplevels(subset(a,!( SSA1==fval))) }        
      if (speciesN==25) {a2<-droplevels(subset(a,!( NOR1==fval))) }
      if (speciesN==26) {a2<-droplevels(subset(a,!( SPR1==fval))) }
    }
    
    if (KeyRunYear==2017) {
      if (speciesN==16) {a2<-droplevels(subset(a,!( COD1==fval))) }
      if (speciesN==17) {a2<-droplevels(subset(a,!( WHG1==fval))) }
      if (speciesN==18) {a2<-droplevels(subset(a,!( HAD1==fval))) }
      if (speciesN==19) {a2<-droplevels(subset(a,!( POK1==fval))) }
      if (speciesN==20) {a2<-droplevels(subset(a,!( MAK1==fval))) }
      if (speciesN==21) {a2<-droplevels(subset(a,!( HER1==fval))) }
      if (speciesN==22) {a2<-droplevels(subset(a,!( NSA1==fval))) }
      if (speciesN==23) {a2<-droplevels(subset(a,!( SSA1==fval))) }        
      if (speciesN==24) {a2<-droplevels(subset(a,!( NOR1==fval))) }
      if (speciesN==25) {a2<-droplevels(subset(a,!( SPR1==fval))) }
    }
    if (KeyRunYear==2020) {
      if (speciesN==16) {a2<-droplevels(subset(a,!( COD1==fval))) }
      if (speciesN==17) {a2<-droplevels(subset(a,!( WHG1==fval))) }
      if (speciesN==18) {a2<-droplevels(subset(a,!( HAD1==fval))) }
      if (speciesN==19) {a2<-droplevels(subset(a,!( POK1==fval))) }
      if (speciesN==20) {a2<-droplevels(subset(a,!( MAK1==fval))) }
      if (speciesN==21) {a2<-droplevels(subset(a,!( HER1==fval))) }
      if (speciesN==22) {a2<-droplevels(subset(a,!( NSA1==fval))) }
      if (speciesN==23) {a2<-droplevels(subset(a,!( SSA1==fval))) }        
      if (speciesN==24) {a2<-droplevels(subset(a,!( NOR1==fval))) }
      if (speciesN==25) {a2<-droplevels(subset(a,!( SPR1==fval))) }
    }
    
       
    #cat(" New dimension",dim(a2),'\n')
  }  else if (my.area=='Baltic Sea')  {
    if (speciesN==1) {a2<-droplevels(subset(a,!( COD1==fval))) }
    if (speciesN==2) {a2<-droplevels(subset(a,!( HER1==fval))) }
    if (speciesN==3) {a2<-droplevels(subset(a,!( SPR1==fval))) }
  }     #cat(" New dimension",dim(a2),'\n')
  return(a2)
}


reduceCombBlim<-function(a,last.sp=no.pred) {
  cat("reduceCombBlim\n")
  goOn<<-F

  aa<-tapply(a$SSB,list(a$Species.n,a$Fround),median)
  ab<-aa/rep(Blim,times=dim(aa)[2])
  dimnames(ab)[[1]]<-sp.names[as.numeric(dimnames(ab)[[1]])]
  print(round(ab,2))
  for (i in (1:dim(ab)[[1]])) {  # do not consider rows (species) where no further reduction can be made
    if (sum(!is.na(ab[i,]))==1)  {
       cat('no further exclusion for :',dimnames(ab)[[1]][i],'\n')
       ab[i,]<-NA
    }
  }

  excl<-min(ab[1:last.sp,],na.rm=T)
  cat("excl:",excl,'\n')
  for (i in (1:last.sp)) {
    f<-match(excl,ab[i,])
    if (!is.na(f) & excl<1){ii<-i; j<-f; goOn<<-T}
  }

  cat("f:",f,'\n')
  if (goOn) {
    fval<-as.numeric(dimnames(ab)[[2]][j])
    speciesN<-match(dimnames(ab)[[1]][ii],sp.names)
    cat(sp.names[speciesN],'with F=',fval,'is below observed Blim\n\n')
    fval<- formatC(fval,digits = 3, width = 4, format = "f")
    #cat("fval:",fval," speciesN:",speciesN,"\n")
    a2<-del_not_used_comb(speciesN=speciesN,fval=fval,a=a,a2=a2)
    return(a2)
  } else return(NA)
}

#goOn<-T;  while (goOn) {a2<-reduceCombBlim(a,last.sp=no.pred); if (goOn) a<-a2} ;


reduceCombMSY<-function(a) {
  cat("reduceCombMSY\n")
  goOn<<-F

  aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
  MSY<-apply(aa,1,max,na.rm=T)
  ab<-aa/rep(MSY,times=dim(aa)[2])
  dimnames(ab)[[1]]<-sp.names[as.numeric(dimnames(ab)[[1]])]
  print(round(ab,2))

  for (i in (1:dim(ab)[[1]])) {  # do not consider rows (species) where no further reduction can be made
    if (sum(!is.na(ab[i,]))==2)  {
       #cat('no further exclusion for :',dimnames(ab)[[1]][i],'\n')
       ab[i,]<-NA

    } else if (sum(!is.na(ab[i,]))==3) {
      m<-match(1.0,ab[i,])
      if ( m>1) if (!is.na(ab[i,m-1])) if (m<dim(ab)[[2]]) if (!is.na(ab[i,m+1])) {
       #cat('no further exclusion for :',dimnames(ab)[[1]][i],'\n')
       ab[i,]<-NA
      }
    }
  }

  excl<-min(ab,na.rm=T)
  for (i in (1:dim(ab)[[1]])) {
    f<-match(excl,ab[i,])
   if (!is.na(f) & excl<1 &
       (sum(!is.na(ab[i,]))>3  | ((sum(!is.na(ab[i,]))==3) & (match(1.0,ab[i,])-f>1) |(match(1.0,ab[i,])-f<1)))){
     ii<-i;
     j<-f;
     goOn<<-T
   }
  }

  if (goOn) {
    fval<-as.numeric(dimnames(ab)[[2]][j])
    speciesN<-match(dimnames(ab)[[1]][ii],sp.names)
    cat(sp.names[speciesN],'with F=',fval,'is below FMSY.\n\n')
    fval<- formatC(fval,digits = 3, width = 4, format = "f")
    a2<-del_not_used_comb(speciesN=speciesN,fval=fval,a=a,a2=a2)
    return(a2)
    
  } else return(NA)
}


reduceCombMSY.one.left<-function(a) {
  cat("reduceCombMSY\n")
  goOn<<-F

  aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
  MSY<-apply(aa,1,max,na.rm=T)
  ab<-aa/rep(MSY,times=dim(aa)[2])
  dimnames(ab)[[1]]<-sp.names[as.numeric(dimnames(ab)[[1]])]
  print(round(ab,2))

  for (i in (1:dim(ab)[[1]])) {  # do not consider rows (species) where no further reduction can be made
    if (sum(!is.na(ab[i,]))==1)  {
       #cat('no further exclusion for :',dimnames(ab)[[1]][i],'\n')
       ab[i,]<-NA
    }
  }


  excl<-min(ab,na.rm=T)
  for (i in (1:dim(ab)[[1]])) {
    f<-match(excl,ab[i,])
   if (!is.na(f) & excl<1 & sum(!is.na(ab[i,]))>=2 ){
     ii<-i;
     j<-f;
     goOn<<-T
   }
  }

  if (goOn) {
    fval<-as.numeric(dimnames(ab)[[2]][j])
    speciesN<-match(dimnames(ab)[[1]][ii],sp.names)
    cat(sp.names[speciesN],'with F=',fval,'is below FMSY.\n\n')
    fval<- formatC(fval,digits = 3, width = 4, format = "f")
    a2<-del_not_used_comb(speciesN=speciesN,fval=fval,a=a,a2=a2)
    return(a2)
    
  } else return(NA)
}

#aa<-reduceCombMSY.one.left(a);
#rm(aa)


reduceCombBpa<-function(a) {
  cat("reduceCombBpa\n")
  goOn<<-F

  aa<-tapply(a$SSB,list(a$Species.n,a$Fround),median)
  ab<-aa/rep(Bpa,times=dim(aa)[2])
  dimnames(ab)[[1]]<-sp.names[as.numeric(dimnames(ab)[[1]])]
  print(round(ab,2))

  for (i in (1:dim(ab)[[1]])) {  # do not consider rows (species) where no further reduction can be made, that is only one value left
    if (sum(!is.na(ab[i,]))==1) ab[i,]<-NA
  }

  excl<-min(ab,na.rm=T)

  for (i in (1:dim(ab)[[1]])) {
    f<-match(excl,ab[i,])
   if (!is.na(f) & excl<1){ii<-i; j<-f; goOn<<-T}
  }

  if (goOn) {
    fval<-as.numeric(dimnames(ab)[[2]][j])
    speciesN<-match(dimnames(ab)[[1]][ii],sp.names)
    cat(sp.names[speciesN],'with F=',fval,'is below Bpa.\n\n')
    fval<- formatC(fval,digits = 3, width = 4, format = "f")
    a2<-del_not_used_comb(speciesN=speciesN,fval=fval,a=a,a2=a2)
    return(a2)
  } else return(NA)
}

reduceCombMSYfinal<-function(a,out.file='MSY_redu.out',scenario.dir=scenario.dir,app=F,keepPro=1) {
  cat("reduceCombMSYfinal\n")
  goOn<<-F

  aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
  MSY<-apply(aa,1,max,na.rm=T)
  ab<-aa/rep(MSY,times=dim(aa)[2])
  dimnames(ab)[[1]]<-sp.names[as.numeric(dimnames(ab)[[1]])]
  print(round(ab,2))
  sink(file.path(scenario.dir,out.file),append=app)
  print(round(ab,2))
  sink()
  for (i in (1:dim(ab)[[1]])) {  # do not consider rows (species) where no further reduction can be made
    if (sum(!is.na(ab[i,]))==1 |       # only one value left
         sum(ab[i,]>=keepPro,na.rm=T)==  sum(ab[i,]>0,na.rm=T) # all ratios above limit
        )  {
       #cat('no further exclusion for :',dimnames(ab)[[1]][i],'\n')
       ab[i,]<-NA
    }

  }
  if (all(is.na(ab))) goOn<-F else {
    excl<-min(ab,na.rm=T)
    for (i in (1:dim(ab)[[1]])) {
      f<-match(excl,ab[i,])
     if (!is.na(f) & excl<1 ){
       ii<-i;
       j<-f;
       goOn<<-T
     }
    }
  }

  if (goOn) {
    fval<-as.numeric(dimnames(ab)[[2]][j])
    speciesN<-match(dimnames(ab)[[1]][ii],sp.names)
    cat(sp.names[speciesN],'with F=',fval,'has the relatively lowest yield.\n\n')
    sink(file.path(scenario.dir,out.file),append=T)
    cat(sp.names[speciesN],'with F=',fval,'has the relatively lowest yield.\n\n')
    sink()
    fval<- formatC(fval,digits = 3, width = 4, format = "f")
    a2<-del_not_used_comb(speciesN=speciesN,fval=fval,a=a,a2=a2)
    return(a2)
  } else return(NA)
}


reduceCombRiskfinal<-function(a,riskLevels=c(30,30,30,30,30,40,40,40),out.file='redu_Blim.out',scenario.dir=scenario.dir,app=F,type=1,excludeSp=0) {
  cat("reduceCombRiskfinal\n")
  goOn<<-F
print(riskLevels)
  if (type==1) ab<-tapply(a$probBelowBlim,list(a$Species.n,a$Fround),mean)*100
  if (type==2) ab<-tapply(a$riskBlim,list(a$Species.n,a$Fround),mean)*100
  if (type==3) ab<-tapply(a$probBelowBpa,list(a$Species.n,a$Fround),mean)*100
  if (type==4) ab<-tapply(a$riskBpa,list(a$Species.n,a$Fround),mean)*100

  dimnames(ab)[[1]]<-sp.names[as.numeric(dimnames(ab)[[1]])]
  # print(ab)
  sink(file.path(scenario.dir,out.file),append=app)
  print(round(ab,1))
  sink()
  subs<-matrix(rep(riskLevels,times=dim(ab)[[2]]),ncol=dim(ab)[[2]])
  ab<-ab-subs
  #print(ab)

  for (i in (1:dim(ab)[[1]])) {  # do not consider rows (species) where no further reduction can be made
    if (sum(!is.na(ab[i,]))==1)  {
       #cat('no further exclusion for :',dimnames(ab)[[1]][i],'\n')
       ab[i,]<-NA
    }
    if (i %in% excludeSp) ab[i,]<-NA; 
  }
 
    if (all(is.na(ab))) goOn<-F else {
    excl<-max(ab,na.rm=T)
    for (i in (1:dim(ab)[[1]])) {
      # f<-match(excl,ab[i,]) # takes the minimum  value
      ff<-which(excl==ab[i,])
      if (length(ff)>1) f<- max(ff) else if (length(ff)==1) f<-ff else if (length(ff)==0) f<-NA
      #print(paste('old:',match(excl,ab[i,]),'new:',f))
     if (!is.na(f) & excl>1 ){
       ii<-i;
       j<-f;
       goOn<<-T
     }
    }
  }

  if (goOn) {
    fval<-as.numeric(dimnames(ab)[[2]][j])
    speciesN<-match(dimnames(ab)[[1]][ii],sp.names)
    sink(file.path(scenario.dir,out.file),append=T)
    cat(sp.names[speciesN],'with F=',fval,' has a too high (',round(ab[ii,j]+riskLevels[speciesN-first.VPA+1],1),'%) risk level to ',ifelse(type %in% c(1,2),'Blim','Bpa'),'.\n\n')
    sink()
    fval<- formatC(fval,digits = 3, width = 4, format = "f")
    a2<-del_not_used_comb(speciesN=speciesN,fval=fval,a=a,a2=a2)
    return(a2)
  } else return(NA)
}



table.MSY<-function(a,out.file='a',scenario.dir=scenario.dir,app=F){
  #aaa<<-a
  a$Fround<-round(a$Fbar,1)
  aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  sink(file.path(scenario.dir,paste(out.file,'.out',sep='')),append=app)
  cat('Median MSY (1000 tonnes)\n')
  print(round(aa/1000,0))
  cat('\n\n')
  cat('CV of yield\n')
  aa2<-tapply(a$yield,list(a$Species.n,a$Fround),sd)/tapply(a$yield,list(a$Species.n,a$Fround),mean)
  dimnames(aa2)[[1]]<-dimnames(aa)[[1]]
  print(round(aa2,2))

  MSY<-apply(aa,1,max,na.rm=T)
  ab<-aa/rep(MSY,times=dim(aa)[2])
  cat('\nYield relativ to observed MSY\n')
  print(round(ab,2))
  cat('\n')

  aa<-tapply(a$SSB,list(a$Species.n,a$Fround),median)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Median SSB (1000 tonnes)\n')
  print(round(aa/1000,0))
  cat('\n')

   aa<-tapply(a$SSB,list(a$Species.n,a$Fround),min)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Minimum SSB (1000 tonnes)\n')
  print(round(aa/1000,0))
  cat('\n')

   aa<-tapply(a$Fbar,list(a$Species.n,a$Fround),median)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Realised F\n')
  print(round(aa,2))
  cat('\n')

   aa<-tapply(a$probBelowBlim,list(a$Species.n,a$Fround),mean)*100
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Probability (%) of SSB below Blim\n')
  print(round(aa,0))
  cat('\n')
  
  aa<-tapply(a$probBelowBpa,list(a$Species.n,a$Fround),mean)*100
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Probability (%) of SSB below Bpa\n')
  print(round(aa,0))
  cat('\n')
  
  
  sink()
  print(round(ab,2))
}

#table.MSY(a,out.file=out.file)
