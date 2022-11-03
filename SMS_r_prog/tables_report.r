
tab_summary_data<-function(outfile,tableNo=1,my.species=c(first.VPA:SMS.control@no.species)) {
  file<-file.path(data.path,'summary_table_raw.out')
  aa<-read.table(file,header=TRUE)
  if (file.exists(file.path(data.path,outfile))) file.remove(file.path(data.path,outfile))
  for (sp in my.species) {
    a<-subset(aa,Species.n==sp)
    print(sp)
    print(SMS.control@discard[sp-first.VPA+1])
    discard<-SMS.control@discard[sp-first.VPA+1]==1
    
    a$Rec<-a$Rec/1000
    tab1<-cbind(a$Rec,a$TSB,a$SSB,a$SOP,a$mean.F)
    geo<-c(exp(mean(log(a$Rec),na.rm = T)),NA,NA,NA,NA)
    tab1<-rbind(tab1,colMeans(tab1,na.rm = T),geo)

    if (discard) colnames(tab1)<-c("Recruits","TSB","SSB","Catch","Mean F") else colnames(tab1)<-c("Recruits","TSB","SSB","Yield","Mean F")
    rownames(tab1)<-c(a$Year,'arith. mean','geo. mean')
    units<-c("(million)","(tonnes)","(tonnes)","(tonnes)",paste('ages ',SMS.control@avg.F.ages[sp.names[sp],1],'-', SMS.control@avg.F.ages[sp.names[sp],2],sep=''))
    if (discard)  title<-paste(".  Estimated recruitment, total stock biomass (TBS), spawning stock biomass (SSB), catch weight incl discards (Catch) and average fishing mortality.")
    if (!discard) title<-paste(".  Estimated recruitment, total stock biomass (TBS), spawning stock biomass (SSB), landings weight (Yield) and average fishing mortality.")

    xtab(tab1, caption=paste("Table ",tableNo,'  ',sp.names[sp],title), cornername='Year',
         file=file.path(data.path,'_tmp.html'), dec=c(0,0,0,0,3), width='"100%"',units=units)
    if (length(my.species)==1) {
     file.copy(file.path(data.path,'_tmp.html'), file.path(data.path,outfile), overwrite = T, recursive = FALSE)
   } else file.append(file.path(data.path,outfile), file.path(data.path,'_tmp.html'))
 }
}


tab_annual_sum_data<-function(data,vari='C.obs',outfile,title,decimals=0,tableNo=1,my.species=c(first.VPA:SMS.control@no.species)) {
  if (file.exists(file.path(data.path,outfile))) file.remove(file.path(data.path,outfile))
  for (sp in my.species) {
    a<-subset(data,Species.n==sp)
    if (dim(a)[1] >0) {
      if (vari=='C.obs') tab1<-tapply(a$C.obs,list(a$Year,a$Age),sum)
      if (vari=='F') tab1<-tapply(a$F,list(a$Year,a$Age),sum)
      if (vari=='M2') tab1<-tapply(a$M2,list(a$Year,a$Age),sum)
      if (vari=='M1M2') tab1<-tapply(a$M2+a$M1,list(a$Year,a$Age),sum)
      if (vari=='M') tab1<-tapply(a$M,list(a$Year,a$Age),sum)
      if (vari=='ration') tab1<-tapply(a$ration,list(a$Year,a$Age),sum)
      if (vari=='consum') tab1<-tapply(a$consum,list(a$Year,a$Age),sum)

      if (sum(tab1,na.rm=T)>0) {
        colnames(tab1)<-sort(unique(a$Age))
        xtab(tab1, caption=paste("Table ",tableNo,'  ',sp.names[sp],title),
           cornername='Year/Age',
           file=file.path(data.path,'_tmp.html'), dec=rep(decimals,length(unique(a$Age))), width='"100%"')
        if (length(my.species)==1) {
           file.copy(file.path(data.path,'_tmp.html'), file.path(data.path,outfile), overwrite = T, recursive = FALSE)
        } else file.append(file.path(data.path,outfile), file.path(data.path,'_tmp.html'))
      }
    }
  }
}


tab_quarterly_data<-function(data,vari='N',outfile,title,decimals=0,tableNo=1,my.species=c(first.VPA:SMS.control@no.species),do.simple=T) {
  outfile1<-paste(outfile,".html",sep='')
  outfile2<-paste(outfile,".txt",sep='')

  if (file.exists(file.path(data.path,outfile1))) file.remove(file.path(data.path,outfile1))
  if (do.simple) if (file.exists(file.path(data.path,outfile2))) file.remove(file.path(data.path,outfile2))

  for (sp in my.species) {
    a<-subset(data,Species.n==sp)
    if (dim(a)[1] >0) {
      a$AgeQ<-paste("A",formatC(a$Age,flag='0',width=2),"Q",a$Quarter,sep='')
      if (vari=='N') tab1<-tapply(a$N,list(a$Year,a$AgeQ),sum)
      if (vari=='west') tab1<-tapply(a$west,list(a$Year,a$AgeQ),sum)
      if (vari=='weca') tab1<-tapply(a$weca,list(a$Year,a$AgeQ),sum)
      if (vari=='C.obs') tab1<-tapply(a$C.obs,list(a$Year,a$AgeQ),sum)
      if (vari=='F') tab1<-tapply(a$F,list(a$Year,a$AgeQ),sum)
      
      if (vari=='Lsea') tab1<-tapply(a$Lsea,list(a$Year,a$AgeQ),sum)
      if (vari=='M2') tab1<-tapply(a$M2,list(a$Year,a$AgeQ),sum)
      if (vari=='M1M2') tab1<-tapply(a$M1+a$M2,list(a$Year,a$AgeQ),sum)
      if (vari=='ration') tab1<-tapply(a$ration,list(a$Year,a$AgeQ),sum)
      if (vari=='consum') tab1<-tapply(a$consum,list(a$Year,a$AgeQ),sum)

      colnames(tab1)<-sort(unique(a$AgeQ))
      xtab(tab1, caption=paste("Table ",tableNo,'  ',sp.names[sp],title),
         cornername='Year/Age Quarter',
         file=file.path(data.path,'_tmp.html'), dec=rep(decimals,length(unique(a$AgeQ))), width='"100%"')
      if (length(my.species)==1) {
         file.copy(file.path(data.path,'_tmp.html'), file.path(data.path,outfile1), overwrite = T, recursive = FALSE)
      } else file.append(file.path(data.path,outfile1), file.path(data.path,'_tmp.html'))
      
      if (do.simple) {
        if (SMS.control@species.info[sp,'last-age']>9) a$Age<-paste("Age ",formatC(a$Age,flag='0',width=2),sep='') else a$Age<-paste("Age ",a$Age,sep='')
        a$Quarter<-paste("Q",a$Quarter,sep='')
        ll<-list(a$Year,a$Quarter,a$Age)
        if (vari=='N') tab1<-tapply(a$N,ll,sum)
        if (vari=='west') tab1<-tapply(a$west,ll,sum)
        if (vari=='weca') tab1<-tapply(a$weca,ll,sum)
        if (vari=='C.obs') tab1<-tapply(a$C.obs,ll,sum)
        if (vari=='F') tab1<-tapply(a$F,ll,sum)
        
        if (vari=='Lsea') tab1<-tapply(a$Lsea,ll,sum)
         if (vari=='M1M2') tab1<-tapply(a$M1+a$M2,ll,sum)
        if (vari=='M2') tab1<-tapply(a$M2,ll,sum)
        if (vari=='ration') tab1<-tapply(a$ration,ll,sum)
        if (vari=='consum') tab1<-tapply(a$consum,ll,sum)
        sink(file=file.path(data.path,'a'))
        cat(paste("\nTable ",tableNo,'  ',sp.names[sp],title,"\n\n"))
        print(ftable(round(tab1,decimals)))
        sink()
        a<-readLines(file.path(data.path,'a'))
        a<-gsub('NA',' .',a)
        a<-writeLines(a,file.path(data.path,'a'))
        if (length(my.species)==1) {
           file.copy(file.path(data.path,'a'), file.path(data.path,outfile2), overwrite = T, recursive = FALSE)
        } else file.append(file.path(data.path,outfile2), file.path(data.path,'a'))
      }
    }
  }
}

tab_quarterly_sum_data<-function(data,vari='Yield',outfile,title,decimals=0,tableNo=1,my.species=c(first.VPA:SMS.control@no.species)) {
  if (file.exists(file.path(data.path,outfile))) file.remove(file.path(data.path,outfile))
  for (sp in my.species) {
    a<-subset(data,Species.n==sp)
    if (dim(a)[1] >0) {
      if (vari=='Yield') tab1<-tapply(a$weca*a$C.obs,list(a$Year,a$Quarter),sum)
      if (vari=='F') tab1<-tapply(a$F,list(a$Year,a$Quarter),sum)
      
       colnames(tab1)<-sort(unique(a$Quarter))
      xtab(tab1, caption=paste("Table ",tableNo,'  ',sp.names[sp],title),
         cornername='Year/Quarter',
         file=file.path(data.path,'_tmp.html'), dec=rep(decimals,length(unique(a$Quarter))), width='"100%"')
      if (length(my.species)==1) {
         file.copy(file.path(data.path,'_tmp.html'), file.path(data.path,outfile), overwrite = T, recursive = FALSE)
      } else file.append(file.path(data.path,outfile), file.path(data.path,'_tmp.html'))
    }
  }
}


tab_quarterly_data(data=Read.summary.data(),vari='F',   outfile='_tab_F4',decimals=3,title=":  Fishing mortality")

tab_quarterly_sum_data(data=Read.summary.data(),vari='Yield',outfile='_tab_Yield4.html',decimals=0,title=":   Seasonal yield")

tab_annual_sum_data(data=Read.summary.data(),vari='C.obs',outfile='_tab_C1.html',decimals=0,title=":   Annual catch numbers")
tab_annual_sum_data(data=Read.summary.data(),vari='F',outfile='_tab_F1.html',decimals=3,title=":   Fishing mortality (sum of quarterly F)")


tab_quarterly_data(data=Read.summary.data(),vari='C.obs',outfile='_tab_C4',decimals=0,title=":   Seasonal catch numbers")
tab_quarterly_data(data=Read.summary.data(),vari='N',   outfile='_tab_oth_N',decimals=0,my.species=c(1:first.VPA-1),title=":  Stock numbers (thousands)")

tab_quarterly_data(data=Read.summary.data(extend=T),vari='N',   outfile='_tab_N',decimals=0,title=":  Stock numbers (thousands)")

dat<-Read.summary.data(extend=T)
dat<-subset(dat,Quarter==1)
tab_quarterly_data(data=dat,vari='N',   outfile='_tab_N_annu',decimals=0,title=":  Stock numbers (thousands)")

tab_quarterly_data(data=Read.summary.data(),vari='west',outfile='_tab_WEST',decimals=3,title=":   Mean weight in the sea(kg)")
tab_quarterly_data(data=Read.summary.data(),vari='weca',outfile='_tab_WECA',decimals=3,title=":   Mean weigh in the catcht(kg)")

if (sum(SMS.control@species.info[,'predator'])>0) {
  tab_quarterly_data(data=Read.summary.data(),vari='ration',outfile='_tab_ration4',decimals=3,title=":   Eaten biomass (kg) for one individual")
  #tab_quarterly_data(data=Read.other.predator(),vari='consum',outfile='_tab_ration4OthPred.html',decimals=3,title=":   Eaten biomass (kg) for one individual",my.species=c(1:(first.VPA-1)))
  tab_quarterly_data(data=Read.summary.data(),vari='Lsea',outfile='_tab_Lsea',decimals=0,title=":   Mean length in the sea(mm)")
  tab_quarterly_data(data=Read.summary.data(),vari='M2',outfile='_tab_M2',decimals=3,title=":   Predation mortality (M2)")
  tab_quarterly_data(data=Read.summary.data(),vari='M1M2',outfile='_tab_M1M2',decimals=3,title=":   Natural mortality (M1+M2)")
  tab_annual_sum_data(data=Read.summary.data(),vari='M2',outfile='_tab_M2_1.html',decimals=3,title=":   Predation mortality (sum of quarterly M2)")
  tab_annual_sum_data(data=Read.summary.data(),vari='M1M2',outfile='_tab_M1M2_1.html',decimals=3,title=":   Natural mortality (sum of quarterly M1+M2)")
  tab_annual_sum_data(data=Read.summary.data(),vari='M',outfile='_tab_M_1.html',decimals=3,title=":   Natural mortality (sum of quarterly M)")
  
  tab_annual_sum_data(data=Read.summary.data(),vari='ration',outfile='_tab_ration.html',decimals=3,title=":   Eaten biomass (kg) for one individual")
  tab_annual_sum_data(data=Read.other.predator(),vari='consum',outfile='_tab_rationOtherPred.html',decimals=3,title=":   Eaten biomass (kg) for one individual",my.species=c(1:(first.VPA-1)))
}


tab_summary_data(outfile='_tab_summary.html')


#########################################
