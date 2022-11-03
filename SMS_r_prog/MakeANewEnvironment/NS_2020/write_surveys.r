
surv.file<-file.path(finalExchangeDir,paste0('fleet_catch.in'))
surv.name.file<-file.path(finalExchangeDir,paste0('fleet_names.in'))

cat('#\n',file=surv.file)
cat('',file=surv.name.file)
write.surv<-function(sp='COD',effort=1E-04,nr=1,digits=3) {
  x<-all.surveys[[sp]]
  if (nr==1) cat('###   ',sp,'  #####################################################\n',file=surv.file,append=TRUE)
  surv.name<-names(x)[nr]
  surv.name<-sub("\t+$", "", surv.name)  ## 
  cat(paste(sp,surv.name,sep='_'),'\n',file=surv.name.file,append=TRUE)
  x<-x[[nr]]
  dims<-dimnames(x)
  cat("## ",sp,'  fleet;',nr,'  ',surv.name," age ",min(dims[[2]]),'-',max(dims[[2]]),' timing:',attr(x,'time'),', ',dims[[1]][1],'-',tail(dims[[1]],1),'\n',file=surv.file,append=TRUE)
  for (y in dimnames(x)[[1]]) {
    cat(effort,formatC(x[y,],digits=digits, width=12,format='f'),paste0('   #',y,'\n'),file=surv.file,append=TRUE) 
  }
}

write.surv(sp='COD',nr=1,effort=1E-02)
write.surv(sp='COD',nr=2,effort=1E-02)

write.surv(sp='WHG',nr=1,effort=1E-03)
write.surv(sp='WHG',nr=2,effort=1E-03)

write.surv(sp='HAD',nr=1,effort=1E-05)
write.surv(sp='HAD',nr=2,effort=1E-05)

write.surv(sp='POK',nr=1,effort=1E-05)

N<-read.csv(file.path(root,exchangeDir,'ByStock','POK','SMS-data',"stock_N.csv"))
y<-N$year
N$year<-NULL

N<-as.matrix(N)
rownames(N)<-y
sdlog<-0.5
cat("##  POK   Stock number with lnorm dist",sdlog, " error, age  3 - 9  timing: 0 0\n",file=surv.file,append=TRUE)
noise<-rlnorm(n=length(N),sdlog=sdlog)
hist(log(noise))
summary(noise)
N<-N*noise

for (y in dimnames(N)[[1]]) {
  cat(1,formatC(N[y,1:7],digits=0, width=12,format='f'),paste0('   #',y,'\n'),file=surv.file,append=TRUE) 
}
cat('POK_N_with_noise\n',file=surv.name.file,append=TRUE)

#MAC


if (FALSE) {
  library(XML)
  
  
  macdir<-file.path("C:","MV","SMS-git","SMS-input-key-run-2017","WGNSSK","user115-NS_saithe_2017_corr_DATRASQ3_cw4sw","NS_saithe_2017_corr_DATRASQ3_cw4sw","res")
  readXML<-function(file="xxx-00-00.00.00_tab2.html") {
    a<-readHTMLTable(file.path(macdir,file),skip.rows = 2,colClasses = 'numeric',as.data.frame = TRUE)
    a<-a[[1]]
    age<-as.vector(tail(t(a[1,]),-1))
    a<-tail(a,-2)
    year<-a$V1
    a$V1<-NULL
    a<-as.matrix(a,ncol=length(year))
    dimnames(a)<-list(year,age)
    a<-head(a,-1)
    return(a)
    
  }
  
  N<-readXML(file="xxx-00-00.00.00_tab2.html") 
}


write.surv(sp='MAC',nr=1,effort=1E-07)
write.surv(sp='MAC',nr=2,effort=1E-00)

load(file.path(root,exchangeDir,'ByStock','MAC',"model fit.RData"),verbose=T)
N<-exp(fit.new$pl$logN)
N<-t(N)

rownames(N)<-fit.new$data$years
round(N[y,1:8])

sdlog=0.40
cat("##  MAC   Stock number with lnorm dist",sdlog, " error, age  1 - 9  timing: 0 0.01 \n",file=surv.file,append=TRUE)
N<-N*rlnorm(n=length(N),sdlog=sdlog)
effort=1

for (y in dimnames(N)[[1]]) {
  cat(effort,formatC(N[y,2:10],digits=0, width=12,format='f'),paste0('   #',y,'\n'),file=surv.file,append=TRUE) 
}
cat('MAC_N_with_noise\n',file=surv.name.file,append=TRUE)


write.surv(sp='HER',nr=1,effort=1E-00,digits=0)
write.surv(sp='HER',nr=2,effort=1E-02,digits=2)
write.surv(sp='HER',nr=3,effort=1E-05,digits=2)

# sandeel commercial CPUE
a<-read.csv(file=file.path(root,exchangeDir,'ByStock','sandeel','effort_to_sandeel_assessment_1982_2019_NS.csv'))
a$species<-ifelse(a$NNSN=='NN','NSA','SSA')

#a$quarter<-ifelse(a$hy==1,2,3)
a<-subset(a,select=c(species,year,quarter,effort2))

b<-subset(NewCat,species %in% c('NSA','SSA'))
head(b)

a<-subset(merge(a,b),select=c(-WCATCH,-PROP_CAT,-oldNew))
head(a)

cpue<-reshape(a,direction='wide',timevar='age',v.names='CATCHN',idvar=c('species','year','quarter','effort2'))
cpue<-subset(cpue,select=c(effort2, CATCHN.0,  CATCHN.1, CATCHN.2, CATCHN.3, species,year,quarter))
cpue<-cpue[order(cpue$species,cpue$quarter,cpue$year),]



wr_sandeel<-function(my.species='NSA', my.quarter=2) {
  a<-subset(cpue,species==my.species & quarter==my.quarter)
  a<-a[order(a$year),]
  cat('#', a[1,'species'],' Quarter:',a[1,'quarter'],  'efort age1-3 \n',file=surv.file,append=TRUE)
  for (i in (1:dim(a)[[1]])) {
    cat(formatC(c(a[i,'effort2'],a[i,'CATCHN.1'],a[i,'CATCHN.2'],a[i,'CATCHN.3']),digits=0, width=12,format='f'),paste0('   #',a[i,'year'],'\n'),file=surv.file,append=TRUE) 
  }
  surv.name<-paste(my.species,'Commercial',sep='_')
  cat(surv.name,'\n',file=surv.name.file,append=TRUE)
}

write.surv(sp='NSA',nr=1,effort=1E-04,digits=0)
wr_sandeel(my.species='NSA', my.quarter=2) 

tt<-readLines(file.path(root,exchangeDir,'ByStock','sandeel','Sandeel_north_season2_old.dat'))
cat(paste0(tt,'\n'),file=surv.file,append=TRUE) 
cat('NSA Commercial','\n',file=surv.name.file,append=TRUE)


tt<-readLines(file.path(root,exchangeDir,'ByStock','sandeel','sandeel_north_acoustic.dat'))
cat(paste0(tt,'\n'),file=surv.file,append=TRUE) 
cat('NSA_acoustic_SA_1R','\n',file=surv.name.file,append=TRUE)


write.surv(sp='SSA',nr=1,effort=1E-04,digits=0)
wr_sandeel(my.species='SSA', my.quarter=2) 


write.surv(sp='NOP',nr=1,effort=1E-04,digits=0)
write.surv(sp='NOP',nr=2,effort=1E-04,digits=0)
write.surv(sp='NOP',nr=3,effort=1E-04,digits=0)
write.surv(sp='NOP',nr=4,effort=1E-04,digits=0)


write.surv(sp='SPR',nr=1,effort=1E-04,digits=0)
write.surv(sp='SPR',nr=2,effort=1E-04,digits=0)
write.surv(sp='SPR',nr=3,effort=1E-04,digits=0)


write.surv(sp='PLE',nr=1,effort=1E-04,digits=3)
write.surv(sp='PLE',nr=2,effort=1E-02,digits=3)
write.surv(sp='PLE',nr=3,effort=1E-02,digits=2)
write.surv(sp='PLE',nr=4,effort=1E-03,digits=2)
write.surv(sp='PLE',nr=5,effort=1E-03,digits=3)
write.surv(sp='PLE',nr=6,effort=1E-02,digits=2)

write.surv(sp='SOL',nr=1,effort=1E-03,digits=3)
write.surv(sp='SOL',nr=2,effort=1E-02,digits=1)
write.surv(sp='SOL',nr=3,effort=1E-02,digits=1)

a<-readLines(surv.name.file)
a<-formatC(a,format='s',flag='-',width=26)
a<-gsub(' ','_',a)
writeLines(a,surv.name.file)
cat("12345678901234567890123456\n",file=surv.name.file,append=TRUE)
cat('Please note, exactly 26 caharacters in fleet names, space as "_"\n',file=surv.name.file,append=TRUE)

