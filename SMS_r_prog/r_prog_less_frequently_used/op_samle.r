data.path<-file.path("C:","MV","SMS","NS_63-10-OP")
scenario<-"HCR_stoc_rec_final"
scenario.dir<-file.path(data.path,scenario)
do.save<-F


spNames<-c('COD', 'WHG', 'HAD', 'POK', 'HER', 'SAN', 'NOR', 'SPR', 'PLE', 'SOL')
allnames<-c(c('yield', 'CWsum', 'Fbar', 'SSB', 'TSB', 'recruit', 'Species.n', 'iteration'),spNames)

if (do.save) {
  setwd(scenario.dir)
  condensed<-read.table(file.path(data.path,"HCR-fin1_stoc_rec","OP_Fcombinations.out"),header=FALSE)
  dimnames(condensed)[[2]]<-allnames
  
  a<-read.table(file.path(data.path,"HCR-fin2_stoc_rec","OP_Fcombinations.out"),header=FALSE)
  dimnames(a)[[2]]<-allnames
  condensed<-rbind(condensed,a)
 
  a<-read.table(file.path(data.path,"HCR-fin3_stoc_rec","OP_Fcombinations.out"),header=FALSE)
  dimnames(a)[[2]]<-allnames
  condensed<-rbind(condensed,a)

  a<-read.table(file.path(data.path,"HCR-fin4_stoc_rec","OP_Fcombinations.out"),header=FALSE)
  dimnames(a)[[2]]<-allnames
  condensed<-rbind(condensed,a)

  a<-read.table(file.path(data.path,"HCR-fin5_stoc_rec","OP_Fcombinations.out"),header=FALSE)
  dimnames(a)[[2]]<-allnames
  condensed<-rbind(condensed,a)
  rm(a)
  condensed<-droplevels(subset(condensed,Species.n<25))
  
  save(condensed, file =file.path(scenario.dir, "condensed.RData"))
} else load(file =file.path(scenario.dir, "condensed.RData"))

setwd(data.path)


a<-condensed

rm(condensed)
z <- sapply(ls(), function(x) object.size(get(x))) ;as.matrix(rev(sort(z))[1:10])

source(file.path(prog.path,"FMSY-matrix.R"))


table.MSY<-function(a){
  aa<-tapply(a$yield,list(a$Species.n,a$Fround),median)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Median MSY (1000 tonnes)\n')
  print(round(aa/1000,0))

  aa2<-tapply(a$yield,list(a$Species.n,a$Fround),sd)/tapply(a$yield,list(a$Species.n,a$Fround),mean)
  dimnames(aa2)[[1]]<-dimnames(aa)[[1]]
  
  MSY<-apply(aa,1,max,na.rm=T)
  ab<-aa/rep(MSY,times=dim(aa)[2])
  cat('\n')
  
  aa<-tapply(a$SSB,list(a$Species.n,a$Fround),median)
  dimnames(aa)[[1]]<-sp.names[as.numeric(dimnames(aa)[[1]])]
  cat('Median SSB (1000 tonnes)\n')
  print(round(aa/1000))
}

table.MSY(a)

a<-droplevels(subset(a,COD<0.66))
table.MSY(a)

a<-droplevels(subset(a,POK>0.4))
table.MSY(a)
