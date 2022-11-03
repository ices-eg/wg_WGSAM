
firstY<-1974    # first year in key run
lastY<-2019     # last year in (new) key run

new.code.name<-c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP",'HAK','COD','WHG','HAD','POK','MAC','HER','NSA','SSA','NOP','SPR','PLE','SOL')

oth<-new.code.name[1:(first.VPA-1)]


# 1. create csv file to be edidet
ofile<-file.path(data.path,'other_catch.csv')
if (FALSE) {
  cat('species, year, catch \n',file=ofile)
  for (sp in oth) {
    for (y in (firstY:lastY)) {
      cat(paste(sp,y,0,sep=','),'\n',file=ofile,append=TRUE)
    }
  }
}

# 2. update csv file  manually

# 3. transform ito SMS input file
a<-read.csv(ofile)
head(a)
ofile2<-file.path(data.path,'other_catch.in')
cat("#### catch of other species within model area - used for output only.\n",file=ofile2)
isp<-1
for (sp in (oth)) {
  cat("## ",sp.names[isp]," ##\n",file=ofile2,append=TRUE)
  b<-subset(a,species==sp)
  b<-b[order(b$year),]
  cat(paste(b$catch," # ",firstY:lastY,'\n'),file=ofile2,append=TRUE)

  isp<-isp+1
}
cat(-9999, ' #Check value \n',file=ofile2,append=TRUE)
