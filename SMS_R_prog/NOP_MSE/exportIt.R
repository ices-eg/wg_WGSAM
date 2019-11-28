NopInput<-file.path('C:','MV','SMS-git','NOP_SXSA_SC2_Sep2017')
cn=read.table(file.path(NopInput,"CANUM.QRT"))
cw=read.table(file.path(NopInput,"Weca.qrt"))
sw=read.table(file.path(NopInput,"West.qrt"))
nm=read.table(file.path(NopInput,"Natmor.qrt"))
mo=read.table(file.path(NopInput,"Matprop.qrt"))
survey=read.table(file.path(NopInput,"tun2017.txt"))


yrange<-1984:2017
do.shift <- TRUE

      
fleet<-c('commercial________________',
         'IBTS_Q1_1984-_____________',
         'ENGFS_1992-_______________',
         'SGOGFS_1998-______________',
         'IBTS_Q3_1991-_____________')
          
out<-file.path(NopInput,'fleet_names.in')  
unlink(out)
for (fl in (2:5)) cat(fleet[fl],'\n',file=out,append=TRUE)

out<-file.path(NopInput,'fleet_catch.in') 
survey$V8 <- survey$V8+survey$V9; survey$V9<- NULL
cat('# from SeaSAM\n',file=out)  
for (fl in (2:5) ) {
  cat(" ######### fleet ",fleet[fl],'\n',file=out,append=TRUE)
  x<-subset(survey,V3==fl & V1>=yrange[1])
  head(x)
  x<-x[,colSums(x)>0]
  head(x)
  x$V4<- 1E-04
  v99 <- paste("  # ",x$V1)
  x<-subset(x,select=c(-V1, -V2, -V3))
  keep<-rowSums(x)>1
  x<-x[keep,]
  x$v99 <-v99[keep]
  head(x)
  write.table(x,file=out,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE)
}
cat("-999 #  Check value\n",file=out,append=T)


P1<-function(x,out,mult,shift=FALSE) {
  out<-file.path(NopInput,out)
  cat('# from SeaSAM\n',file=out)
  if (shift) {
    x[x$V2==4,5:8] <- x[x$V2==4,4:7] # Shift ages
    x[x$V2==4,4] <- 0
    x[x$V2==4,'V1'] <-  x[x$V2==4,'V1'] +1   #year
    x$V2<-x$V2+1  # quarter
    x[x$V2==5,'V2']<-1
  }
  for (y in (yrange)){
      cat("# ",y,'\n',file=out,append=T)
      for (q in (1:4)) {
        v<-subset(x,V1==y & V2==q,select=c(V4,V5,V6,V7,V8))  *mult
        if (out=='canum.in') v$V7 <- v$V7+v$V8
        v$V8 <- NULL
        cat(paste(v),'\n',file=out,append=T)
      }
  }
  cat("-999 #  Check value\n",file=out,append=T)
}  
P1(cn,out='canum.in',mult=1000,shift=do.shift)
P1(cw,out='weca.in',mult=0.001,shift=do.shift)


P2<-function(x,out,mult=1,shift=FALSE) {
  out<-file.path(NopInput,out)
  cat('# from SeaSAM\n',file=out)
  if (shift) {
    x[x$V2==4,4:7] <- x[x$V2==4,3:6] # Shift ages
    x[x$V2==4,3] <- 0
    x[x$V2==4,'V1'] <-  x[x$V2==4,'V1'] +1   #year
    x$V2<-x$V2+1  # quarter
    x[x$V2==5,'V2']<-1
  }
  
  for (y in (yrange)){
    cat("# ",y,'\n',file=out,append=T)
    for (q in (1:4)) {
      v<-subset(x,V1==y & V2==q,select=c(V3,V4,V5,V6))*mult
      cat(paste(v),'\n',file=out,append=T)
    }
  }
  cat("-999 #  Check value\n",file=out,append=T)
}  
P2(sw,out='west.in',mult=0.001,shift=do.shift)
P2(nm,out='natmor.in',shift=do.shift)
P2(mo,out='propmat.in',shift=FALSE)
