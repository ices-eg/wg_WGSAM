inp<-file.path("C:","MV","SMS","Data_baltic_WB","data-2014-one-area")

a<-read.table(file<-file.path(inp,"mean_W_and_l.dat"),header=T)

b<-round(tapply(a$mean_w,list(a$quarter,a$age),sum),4)
b<-cbind(rep(0,4),b,rep(0,4))
write.table(b,row.names=F,col.names=F)

out<-file.path(inp,'mean_W.in')
cat("# Whiting mean weight\n",file=out)
for (y in (1977:2012) ) {
  cat("# whiting ",y,"\n",file=out,append=T)
  write.table(b,row.names=F,col.names=F,file=out,append=T)
}


b<-round(tapply(a$mean_l,list(a$quarter,a$age),sum)*10,0)
b<-cbind(rep(0,4),b,rep(0,4))
write.table(b,row.names=F,col.names=F)

out<-file.path(inp,'mean_l.in')
cat("# Whiting mean length\n",file=out)
for (y in (1977:2012) ) {
  cat("# Whiting ",y,"\n",file=out,append=T)
  write.table(b,row.names=F,col.names=F,file=out,append=T)
}


a<-read.table(file<-file.path(inp,"N_whg.dat"),header=T)
out<-file.path(inp,'other_pred_N.in')

cat("# Whiting stock numbers\n",file=out)
for (y in (1977:2012) ) {
  cat("# whiting ",y,"\n",file=out,append=T)
  b<-droplevels(subset(a,year==y))

  b<-round(tapply(b$N,list(b$quarter,b$age),sum),4)
  b[is.na(b)]<-0
  b<-cbind(rep(0,4),b,rep(0,4))
    
  write.table(round(b),row.names=F,col.names=F,file=out,append=T)
}
