## Make file for inclusion of stomach observations in likelihood
#################################################################

stom<-Read.stomach.data.start()
###############################################################
#Number of stomachs;
ns<-unique(subset(stom,select=c(Area,Predator,Predator.no,Predator.length,Predator.length.class,Year,Quarter, N.haul)))

tapply(ns$N.haul,list(ns$Area,ns$Predator.length,ns$Year,ns$Quarter,ns$Predator),sum,na.rm=T)


ns$one<-1
ns$pp<-paste(ns$Predator.length.class,'-',ns$Predator.length,sep='')
tab<-tapply(ns$N.haul,list(ns$Area,ns$Predator.length.class,ns$Year,ns$Quarter,ns$Predator.no),sum,na.rm=T)
tab[is.na(tab)]<-0
#tab[tab>0]<-1
tab
dt<-dim(tab)
cat("# Table for inclusion of stomach data in likelihood. 0=no use, >=1 use data\n",file = file.path(data.path,'incl_stom.in'))

for (d in (1:dt[1])) {
  cat(paste("# AREA: ",d,'\n#################################################\n'),file = file.path(data.path,'incl_stom.in'),append=T)
  cat("# ", as.numeric(unlist(dimnames(tab)[1])),'\n',file = file.path(data.path,'incl_stom.in'),append=T)
for (p in (1:dt[5])) {
 for (q in (1:dt[4])) {
  cat(paste("# ",  sp.names[p]," quarter:",q,'\n'),file = file.path(data.path,'incl_stom.in'),append=T)
  cat("# ", as.numeric(unlist(dimnames(tab)[2])),'\n',file = file.path(data.path,'incl_stom.in'),append=T)
  write.table(tab[d,,,q,p],row.names = FALSE,col.names=F,file = file.path(data.path,'incl_stom.in'),append=T)
  }
}}
