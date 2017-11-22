
dev<-"print"
dev<-"screen"
nox<-4; noy<-4;
paper<-FALSE        # graphics on paper=file (TRUE) or on screen (FALSE)
run.ID<-'B'         # file id used for paper output
cleanup()



first.year.on.plot<-1977
last.year.on.plot<-2030

incl.sp<-seq(1,14)  # species number to be included

#do.plot.other<-function(sp.plot=c(1),incl.prob=FALSE,incl.closure=FALSE,incl.0.group.M2=FALSE) {
#########################################
cleanup()
i<-0
oth<-Read.other.predator()
oth<-subset(oth,Year>=first.year.on.plot&Year<=last.year.on.plot)
oth<-tapply(oth$Other.bio/1000,list(oth$Species.n,oth$Year,oth$Quarter),sum)
y<-as.numeric(unlist(dimnames(oth)[2]))
q<-as.numeric(unlist(dimnames(oth)[3]))
for (sp in (incl.sp)) {
  if ((i %% (nox*noy-1))==0) {
    newplot(dev,nox,noy,Portrait=TRUE);
    par(mar=c(3,5,3,2)) 
  }    
  i<<-i+1
  plot(y,oth[sp,,1],ylab='biomass',main=name[sp+1],ylim=c(0,max(oth[sp,,])),col=1,type='l')
  for (qq in (q)) lines(y,oth[sp,,qq],col=qq)
}  
  
