
cleanup()
incl.sp<-seq(15,23)                      # species number to be included. Numbers or "all"
incl.sp<-"all"
#incl.sp<-1
comp.like<-F

first.pch<-0    # first pch symbol
first.color<-1   # first color

if (F) {
  dirs<-c("bal-1-area-run-00","bal-1-area-run-00-test01","bal-1-area-run-00-test02","bal-1-area-run-00-test03")
  labels<-c("Run 00","test 01","test 02","test 03")
}
  
for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist')) #else cat(dir,'\n')
} 


Init.function() # get SMS.contol object  including sp.names
for (dir in dirs) {  
  a<-Read.objective.function(dir=file.path(root,dir),read.init.function=FALSE)
  a$dir<-dir
  a$label<-labels[ which(dirs==dir)]

 if (dir==dirs[1]) aa<-a else aa<-rbind(aa,a)

}

bb<-subset(aa,select = c(label,Species, catch, CPUE, SSB.Rec, stomachs,stomachs.N ,penalty))

bb2<-subset(bb,select=c(-Species,-label))
bb3<-aggregate(bb2,list(label=bb$label),sum)
bb3$all<- bb3$catch +bb3$CPUE+bb3$SSB.Rec+bb3$stomachs+bb3$penalty
#print(bb3,digits=1)

read.fit<-function(dir=data.path){
  # Function to read a basic fit
  k<-2
  parfile<-as.numeric(scan(file.path(dir,"sms.par"),
                      what='', n=16, quiet=TRUE)[c(6,11,16)])
  data.frame(n.par=as.integer(parfile[1]),
             neg.log.like=parfile[2],
             max.grad=parfile[3],
           AIC=round(k*parfile[1]+2*parfile[2],2)
           )
}

for (dir in dirs) {  
  a<-read.fit(dir=file.path(root,dir))
  a$dir<-dir
    a$label<-labels[ which(dirs==dir)]
 if (dir==dirs[1]) aa<-a else aa<-rbind(aa,a)
}

aa<-subset(aa,select=c(label,n.par,neg.log.like, max.grad,AIC))
if (comp.like) { 
  ll<-dim(aa)[[1]]
  aa$dif.like<-NA
  aa$dif.n.par<-NA
  for (i in (2:ll)) {
    aa$dif.like[i]<-abs(aa$neg.log.like[i]-aa$neg.log.like[i-1])
    aa$dif.n.par[i]<-abs(aa$n.par[i]-aa$n.pa[i-1])
  }
  aa$prob.likelihood.ratio<-NA
  for (i in (2:ll))aa[i,]$prob.likelihood.ratio <- 1-pchisq(2*aa[i,]$dif.like,aa[i,]$dif.n.par)
} 
aaa<-merge(bb3,aa,by='label')
print(aaa)

write.table(aaa,file=file.path(data.path,"compare_like.csv"),sep=',',row.names=F)

 