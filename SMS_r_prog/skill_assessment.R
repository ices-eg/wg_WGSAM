
useLogValues<-FALSE

a<-Read.catch.survey.residuals()
a$logObs=log(a$observed)
a$logPred=log(a$model)
head(a)

b<-aggregate(cbind(meanO=observed,meanLO=logObs,meanLP=logPred)~data+Species+Quarter+fleet+Age,mean,data=a)
head(b)

a<-merge(x=a,y=b,all.x=TRUE)
head(a)
tail(a)


aa<-by(a,list(a$data,a$Species.n,a$Quarter,a$fleet,a$Age),simplify=FALSE,function(x) {
  r<-cor(x=x$logObs, y=x$logPred, method = c("pearson", "kendall", "spearman")[1])
  return(data.frame(Species=x[1,"Species"],Species.n=x[1,"Species.n"],Age=x[1,"Age"],Quarter=x[1,"Quarter"], fleet=x[1,"fleet"],r=r))
})
aa<-do.call(rbind,aa)
aa



aa<-by(a,list(a$data,a$Species.n,a$fleet),simplify=FALSE,function(x) {
  r<-cor(x=x$logObs, y=x$logPred, method = c("pearson", "kendall", "spearman")[1])
  return(data.frame(Species=x[1,"Species"],Species.n=x[1,"Species.n"], data=x[1,'data'],fleet=x[1,"fleet"],r=r))
})
aa<-do.call(rbind,aa)


