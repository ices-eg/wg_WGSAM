read.in.files<-function(years,quarters,areas,species,ages,filename) {

a<-expand.grid(Area=areas,Species.n=species,Year=years,Quarter=quarters,Age=ages)
a<-a[order(a$Area,a$Species.n,a$Year,a$Quarter,a$Age),]
a$variable<-scan(file.path(data.path,filename),comment.char='#')
a
}

west<-read.in.files(years=SMS.control@first.year:SMS.control@last.year,
              quarters=1:SMS.control@last.season,
              areas=1:SMS.control@no.areas,
              species=1:nsp,
              ages=SMS.control@first.age:SMS.control@max.age.all,
              filename="west.in")

names(west)<-c("Area","Species.n","Year","Quarter","Age","west")

consum<-read.in.files(years=SMS.control@first.year:SMS.control@last.year,
              quarters=1:SMS.control@last.season,
              areas=1:SMS.control@no.areas,
              species=1:npr,
              ages=SMS.control@first.age:SMS.control@max.age.all,
              filename="consum.in")

names(consum)<-c("Area","Species.n","Year","Quarter","Age","consum")

a<-merge(consum,west)
a<-subset(a,consum>0 & west>0)

a$log.consum<-log(a$consum)
a$log.west<-log(a$west)
a$Species<-sp.names[a$Species.n]
a$spQ<-paste(a$Species," Q:",a$Quarter,sep='')

trellis.device()
xyplot(log.consum~log.west|spQ,data=a,layout = c(2, 2),subset=(Year<=2000),
  scales = "free",
  panel=function(x,y) {
    panel.xyplot(x,y,col=1 ,pch=1,type='p')
    panel.loess(x,y, span=1)
    panel.lmline(x,y,col=2)
  }
)

b<-by(a,list(a$Species.n,a$Quarter),function(x) {
    a<-(lm(log.consum~log.west,data=x))
    list(Species.n=x[1,'Species.n'],Quarter=x[1,'Quarter'],a=exp(coef(a)[[1]]),b=coef(a)[[2]])
})

matrix(unlist(b),ncol=4,byrow=T)

