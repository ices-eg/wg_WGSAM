
a<-Read.summary.data()
a<-subset(a,Species=='Sprat',select=c(Species,Year,Quarter,Age,F))
head(a)

F12<-subset(a,Age %in% c(1,2))
FMS<-tapply(F12$F,list(F12$Year),sum)/2


qShift<-function(a) {
  q12<-a$Quarter %in% c(1,2)
  q34<-a$Quarter %in% c(3,4)
  a[q12,'Quarter']<-a[q12,'Quarter']+2
  a[q34,'Quarter']<-a[q34,'Quarter']-2
  a[q12,'Year']<-a[q12,'Year']-1
  a[q12,'Age']<-a[q12,'Age']-1
  return(a)
}

b<-qShift(a)

F12b<-subset(b,Age %in% c(1,2))
head(F12)
head(F12b)
 

FSS<-tapply(F12b$F,list(F12b$Year),sum)/2

FMS
FSS

FSS<-tail(FSS,-1)
FMS/FSS
