a<-Read.MCMC.detailed.data()
a<-subset(a,Year==2012,select=c(-Repetion, -Iteration))
names(a)
#[1] "Species"   "Species.n" "Year"      "Quarter"   "Age"       "N"  "F"         "M2"

# data used to update
aa<-subset(a,Quarter==1)
aa<-tapply(aa$N,list(aa$Species.n,aa$Age),sum)
cat('stock numbers at age, Remember to insert age 0=0\n')
aa*1000

M2.a<-tapply(a$M2,list(a$Species,a$Quarter,a$Age),sum)
N.a<-tapply(a$N,list(a$Species,a$Quarter,a$Age),sum)
ftable(round(M2.a,3))

b<-Read.summary.data(infile="OP_summary.out",read.init.function=F)
b<-subset(b,Year==2012)
M2.b<-tapply(b$M2,list(b$Species,b$Quarter,b$Age),sum)
ftable(round(M2.b,3))

N.b<-tapply(b$N,list(b$Species,b$Quarter,b$Age),sum)

ftable(round(N.a,0))
ftable(round(N.b/1000,0))

ftable(round(M2.a,3))
ftable(round(M2.b,3))


ftable(round(tapply(a$F,list(a$Species,a$Age),sum),3))
ftable(round(tapply(b$F,list(b$Species,b$Age),sum),3))
