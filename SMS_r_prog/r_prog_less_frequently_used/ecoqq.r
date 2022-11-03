nsp<-23
noth<-14
fy.VPA<-1975
ly.VPA<-2005
fa<-0
la<-10

fy.for<-2006
ly.for<-2030
ny<-ly.for-fy.for+1

file<-file.path(data.path,'Length_weight_relations.in')
lw<-read.table(file,header=F)
lw<-data.frame(Species.n=as.factor(seq(1,dim(lw)[1])),a=lw$V1,b=lw$V2)

file<-file.path(data.path,'west.in')
west<-array(scan(file,comment.char = "#"),dim=c(la-fa+1,4,ly.VPA-fy.VPA+1,nsp),dimnames=list(fa:la,1:4,fy.VPA:ly.VPA,1:nsp))
op<-arr2dfny(west)
west<-data.frame(Age=op$index.1,Quarter=op$index.2,Year=op$index.3,Species.n=(op$index.4),W=op$y)

file<-file.path(data.path,'other_pred_N.in')
op<-array(scan(file,comment.char = "#"),dim=c(la-fa+1,4,ly.VPA-fy.VPA+1,noth),dimnames=list(fa:la,1:4,fy.VPA:ly.VPA,1:noth))
op<-arr2dfny(op)
op<-data.frame(Age=op$index.1,Quarter=op$index.2,Year=op$index.3,Species.n=(op$index.4),oth.N=op$y)

other<-merge(west,op)
other<-subset(other,other$oth.N>0 )
other<-data.frame(Age=as.factor(other$Age),Quarter=as.factor(other$Quarter),
            Year=as.factor(other$Year),Species.n=as.factor(other$Species.n),N=other$oth.N,W=other$W)

#tapply(other$W*other$N,list(other$Species.n,other$Year),sum)

ol<-subset(other,Year==ly.VPA & Species.n %in% as.factor(seq(1,noth)),select=-Year,drop=T)
tapply(ol$W*ol$N,list(ol$Species),sum)

l.set<-dim(ol)[1]

oo<-data.frame(Species.n=rep(ol$Species.n,ny),Age=rep(ol$Age,ny),Quarter=rep(ol$Quarter,ny),N=rep(ol$N,ny),
              W=rep(ol$W,ny),Year=as.factor(rep(seq(fy.for,ly.for),each=l.set)))

other<-rbind(other,oo)

b<-tapply(other$N*other$W,list(other$Species.n,other$Year),sum)   # just checking
#barplot(b)

file<-file.path(data.path,'summary.out')
s<-read.table(file,header=TRUE)
s<-subset(s,select=-c(M1,M2,M,F,Z,N.bar,C.hat,C.obs,weca,Yield,propmat,BIO,SSB))
s<-data.frame(subset(s,select=-west),W=s$west)
#head(s)
#tapply(s$W,list(s$Species.n,s$Year),mean)
so<-rbind(other,s)

#tapply(so$W,list(so$Species.n,so$Year),mean)

#b<-tapply(so$N*so$W,list(so$Species.n,so$Year),sum)   # just checking
#barplot(b)

# just because Alex is using an old version
Read.species.names<-function(dir=data.path)
{
  file<-file.path(dir,'Species_names.in')
  s<-readLines(file)
  for (i in (1:10)) s<-sub('_',' ',s)
  s<-sub('[[:space:]]+$', '', s)
}
sp<-Read.species.names()
sp<-sp[1:nsp]

file<-file.path(data.path,'mcout_N.out')
N<-read.table(file,header=TRUE)
N2<-aggregate(N$N,list(Species.n=N$Species.n,Year=N$Year,Quarter=N$Quarter,Age=N$Age),mean)
names(N2)<-c("Species.n","Year","Quarter","Age","N")

w2<-subset(west,Year==ly.VPA & W>0 & Species.n %in% as.factor(seq((noth+1),nsp)) ,select=-Year)
dim(w2)
 head(w2)
 
NW<-merge(N2,w2)

#tapply(NW$N*NW$W,list(NW$Species.n,NW$Year),sum)   # just checking

#tapply(NW$W,list(NW$Species.n,NW$Year),mean)   # just checking

all<-rbind(so,NW)
#tapply(all$N*all$W,list(all$Year,all$Species,all$Quarter),sum)   # just checking

NW<-merge(all,lw)  # add l-w relation
sp<-data.frame(sp)
ssp<-data.frame(Species.n=as.factor(seq(1,23)),Species=sp$sp)

NWL<-merge(ssp,NW)  # add species names
head(NWL)

#NW<-subset(NW,b>2,drop=T) # minus birds
#tapply(NWL$N*NWL$W,list(NWL$Year,NWL$Species),sum)   # just checking

NWL<-data.frame(NWL,L=(NWL$W/NWL$a)^(1/NWL$b)/10,NW=NWL$N*NWL$W)
NWL<-data.frame(NWL,size=ifelse(NWL$L>40," >40","<=40"))

a<-tapply(NWL$L,list(NWL$Year,NWL$Quarter,NWL$Species,NWL$Age),mean)   # just checking
round(a["2005","1",,])
a<-tapply(NWL$W,list(NWL$Year,NWL$Quarter,NWL$Species,NWL$Age),mean)   # just checking
round(a["2005","1",,],digits=3)



#incl.sp<-c("Fulmar","Guillemot","Her. Gull","Kittiwake","GBB. Gull","Gannet","Puffin","Razorbill","R. radiata",
#            "G. gurnards","W. mackerel", "N. mackerel", "W.horse mac", "N.horse mac","Cod","Whiting","Haddock",
#            "Saithe","Herring","Sandeel","Nor. pout","Plaice","Sole")

incl.sp<-c("R. radiata",
            "G. gurnards","W. mackerel", "N. mackerel", "W.horse mac", "N.horse mac","Cod","Whiting","Haddock",
            "Saithe","Herring","Sandeel","Nor. pout","Plaice","Sole")

#incl.sp<-c("Cod","Whiting","Haddock","Saithe","Nor. pout","Plaice","Sole")
#incl.sp<-c("Cod","Haddock","Saithe","Plaice")

#incl.sp<-c("R. radiata","G. gurnards","W. mackerel", "N. mackerel", "W.horse mac", "N.horse mac")

ploteco<-function(q=1){
  out<-subset(NWL, Species %in% incl.sp & Quarter==q)
  NP<-aggregate(out$NW,list(Year=out$Year,Quarter=out$Quarter,size=out$size),sum)
  NP.all<-aggregate(out$NW,list(Year=out$Year,Quarter=out$Quarter),sum)
  names(NP.all)<-c("Year","Quarter","sum")
  NP.prop<-merge(NP,NP.all)
  NP.prop<-data.frame(NP.prop,prop=NP.prop$x/NP.prop$sum)
  b<- tapply(NP.prop$prop,list(NP.prop$size,NP.prop$Year),sum)
  b[is.na(b)]<-0
  barplot(b,col=c("red","white"),main=paste("Q",q))
  abline(h=0.3)
  abline(h=0.2)
  abline(h=0.1)

}

cleanup()

dev<-"wmf"
#dev<-"screen"
nox<-2
noy<-2
newplot(dev,nox,noy,Portrait=TRUE,w8=9,w11=11);
ploteco(1)
ploteco(2)
ploteco(3)
ploteco(4)

cleanup()
q<-1
  out<-subset(NWL, Species %in% incl.sp & Quarter==1 & Year %in% c(1975,1985,1995,2000,2005,2010))
  NP<-aggregate(out$NW,list(Year=out$Year,Quarter=out$Quarter,Species=out$Species,size=out$size),sum)
  NP.all<-aggregate(out$NW,list(Year=out$Year,Quarter=out$Quarter,Species=out$Species),sum)
  names(NP.all)<-c("Year","Quarter","Species","sum")
  NP.prop<-merge(NP,NP.all)
  NP.prop<-data.frame(NP.prop,prop=NP.prop$x/NP.prop$sum)
  b<- tapply(NP.prop$prop,list(NP.prop$size,NP.prop$Year,NP.prop$Species),sum)
  b[is.na(b)]<-0
op<-arr2dfny(b)
prop<-data.frame(size=op$index.1,Year=op$index.2,Species=op$index.3,prop=op$y)
barchart(prop~Year|Species,groups=size,stack=T,data=prop,ylab='Proportion')
