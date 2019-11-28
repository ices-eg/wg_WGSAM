#library(RColorBrewer)

first.year<- 1974                #first year on plot, negative value means value defined by data
last.year<- 2030 #2016                 #last year on plot

OperatingModel<-T;   # include data from forecast (the OP )
redefine.scenario.manually<-FALSE

output.dir<-data.path
op.dir<-data.path
if (OperatingModel==T & redefine.scenario.manually==T)  {
  scenario<-"test"; 
  output.dir<-data.path 
  op.dir<-file.path(data.path,"HCR_1_deter_noadjust_test_01_HCR1_0_Rec0__2030")
  #op.dir<-data.path
} else if (OperatingModel==T & redefine.scenario.manually==FALSE) {
  output.dir<-scenario.dir 
  op.dir<-scenario.dir
} 

if (!OperatingModel) dat<-Read.summary.data(read.init.function=F)
if (OperatingModel) {
  dat1<-Read.summary.data(extend=F,read.init.function=F)
  
  dat<-Read.summary.data(dir=op.dir,infile="op_summary.out",read.init.function=F)

  dat$N.bar<-dat$N*(1-exp(-dat$Z))/dat$Z
  dat$C<-NULL
  dat$N_dist<-NULL
  dat$Area<-NULL
  dat1<-subset(dat1,select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,BIO,SSB))
  dat <-subset(dat, select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,BIO,SSB))
  
  dat<-rbind(dat1,dat)
}
#tapply(dat$Yield,list(dat$Year,dat$Species),sum)





dat<-subset(dat,Year<=last.year )
if (first.year>0) dat<-subset(dat,Year>=first.year )

dat<-data.frame(dat,deadM2=dat$M2*dat$N.bar*dat$west,deadM=dat$M*dat$N.bar*dat$west)
dat<-subset(dat,select=c(Species, Year, Quarter, Species.n, Age, M2,deadM2))

M2<-Read.part.M2.data()

if (OperatingModel) {
  M2b<-Read.part.M2.OP.prediction.data(dir=op.dir)
  M2b$Area<-NULL
  M2<-rbind(M2,M2b)
}



a<-merge(x=dat,y=M2, by.x = c("Year","Quarter","Species","Age"), by.y = c("Year","Quarter","Prey","Prey.age"))
a$eatenW<- a$deadM2*a$Part.M2/a$M2

a$Prey<-a$Species
a$Prey.age<-a$Age
a$tot.M2.prey<-a$M2

b<-subset(a,select=c( Year, Quarter, Predator,Predator.age, Prey, Prey.age,Prey.no, eatenW, Part.M2,tot.M2.prey))
write.table(b,file=file.path(data.path,'who_eats_whom_level1.csv'),sep=',',row.names = F)

bb<-droplevels(aggregate(list(eatenW=b$eatenW),list(Year=b$Year, Quarter=b$Quarter, Predator=b$Predator,Prey=b$Prey),sum))
write.table(bb,file=file.path(data.path,'who_eats_whom_level2.csv'),sep=',',row.names = F)

bbb<-droplevels(aggregate(list(eatenW=b$eatenW),list(Year=b$Year, Predator=b$Predator,Prey=b$Prey,Prey.no=b$Prey.no),sum))
write.table(bbb,file=file.path(data.path,'who_eats_whom_level3.csv'),sep=',',row.names = F)
bbb$eatenW<-bbb$eatenW/1000
ftable(round(tapply(bbb$eatenW/1000,list(bbb$Year,bbb$Prey,bbb$Predator),sum,na.rm=T),0))

file<-file.path(data.path,'pred_format.dat')
s<-read.table(file,header=TRUE)
pformat<-unique(subset(s,new.no<99,select= -Predator))
pformat<-pformat[order(pformat$new.no),]
pformat<-as.character(pformat$new)
s<-merge(x=bbb,y=s,by='Predator',all.y=T)
s<- subset(s,!is.na(s$eatenW))
head(s)
a<-aggregate(s$eatenW,list(s$new,s$Year,s$new.no,s$Prey,s$Prey.no),sum)
names(a)<-c("Predator","Year","Predator.no","Prey","Prey.no","eatenW")
head(a)

write.table(a,file=file.path(data.path,'who_eats_whom_combined.csv'),sep=',',row.names = F)

cleanup()
dev<-"screen"
#dev<-"png"
nox<-2
noy<-3

if (makeAllGraphs) {
  dev<-'png'
  nox<-2
  noy<-3
}

i<-0
all.pred.col<-sort(unique(a$Predator.no),decreasing = TRUE)
all.prey.col<-sort(unique(a$Prey.no),decreasing = TRUE)
all.names<-rep('aaa',length(all.pred.col))

new.names<-pformat
for (s in (1:length(all.pred.col))) all.names[s]<-new.names[all.pred.col[s]]

palette(rainbow(length(new.names)))
#palette(cm.colors(length(new.names)))
my.colors<-c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )
my.colors<-my.colors[1:length(new.names)]
palette(my.colors)



b<- tapply(a$eatenW,list(a$Predator.no,a$Year),sum)
b[is.na(b)]<-0
pred.no<-as.numeric(dimnames(b)[[1]])
v<-pred.no
for (l in (1:length(v))) pred.no[l]<-v[length(v)-l+1]
pred.names<-rep('aaa',length(pred.no))
v<-pred.names
for (l in (1:length(v))) pred.names[l]<-v[length(v)-l+1]
for (s in (1:length(pred.no))) pred.names[s]<-new.names[pred.no[s]]

i<<-0
by(a,list(a$Prey),function(x) {

    b<- tapply(x$eatenW,list(x$Predator.no,x$Year),sum)
    b[is.na(b)]<-0
    pred.no.age<-as.numeric(dimnames(b)[[1]])
    pred.names.age<-rep('aaa',length(pred.no.age))
    for (s in (1:length(pred.no.age))) pred.names.age[s]<-new.names[pred.no[s]]


    length.names<-dimnames(b)[[2]]
    if ((i %% (nox*noy-1))==0) {
      filename<-paste("WhoEats",'_',x[1,]$Prey,sep='')
      if (makeAllGraphs) filename=file.path(whoEatsWhom.dir,filename)
       newplot(dev,nox,noy,w8=8,w11=11,Portrait=TRUE,pointsize=12,filename=filename);
      par(mar=c(3,2,2,1))  #  c(bottom, left, top, right)
      plot.new(); legend(x=0,y=1,pred.names,fill=pred.no,cex=0.8,col=pred.no,ncol=1)
      }
    i<<-i+1

    barplot(b,space=0.4,names=length.names,col=pred.no.age)
    title(main=paste(x[1,]$Prey))
})

if (dev %in% c('png','print')) cleanup()

# cleanup()
nox<-2;noy=1
#aa<-droplevels(subset(a,Prey %in% c('Sprat','Nor. pout','Herring')))
aa<-a
by(aa,list(aa$Prey),function(x) {

    b<- tapply(x$eatenW,list(x$Predator.no,x$Year),sum)
    b[is.na(b)]<-0
    pred.no.age<-as.numeric(dimnames(b)[[1]])
    pred.names.age<-rep('aaa',length(pred.no.age))
    for (s in (1:length(pred.no.age))) pred.names.age[s]<-new.names[pred.no[s]]


    length.names<-dimnames(b)[[2]]
    if ((i %% (nox*noy-1))==0) {
      filename<-paste("WhoEatsOne",'_',x[1,]$Prey,sep='')
      if (makeAllGraphs) filename=file.path(whoEatsWhom.dir,filename)
      newplot(dev,nox,noy,w8=12,w11=7,Portrait=TRUE,pointsize=12,filename=filename);
      par(mar=c(3,3,2,1))  #  c(bottom, left, top, right)
      plot.new(); legend(x=0,y=1,pred.names,fill=pred.no,cex=1.4,col=pred.no,ncol=1,title='Predator')
      }
    i<<-i+1
    par(mar=c(3,4,2,1))  #  c(bottom, left, top, right)
    barplot(b,space=0.4,names=length.names,col=pred.no.age,ylab='Eaten biomass (1000t)')
    title(main=paste(x[1,]$Prey))
})


if (dev %in% c('png','print')) cleanup()


aa<-droplevels(aggregate(eatenW~Predator+Year,data=a,sum,na.rm=T))
tot<-tapply(aa$eatenW,list(aa$Predator,aa$Year),sum)
tot[is.na(tot)]<-0
nox<-1;noy=1
newplot(dev,nox,noy,w8=12,w11=7,Portrait=TRUE,pointsize=12,filename=paste("WhoEats_AllPredators",sep=''));

barplot(tot,
  #col=rainbow(dim(tot)[1]),
  col=my.colors,
  ylab='biomass eaten (1000 t)',
  legend =rownames(tot),args.legend=list(x="topright",ncol=2,title='Predator'))


if (dev %in% c('png','print')) cleanup()

filename<-paste("WhoEats_AllPredators_Relative",sep='')
if (makeAllGraphs) filename=file.path(whoEatsWhom.dir,filename)

newplot(dev,nox,noy,w8=12,w11=7,Portrait=TRUE,pointsize=12,filename=filename);

tot<-tot/rep(colSums(tot),each=dim(tot)[1])
barplot(tot, col=my.colors,ylab='Proportion eaten')

filename<-paste("WhoEats_AllPreys",sep='')
if (makeAllGraphs) filename=file.path(whoEatsWhom.dir,filename)
newplot(dev,nox,noy,w8=12,w11=7,Portrait=TRUE,pointsize=12,filename=filename);
aa<-droplevels(aggregate(eatenW~Prey+Year,data=a,sum,na.rm=T))
tot<-tapply(aa$eatenW,list(aa$Prey,aa$Year),sum)
tot[is.na(tot)]<-0
barplot(tot,
  #col=rainbow(dim(tot)[1]),
  col=my.colors,
  ylab='biomass eaten (1000 t)',
  legend =rownames(tot),args.legend=list(x="topright",ncol=2,title='Prey'))

if (dev %in% c('png','print')) cleanup()

filename<-"WhoEats_AllPreys_Realative"
if (makeAllGraphs) filename=file.path(whoEatsWhom.dir,filename)
newplot(dev,nox,noy,w8=12,w11=7,Portrait=TRUE,pointsize=12,filename=filename);

tot<-tot/rep(colSums(tot),each=dim(tot)[1])
barplot(tot, col=my.colors,ylab='Proportion eaten')


if (dev %in% c('png','print')) cleanup()

# biomass eaten by predator
if (FALSE) {
  cleanup()
  for (Pred in c('Cod','Hake')){
    X11()
    aaa<-subset(a,Predator==Pred)
    aa<-droplevels(aggregate(eatenW~Prey+Year,data=aaa,sum,na.rm=T))
    tot<-tapply(aa$eatenW,list(aa$Prey,aa$Year),sum)
    tot[is.na(tot)]<-0
    barplot(tot,
      #col=rainbow(dim(tot)[1]),
      col=my.colors,
      ylab='biomass eaten (1000 t)',
      legend =rownames(tot),args.legend=list(x="topright",ncol=2,title=paste('Preys eaten by',Pred)))
    X11()
    tot<-tot/rep(colSums(tot),each=dim(tot)[1])
    barplot(tot, col=my.colors,ylab='Proportion eaten')
  }
} 




