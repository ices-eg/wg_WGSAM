#a<-Read.other.predator.prediction()
a<-Read.other.predator()

scenario<-"HCR_1_deter_noadjust_test_01_HCR1_0_Rec0__2030";
usedOther<-"op_other_sp_var.out"
usedOther<-"op_other_sp.out"
scenario<-NA

if ( ! is.na(scenario)) {
 b<- Read.op.other.sp.var(dir=file.path(data.path,scenario),infile=usedOther)
 a$size<-NULL
 a<-rbind(a,b)
}

#hm<-subset(a,Species.n==13)
#nonFish<-c(1:8,13,14)   # North Sea
nonFish<- -99
nonFish<-c('Fulmar','Gannet','GBB. Gull','Grey seal','Guillemot','H. porpoise','Her. Gull','Kittiwake','Puffin','Razorbill') #biomass make no sense for non-fish

a$BIO<-a$N*a$west/1000
a[a$Species.n %in% nonFish,'BIO']<- a[a$Species.n %in% nonFish,'N']

a$BIO[a$BIO<=0]<-0
#a$BIO<-a$Other.bio

#write.csv(a, file = file.path(data.path,'Other_predators.csv'),row.names = FALSE)

if (F) {
  a<-subset(a,BIO>0)
  
  # to get a nice plot for Horse Mackerel
  hm<-subset(a,Species.n==13)
  hm$BIO<-0
  hm<-aggregate(BIO~Predator+Species.n+Year,sum,data=hm)
  hm1<-hm; hm1$Quarter<-1
  hm2<-hm; hm2$Quarter<-2
  hm3<-hm; hm3$Quarter<-3
  hm4<-hm; hm4$Quarter<-4
  hm<-rbind(hm1,hm2,hm3,hm4)
  aa<-subset(a,select=c(Predator,Species.n,Year,Quarter,BIO))
  aa<-rbind(hm,aa)
  a<-aggregate(BIO~Predator+Species.n+Year+Quarter,sum,data=aa)
  
  #tapply(a$consum,list(a$Year,a$Predator),sum)/tapply(a$BIO,list(a$Year,a$Predator),sum)/1000
}

cleanup()
#dev<-"print"
dev<-"screen"
dev<-'png'
if (makeAllGraphs) dev<-'png'
nox<-4
noy<-4

i<-0

by(a,list(a$Quarter,a$Species.n),function(x) {
    b<- tapply(x$BIO,list(x$Year),sum)
    b[is.na(b)]<-0
    if ((i %% (nox*noy))==0) {
      
      filename=file.path
      
      filename<-paste0("OtherPredators",'_',i)
      if (makeAllGraphs) filename=file.path(otherPred.dir,filename)
      newplot(dev,nox,noy,Portrait=TRUE,filename=filename);
      par(mar=c(3,5,3,2))
    }
    i<<-i+1
   # print(sp.names[x[1,'Species.n']])
   if (sp.names[x[1,'Species.n']] %in% nonFish) ylab='Abundance' else  ylab='Biomass'
    barplot(b,space=0.0,ylab=ylab)
    title(main=paste(x[1,'Predator']," Q:",x[1,'Quarter'],sep=''))
})

if (dev=='png') cleanup()


