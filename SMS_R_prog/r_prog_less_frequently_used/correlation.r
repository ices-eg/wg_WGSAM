
 # max correlation
parm<-"estParm"         # select estParm (estimated) or allParm (estimated plus derived variables)

################################

fit<-read.fit()
 cor<-fit$cor
if (parm=="estParm") {
  npar<-fit$nopar
  cor<-cor[1:npar,1:npar]
} else npar<-fir$npar

var.name<-seq(1,npar)
dimnames(cor)[2]<-list(var.name)
dimnames(cor)[1]<-list(var.name)

cor[cor==1]<-NA
cor[lower.tri(cor)]<-NA

n<-dim(cor)[1]
size<-n*n

a<- arr2dfny(cor,name="cor")
#a<-subset(a,is.na(cor)==F & cor!=0)
a<-subset(a,is.na(cor)==F )

b<-read.table(file.path(data.path,"par_exp.out"),comment.char = "#",header=T)
namesb<-names(b)
names(b)<-paste(namesb,"1",sep='')

tmp<-merge(a,b,by.x="index.1",by.y="parNo1")
head(tmp)
names(b)<-paste(namesb,"2",sep='')
tmp<-merge(tmp,b,by.x="index.2",by.y="parNo2")

subset(tmp,par1=='qq_s2_ini')

head(tmp)
a<-droplevels(subset(tmp,par1=='qq_s2_ini' & par2=='qq_s2_ini'))
tapply(a$cor,list(paste(a$species1,a$fleet1,a$age1),paste(a$species2,a$fleet2,a$age2)),sum)

spnames<-function(sp) {
 if (sp >0) s<-sp.names[sp] else {if (sp<0) s<-'NA' else if (sp==0) s<-'Other'}
 s
}
spnames<-c("NA","Other",sp.names)

if (nsp==1) {
   ab<-data.frame(par1=tmp$par1,par2=tmp$par2,cor=tmp$cor,year1=tmp$year1,year2=tmp$year2,q1=tmp$quarter1,q2=tmp$quarter2,age1=tmp$age1,age2=tmp$age2)
} else {
   ab<-data.frame(par1=tmp$par1,par2=tmp$par2,cor=tmp$cor,species1=spnames[tmp$species1+2],species2=spnames[tmp$species2+2],
      year1=tmp$year1,year2=tmp$year2,q1=tmp$quarter1,q2=tmp$quarter2,age1=tmp$age1,age2=tmp$age2,
      pred1=spnames[tmp$predator1+2],prey1=spnames[tmp$prey1+2],pred2=spnames[tmp$predator2+2],prey2=spnames[tmp$prey2+2])
}

cors<-ab[order(abs(ab$cor),decreasing = T),]
print(head(cors,10))

head(subset(cors,par2=="init_season_overlap"))

