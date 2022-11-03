
# library(devtools)
# devtools::install_github("r4ss/r4ss")

library(r4ss)

myreplist <- SS_output(dir='C:/_C_drev/SMS-git/Data_Baltic/2019-data/Cod/BFAS_Final')
# make a bunch of plots
# SS_plots(myreplist)

str(myreplist,1,list.len=200)

head(myreplist$natlen)
myreplist$lbins
myreplist$lbinspop

#head(myreplist$lendbase)
#head(myreplist$cpue)
#head(myreplist$age_comp_fit_table)
#tail(myreplist$condbase)
#summary(myreplist$condbase)


ALK<-myreplist$ALK
dim(ALK)  # length, age, fleet  (but no year)
a<-ALK[,,3]
a
colSums(a)  # adds up to one



head(myreplist$agedbase)
summary(myreplist$agedbase)

myreplist$Growth_Parameters

tail(myreplist$catage)
summary(myreplist$catage) #only commercial catches (no survey catches)

head(myreplist$M_at_age)
head(myreplist$Z_at_age)


head(myreplist$ageselex)

summary(myreplist$ageselex)


fa=0;la=15
conv<-function(x,fa=0,la=15,v.names='cn') {
  x<-subset(x,select=c("Yr","Seas","Fleet",as.character(fa:la)))
  colnames(x)<-c("Yr","Seas","Fleet",paste("age",as.character(fa:la),sep='_'))
  
  x<-reshape(x,direction='long',varying=list(4:(4+la-fa)),v.names=v.names)
  x$Age<-x$time-1
  x<-subset(x,select=c("Yr","Seas",'Fleet','Age',v.names))
  return(x)
}

ca<-myreplist$catage
ca<-conv(x=ca,v.names='cn')

wa<-myreplist$wtatage
wa<-conv(x=wa,v.names='w')
wa<-subset(wa,Yr>1945)
a<-tapply(wa$w,list(wa$Yr,wa$Age,wa$Seas,wa$Fleet),sum,na.rm=T)
matplot(x=as.numeric(dimnames(a)[[1]]),a[,,4,2],xlab='Year',ylab='Catch mean weight (kg) in Season 4',pch=as.character(as.hexmode(fa:la)))

matplot(x=as.numeric(dimnames(a)[[1]]),a[,1:4,4,2],xlab='Year',ylab='Catch mean weight (kg) in Season 4',pch=as.character(as.hexmode(fa:la)))


cw<-merge(x=ca,y=wa,by=c("Yr","Seas","Fleet","Age"),all.x=TRUE)  # catch by fleet 1 and 3 (active and passive gears)
cw$cw<-cw$cn*cw$w
summary(cw)

cw<-aggregate(cw~Yr+Age,sum,na.rm=TRUE,data=cw)
a<-tapply(cw$cw,cw$Yr,sum)

#compare with catch input
cb<-myreplist$catch 
b<-tapply(cb$Obs,cb$Yr,sum)

a<-a[as.character(1946:2019)]
b<-b[as.character(1946:2019)]
cbind(a,obs=b,a/b)  # the same!


# biomas at age
ba<-myreplist$batage
cba<-colnames(ba)
cba<-gsub("Beg/Mid","BM",cba)
colnames(ba)<-cba
ba<-subset(ba,BM=='B' & Yr>=1946 & Era=='TIME')
summary(ba)
ba$Fleet<- -9
head(ba)
unique(ba$Era)

ba<-conv(x=ba,v.names='bio')
head(ba)

# N at age
na<-myreplist$natage
cna<-colnames(na)
cna<-gsub("Beg/Mid","BM",cna)
colnames(na)<-cna
na<-subset(na,BM=='B' & Yr>=1946 & Era=='TIME')
summary(na)
na$Fleet<- -9
head(na)
unique(na$Era)

na<-conv(x=na,v.names='n')
head(na)
a<-tapply(na$n,list(na$Yr,na$Age,na$Seas),sum)
a
matplot(x=as.numeric(dimnames(a)[[1]]),a[,,3],xlab='Year',ylab='Stock numbers in Season 3',pch=as.character(as.hexmode(fa:la)))


# get mean weight at age in the sea from biomass and numbers

sw<-merge(x=ba,y=na,by=c("Yr","Seas","Fleet","Age")) 

sw$sw<-sw$bio/sw$n
write.csv(sw,file='SS3_results_at_age.csv',row.names =FALSE)

sw$Fleet<-NULL
summary(sw)
a<-tapply(sw$sw,list(sw$Yr,sw$Age,sw$Seas),sum)
a
matplot(x=as.numeric(dimnames(a)[[1]]),a[,,1],xlab='Year',ylab='Stock mean wight (kg) in Season 1',pch=as.character(as.hexmode(fa:la)))

# M
m<-myreplist$M_at_age
m$Fleet<- -9
m$Seas<-9
head(m)

m<-conv(x=m,v.names='m')
head(m)
a<-tapply(m$m,list(m$Yr,m$Age),sum)
a
matplot(x=as.numeric(dimnames(a)[[1]]),a[,],xlab='Year',ylab='M at age',pch=as.character(as.hexmode(fa:la)))


# Data for SMS (all by length classess)

str(myreplist,1,list.len=200)
nl<-myreplist$lbinspop    # length bins used for population
write.csv(nl,file='lengthBin.csv',row.names =FALSE)


convL<-function(x,v.names='n') {
  # test:  x<-N
  x<-subset(x,x[,"Beg/Mid"]=='M')
  x<-subset(x,select=c("Yr","Seas",as.character(nl)))

  x<-reshape(x,direction='long',varying=list(3:(2+length(nl))),v.names=v.names)
  x$Length<-nl[x$time]
  x<-subset(x,select=c("Yr","Seas",'Length',v.names))
  return(x)
}


# head(myreplist$natlen)
N<-convL(x=myreplist$natlen,v.names='n')

tst<-subset(N,Length==30 & Seas==1)
plot(tst$Yr,tst$n,type='b')

bio<-convL(x=myreplist$batlen,v.names='bio') 
nb<-merge(N,bio)
nb$w<-nb$bio/nb$n
head(nb)

plot(nb$Length,nb$w,col=nb$Seas)


head(nb)
write.csv(nb,file='SS3_results.csv',row.names =FALSE)


