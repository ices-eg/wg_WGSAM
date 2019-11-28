# read binary admodel.cov file
file<-file(file.path(data.path,"admodel.cov"), "rb")
# read dimension
dim<-readBin(file,integer(),1)
# read values 
COV<-matrix(readBin(file, numeric(), dim*dim), dim, dim)
close(file)
COV[1:8,1:8]


#Open SMS correlation file
file<-file.path(data.path,"sms.cor")
ofil<-file.path(data.path,"sms_out.cor")

a<-readLines(file)

len<-length(a)
a<-a[3:len]
len<-len-2
write.table(a,file=ofil,quote=FALSE,col.names=FALSE,row.names=FALSE)

var.name<-substr(a,start=10,stop=23)

b<-substr(a,start=49,stop=1e5)
write.table(b,file=ofil,quote=FALSE,col.names=FALSE,row.names=FALSE)
c<-scan(file=ofil)
cor<-matrix(NA,nrow=len,ncol=len)

j<-1
for (i in (1:len)) {
 cor[i,1:(i)]<-c[j:(j+i-1)]
 j<-j+i
}

cor[1:8,1:8]

# fill the upper triangle
for (i in (1:len)) {
  for (j in (min(i+1,len):len)) {
    cor[i,j]<-cor[j,i]
  }
}

cor[1:8,1:8]

# read SMS.std 
a<-read.table(file.path(data.path,"sms.std"),comment.char = "#",header=FALSE,skip=1) 
std<-a$V4
value<-a$V3
var.name.std<-a$V2

cov<-matrix(NA,nrow=len,ncol=len)  # co-variance 
for (i in (1:len)) {
  for (j in (1:len)) {
    if (i!=j) cov[i,j]=cor[i,j]*std[i]*std[j]
  }
}
diag(cov)<-std^2

cov[1:8,1:8]
COV[1:8,1:8]

 COV[1:8,1:8]/cov[1:8,1:8]
 
cov[1:8,1:8]/  COV[1:8,1:8]

round(diag(cov[1:dim(COV)[1],1:dim(COV)[1]])/diag(COV))

#test
cor/cov2cor(cov)  # my covariance matrix produces a correlation matrix as produced by ADMB , OK


aa<-cov2cor(COV)
aa[1:8,1:8]   # the same as sms.cor 


west<-c(  0.05319, 0.07434, 0.10004, 0.10624, 0.11971, 0.13278, 0.14604, 0.15998, 0.19279, 0.20853 )
pm<-c(   0.110,    0.400,    0.820,    0.860,    0.910,    0.940,    1.000,    1.000,    1.000,    1.000)
M<-rep(0.2,10)


##############################################
# First simple test. SSB in the first year after last assessment year

vars<- 'term_N'
my.var<-grep(vars,var.name)
final.cov<-cov[my.var,my.var]
dimnames(final.cov)[2]<-list(var.name[my.var])
dimnames(final.cov)[1]<-dimnames(final.cov)[2]
final.cov
 
pars<-value[grep(vars,var.name.std)]   # N


SSB<-function(pars) {
 sum(pars*west*pm)
}

# partial derivatives
grad<-rep(NA,length(pars))
B1<-SSB(pars)
delta<-0.01
for (i in (1:length(pars))) {
  localPar<-pars
  localPar[i]<- localPar[i]*(1.0+delta)
  B2<-SSB(localPar)
  grad[i]<-(B2-B1)/(delta*pars[i])
}


# uden covariance
varSSB<-sum(diag(final.cov)*grad^2)

sms.est.std<- std[grep('next_SSB',var.name.std)][1]   # SMS std estimate
sqrt(varSSB)/ sms.est.std   # compare

# med covariance
var2<-t(grad) %*% final.cov %*% grad
sqrt(var2)/ sms.est.std   # compare

# uden covariance, ved at sætte cov til 0

cov2<-final.cov
cov2[upper.tri(cov2)]<-0
cov2[lower.tri(cov2)]<-0

varSSB2<-t(grad) %*% cov2 %*% grad

varSSB/varSSB2

##############################################
# Second simple test. SSB in the second year after last assessment year

fa<-1
la<-10
ages<-la-fa+1

vars<- 'term'
my.var<-grep(vars,var.name)
final.cov<-cov[my.var,my.var]
dimnames(final.cov)[2]<-list(paste(rep(seq(fa,la,1),2),var.name[my.var]))
dimnames(final.cov)[1]<-dimnames(final.cov)[2]
final.cov
 
pars<-value[grep(vars,var.name.std)]   # N

SSB<-function(pars) {
 N<-pars[1:ages]
 N2<-rep(NA,ages)
 i<-ages+1
 Fi<-pars[i:(i+ages)]
 for (a in ((fa:(la-1))))  N2[a+1]<-N[a]*exp(-Fi[a]-M[a])
 N2[la]<-N2[la]+N[la]*exp(-Fi[la]-M[la])  #plusgroup
 N2[fa]<-N[fa]   # recruits
 sum(N2*west*pm)
}

B1<-SSB(pars)

# partial derivatives
grad<-rep(NA,length(pars))
B1<-SSB(pars)
delta<-0.01
for (i in (1:length(pars))) {
  localPar<-pars
  localPar[i]<- localPar[i]*(1.0+delta)
  B2<-SSB(localPar)
  grad[i]<-(B2-B1)/(delta*pars[i])
}


# uden covariance
varSSB<-sum(diag(final.cov)*grad^2)

sms.est.std<- std[grep('next_SSB',var.name.std)][2]   # SMS std estimate
sqrt(varSSB)/ sms.est.std   # compare

# med covariance
var2<-t(grad) %*% final.cov %*% grad
sqrt(var2)/ sms.est.std   # compare


# rate sensitivity coefficients
sens<-grad*diag(final.cov)/ varSSB
sens2<-sens[order(abs(sens))]
barchart(sens2)

#partial Variance

p<-grad^2*diag(final.cov)/ varSSB
p
p1<-p[order(p)]
sepa<-0.8
p2<-c(sum(p1[1:(length(p1)*sepa)]),p1[(length(p1)*sepa+1):length(p1)])
names(p2)[1]<-"other"
pie(p2)



#RSiteSearch("error propagation")
#Pakke 
#propagate {qpcR}