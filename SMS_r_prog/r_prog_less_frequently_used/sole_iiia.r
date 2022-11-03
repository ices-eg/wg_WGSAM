b<-read.table(file='sole_XSA_summary.dat',header=T)

Blim<-770


recs<-subset(b,SSB>=Blim)
mu<-mean(log(recs$Recruitment))
std<-sd(log(recs$Recruitment))


# alfa in SMS SSB-R
alfa<-log(exp(mu)/Blim)

arti.mean<-mean(b$Recruitment)
geo.mean<-exp(mean(log(b$Recruitment)))
sd(log(b$Recruitment))

cat("#model alfa  beta std\n",100,alfa,Blim,std,'\n', file='SSB_R.in')


