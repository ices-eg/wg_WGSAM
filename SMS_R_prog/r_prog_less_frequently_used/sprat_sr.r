temp<-read.csv(file.path(data.path,'Temp_data_August.csv'))
a<-data.frame(year=1974:2010,SSB=gem.SSB,REC.obs=gem.REC.obs,REC.hat=gem.REC.hat)
a<-merge(a,temp)
a$residual<-a$REC.obs- a$REC.hat


cleanup()
X11()
 par(mfcol=c(2,2))
plot(a$year,a$temp,type='b')
plot(a$year,a$REC.obs,type='b')

plot(a$SSB,a$REC.obs,type='p')

X11()
 par(mfcol=c(2,2))
plot(a$year,a$residual,type='b')
plot(a$temp,a$residual,type='p')
plot(a$SSB,a$residual,type='p')
plot(a$SSB,a$REC.obs,type='p')


