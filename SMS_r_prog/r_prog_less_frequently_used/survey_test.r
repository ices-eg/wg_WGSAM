n<-50
x<-1:n
y<-x

cleanup()
X11()
par( mfcol=c(2,2))
plot(x,y,type='b',main='True signal')
y1<- 5*y * exp(rnorm(n, mean = 0, sd = 0.15))
y2<- 2*y * exp(rnorm(n, mean = 0, sd = 0.75))

yy<-cbind(y1,y2)

matplot(yy,type='b',main='1) observed')

yy1<-yy/rep(apply(yy,2,mean),each=n)
matplot(yy1,type='b',main='2) standardised to the mean')


yy2<-yy1/rep(apply(yy1,2,sd),each=n)
matplot(yy2,type='b',main='3) standardised to mean and the sd')

lm(yy2[,1]~x)
lm(yy2[,2]~x)

