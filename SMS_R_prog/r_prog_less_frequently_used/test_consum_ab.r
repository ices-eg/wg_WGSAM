n<-100
w<-1; a1<-runif(n, min = w*0.5, max = w*1.5)
w<-2; a2<-runif(n, min = w*0.5, max = w*1.5)
w<-3; a3<-runif(n, min = w*0.5, max = w*1.5)

if (T) {
  sd2<-0.2
  w<-1; a1<-rnorm(n, mean = w, sd =sd2)
  w<-2; a2<-rnorm(n, mean = w, sd = sd2)
  w<-3; a3<-rnorm(n, mean = w, sd = sd2)
}

ww<-c(a1,a2,a3)
a<-1.2
b<-0.66
h<-data.frame(w=rep(1:3,each=n),wobs=ww,cons=a*ww^b)
cleanup()
plot(x=h$w,y=h$cons)
lines(x=h$wobs,y=h$cons,type='p',col='red')

plot(x=log(h$wobs),y=log(h$cons))

aa<-lm(log(cons)~log(wobs),data=h)

h$predict<-predict(aa)
plot(x=log(h$wobs),y=log(h$cons))
lines(x=log(h$w),y=log(h$cons),col='red',type='p')
abline(aa)



plot(x=h$wobs,y=(h$cons))

co<-coef(aa)
h$pred2<-exp(co[1])*h$wobs^co[2]
lines(x=h$wobs,y=h$pred2,col='red',type='p')
summary(aa)
cat("a:",a," b:",b,"\n")
cat("estimated a:",exp(co[1])," b:",co[2],"\n")