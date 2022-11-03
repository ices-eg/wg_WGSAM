
n<-100

a<-rnorm(n,mean=1,sd=0.2)

mean(a)
var(a)
sd(a)

sumx2<-0
sumx<-0
for (i in (1:n)){
  x<-a[i]-mean(a)
  sumx2<-sumx2+a[i]*a[i]
  sumx<-sumx+a[i]
} 


((sumx2 - (sumx^2)/n))/(n-1)
var(a)

####

(sum(a^2) - (sum(a)^2)/n)/(n-1)
var(a)
