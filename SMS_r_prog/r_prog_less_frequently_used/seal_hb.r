a<-read.csv(file.path("C:","MV","SMS","data_northSea","2011-data","mammals","seal_diet.csv"))
a<-subset(a,prey=='COD')

cleanup()
X11()

par(mfcol=c(2,1))
par(mar=c(4,4,3,2)) # c(bottom, left, top, right)

b<-aggregate(preyw~prey+lowpreyl,data=a,sum)
b$preyw<-b$preyw/sum(b$preyw)*100
plot(b$lowpreyl,b$preyw,type='h',xlab='length (cm)',ylab='Weight proportion (%)',lwd=3,col='blue')

b<-aggregate(nprey~prey+lowpreyl,data=a,sum)
b$nprey<-b$nprey/sum(b$nprey)*100
plot(b$lowpreyl,b$nprey,type='h',xlab='length (cm)',ylab='Number proportion (%)',lwd=3,col='blue')



