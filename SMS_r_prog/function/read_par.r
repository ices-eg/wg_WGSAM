p<-scan(file.path(data.path,"sms.par"),comment.char = "#")

dp<-data.frame(no=1:length(p),par=p)

val<-  1.10741

dp$dif<-dp$par-val
head(dp[order(dp$dif**2),])

found<-head(dp[order(dp$dif**2),])$no

# for (i in (1:length(p))) print(paste(i ,p[i]))

par_exp<-read.table(file.path(data.path,"par_exp.out"),header=TRUE)

par_exp[found+1,]

