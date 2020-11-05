a<-Read.consum_ab()
head(a)
ftable(round(tapply(a$parameter,list(paste(formatC(a$Species.n,width=2,flag='0'),a$Species),a$Quarter,a$ab),sum),3))