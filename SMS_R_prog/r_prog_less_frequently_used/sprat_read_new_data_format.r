# filenames for input data
newCW<-'Total_catch_in_numbers_and_mean_weight_2011.csv'

first.year.out<-1991
last.year.out<-2011
n.season<-4


####################################


# WECA and WEST

a<-read.table(file=file.path(data.path,'Input',newCW),header=T,sep=',')
a[is.na(a$mw0),'mw0']<-0
b<-subset(a,select=c(year,quarter, mw0,mw1,mw2,mw3,mw4))
b
out<-file.path(data.path,"weca.in")
unlink(out)
for (y in(first.year.out:last.year.out)) {
  cat(paste("# year:",y,"\n"), file=out,append=TRUE)
    for (s in(1:n.season)){ 
     aa<-subset(b,year==y & quarter==s)
     cat(format(c(aa$mw0,aa$mw1, aa$mw2, aa$mw3, aa$mw4) ,format='fg',width=10,justify='right',digits=2,nsmall=5),file=out,append=T)
     cat("\n",file=out,append=T)
    }
}

file.copy(out, file.path(data.path,"west.in"), overwrite = TRUE)

# CANUM

a$n0<-round(a$n0,0)
a$n1<-round(a$n1,0)
a$n2<-round(a$n2,0)
a$n3<-round(a$n3,0)
a$n4<-round(a$n4,0)

b<-subset(a,select=c(year,quarter, n0,n1,n2,n3,n4))


out<-file.path(data.path,"canum.in")
unlink(out)
for (y in(first.year.out:last.year.out)) {
  cat(paste("# year:",y,"\n"), file=out,append=TRUE)
    for (s in(1:n.season)){ 
     aa<-subset(b,year==y & quarter==s)
     cat(format(c(aa$n0,aa$n1, aa$n2, aa$n3, aa$n4) ,format='fg',width=12,justify='right'),file=out,append=T)
     cat("\n",file=out,append=T)
    }
}

################################
# natural mortality
a<-read.table(file=file.path(data.path,'Input','m.csv'),header=T,sep=',')
m<-tapply(a$m,list(a$Quarter,a$Age),sum)
m[is.na(m)]<-0
m
m<-round(cbind(m,m[,'3']),2)

out<-file.path(data.path,'natmor.in')
cat("####################################\n","###  Sprat Natural Mortality\n",file=out,append=F)

for (y in (first.year.out:(last.year.out+1))) {
  cat("# ",y,'\n',    file=out,append=T)
  write.table(m,file=out,append=T,col.names=F,row.names=F)
}


################################
# maturity
a<-read.table(file=file.path(data.path,'Input','maturity.csv'),header=T,sep=',')
m<-round(tapply(a$m,list(a$quarter,a$age),sum),2)
m[is.na(m)]<-0
m
m<-matrix(rep(m,each=4),nrow=4)

out<-file.path(data.path,'propmat.in')
cat("####################################\n","###  Sprat Proportion mature\n",file=out,append=F)

for (y in (first.year.out:(last.year.out+1))) {
  cat("# ",y,'\n',    file=out,append=T)
  write.table(m,file=out,append=T,col.names=F,row.names=F)
}


##################################
# CPUE indeces

a<-read.table(file=file.path(data.path,'Input','ibtsq1_q3.csv'),header=T,sep=',')
head(a)
a$survey<-'IBTS'

b<-read.table(file=file.path(data.path,'Input','heras.csv'),header=T,sep=',')
b$survey<-'Heras'

ab<-rbind(a,b)
ab<-subset(ab,year>=1991)

a<-tapply(ab$n,list(ab$survey,ab$year,ab$quarter,ab$age),sum)
ftable(round(a,1))

a['IBTS','1995','3','0'] <- -a['IBTS','1995','3','0']
ftable(round(a,1))


out<-file.path(data.path,'fleet_catch.in')

cat("####################################\n","###  Sprat\n",file=out,append=F)

cat("####################################\n","###  IBTS Q1\n",file=out,append=T)
cat("# effort Age1 Age2 Age3 Age4\n",file=out,append=T)
x<-cbind(1E-4,round(a['IBTS',as.character(seq(first.year.out,last.year.out+1)),'1',c('1','2','3','4')],2))
write.table(x,file=out,append=T,col.names=F,row.names=F)

cat("####################################\n","###  IBTS Q3\n",file=out,append=T)
cat("# effort  Age1 Age2 Age3\n",file=out,append=T)
x<-cbind(1E-4,round(a['IBTS',as.character(seq(first.year.out,last.year.out)),'3',c('1','2','3')],2))
write.table(x,file=out,append=T,col.names=F,row.names=F)

cat("####################################\n","###  Heras Q2\n",file=out,append=T)
cat("# effort  Age1 Age2 Age3 \n",file=out,append=T)
x<-cbind(1E-3,a['Heras',as.character(seq(2003,last.year.out)),'2',c('1','2','3')])
write.table(x,file=out,append=T,col.names=F,row.names=F)


