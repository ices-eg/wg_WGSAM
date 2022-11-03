
###################################################
# consum (ration) 

a<-read.csv(file=file.path(root,exchangeDir,'consum_key_2017.csv'))
head(a)
sort(unique(a$species))
a[a$species=='HAK','species']<-'HKE'
a<-subset(a,!(species %in% c('N_M','W_M')),select=-species.n)

# just add the new years, re-using old data
b<-subset(a,year==lastYold)
for (y in ((lastYold+1):lastY)) {
  b$year<-y
  a<-rbind(a,b)
}
head(a)

#new data

b<-read.csv(file=file.path(root,exchangeDir,'consumPerAge_2017.csv'))
bn<-names(b)
bn<-sub('Species', 'species', bn)
bn<-sub('consum', 'CONSUM', bn)
names(b)<-bn
b$SMS_area<-1
b$year<-firstY

bb<-subset(b,year=firstY)
for (y in ((firstY+1):lastY)) {
  bb$year<-y
  b<-rbind(b,bb)
}

ex<-sort(unique(b$species))
a<-subset(a,!(species %in% ex))

#head(a)
#head(b)
a<-rbind(a,b)
####

CONSUM<-a

save(CONSUM,file=file.path(root,exchangeDir,'CONSUM.Rdata'))


write.table(CONSUM,file=file.path(finalExchangeDir,paste0('consum.dat')),row.names = F,quote = T,sep=',')



### consum ab
#REMEMBER TO CHECK IF IT INCLUDES THE RIGHT SPECIES

#file.copy(from=file.path(data.path,'consum_ab.in'),to=file.path(data.path,'consum_ab_OLD.in'),overwrite=TRUE)

a<-scan(file=file.path(root,exchangeDir,'consum_ab_key_2017.in'),comment.char = "#")
# test a<-1:length(a)
b<-array(a,dim=c(2,4,npr))
dimnames(b)<-list(c('a','b'),paste0('quarter',1:4),new.code.name[1:npr])
round(ftable(b),1)


# read new values
bb<-read.csv(file=file.path(root,exchangeDir,'consum_AB_2017.csv'))

for (i in (1:dim(bb)[[1]])) {
  b[1,bb[i,'quarter'],bb[i,'Species']]<-bb[i,'FOODA']
  b[2,bb[i,'quarter'],bb[i,'Species']]<-bb[i,'FOODB']
}


# use W.horse mac values for the North Sea horse mac
b[,,'N_H']<-b[,,'W_H']

b[,"quarter2",'N_H']<-b[,"quarter3",'N_H']
b['a',"quarter2",'N_H']<-b['a',"quarter3",'N_H']*0.7 # guessing


# write new values
out<-file.path(data.path,'consum_ab.in')
cat("# paramter a and b for consumption=a*weight^b\n",file=out)
for (s in (1:npr)) {
  cat("# ",sp.names[s],'\n',file=out,append=TRUE)
  for (q in (1:4)) {
    cat(round(b[1,q,s],5), round(b[2,q,s],5), '  # quarter ',q,'\n',file=out,append=TRUE)
  }
}
cat("-999 # check",'\n',file=out,append=TRUE)

