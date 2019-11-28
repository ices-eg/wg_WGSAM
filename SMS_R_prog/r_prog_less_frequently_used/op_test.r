# Script to test the OP program
SMS.installed<-T
area_model<-T

library(lattice)

if (!SMS.installed) {
  data.path<-file.path("C:","mv","sms","FBA")
  source(file.path(data.path,"R-prog","FLOP.control.r"))
  source(file.path(data.path,"R-prog","OP_plot.r"))
}

if (SMS.installed) {
  #read the "environment"
  SMS<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))
  no.areas<-SMS@no.areas     # 4
  no.q<-SMS@last.season      # 4
  nsp<-SMS@no.species        # 3
  max.age.all<-SMS@max.age.all  # 8
  n.other.pred<-sum(SMS@species.info[,'predator']==2) # 0
  n.vpa<-nsp-n.other.pred # 3
}  else {
  if (area_model) no.areas<-4 else no.areas<-1
  no.q<-4
  nsp<-3
  max.age.all<- 8
  n.other.pred<-0
  n.vpa<-3
  first.VPA<-1
  sp.names<-c("Cod","Herring","Sprat")
  fa<-0
  rec.season<-3
  av.F.age<- matrix(c(4,7,3,6,3,5),byrow=T, ncol=2)
  setwd(data.path)
}


# Read the OP.par file
OP<-read.FLOP.control(file=file.path(data.path,'OP.dat'),n.VPA=n.vpa,n.other.pred=n.other.pred)

OP@first.year<-2012
OP@last.year<-2012
OP@F.or.C[1,]<-rep(1,n.vpa)    # set input as F at age
OP@output<-15                  # very detailed output!
write.FLOP.control(OP,file='OP.dat',nice=T)   # write the OP.dat file with updated values


# SMS has provided stock numbers
# first we have to create catch by area from dummy F
# create F at age by area
f<-array(0.1,dim=c(no.q,no.areas,n.vpa,max.age.all+1),
   dimnames=list(paste('q_',(1:no.q),sep=''),
                 paste('area_',(1:no.areas),sep=''),
                 paste('sp_',(nsp-n.vpa+1):n.vpa,sep=''),
                 paste('a_',(0:max.age.all),sep='')))
f[,,'sp_1',]<-0.15   # higher F on cod
if (area_model) f[,4,'sp_1',]<-0    # no cod in area 4 (outside area 25, 26 and 28)
f[,,,1]<-0         # no fishing for the 0-group


read_area_file<-function(filename) {
    f<-scan(file=file.path(data.path,filename),comment.char = "#" )
    dim(f)<-c(max.age.all+1,n.vpa,no.areas,no.q)
    dimnames(f)=list( paste('a_',(0:max.age.all),sep=''),
                     paste('sp_',(nsp-n.vpa+1):n.vpa,sep=''),
                     paste('area_',(1:no.areas),sep=''),
                     paste('q_',(1:no.q),sep=''))
    f<-aperm(f,c(4,3,2,1))
    return(f)
}


write_area_file<-function(out,vari) {
  outFile<-file.path(data.path,out)
  cat("# data by quarter, area, species and age\n",file=outFile)
  for (q in (1:no.q))  {
    cat("# quarter:",q,"\n",file=outFile,append=T)
    for (d in (1:no.areas)) write.table(vari[q,d,,],file=outFile,append=TRUE,quote=FALSE,sep=" ",row.names=FALSE,col.names=FALSE)
  }
}

write_area_file('OP_F.in',f)

# make a run one year ahead (you could change the years in OP.dat, but it is really not nesesary)
system("op -maxfn 0 -nohess",show.output.on.console=T)


# read detalied data for year y
a<-read.table(file=file.path(data.path,"OP_summary.out"),header=T)
head(a)

# read detalied annual data  for year y
anno<-read.table(file=file.path(data.path,"OP_summary_anno.out"),header=T)

# read stock number in year y+1
readN<-function() {
  matrix(scan(file=file.path(data.path,"OP_N.out"),comment.char = "#",quiet=T),nrow=n.vpa,ncol=max.age.all+1,byrow=T,dimnames=list(sp.names,paste("age",0:max.age.all,sep="")))
}

# write stock number in year y+1
writeN<-function(N){
  write(t(N),ncolumns=max.age.all+1,file.path(data.path,"OP_N.in"))
}

N<-readN()

# just checking  that F going in and out are the same
Fout<-tapply(a$F,list(Quarter=a$Quarter,Area=a$Area,Species=a$Species.n,Age=a$Age),sum,na.rm=T)

ftable(Fout/f)


# just for testing: Give C as input and see if you get the same F outs as used to produce C
Cout<-tapply(a$C,list(Quarter=a$Quarter,Area=a$Area,Species=a$Species.n,Age=a$Age),sum,na.rm=T)
ftable(round(Cout/1000))
Cout[is.na(Cout)]<-0
write_area_file('OP_C.in',Cout)


OP@F.or.C[1,]<-rep(2,n.vpa)    # set input as C at age
write.FLOP.control(OP,file='OP.dat',nice=T)
system("op -maxfn 0 -nohess",show.output.on.console=T)


a<-read.table(file=file.path(data.path,"OP_summary.out"),header=T)

# just checking that F derived from catches are the same as F used to calculate catches
Fout<-tapply(a$F,list(Quarter=a$Quarter,Area=a$Area,Species=a$Species.n,Age=a$Age),sum,na.rm=T)
ftable(Fout/f)



##############################

# test
# get a fresh copy of initial stock numbers and F at age
file.copy(file.path(data.path,'backup','OP_N.in'),file.path(data.path,'OP_N.in'), overwrite = TRUE)
file.copy(file.path(data.path,'backup','OP_F.in'),file.path(data.path,'OP_F.in'), overwrite = TRUE)

OP@F.or.C[1,]<-rep(1,n.vpa)    # set input as F at age
OP@output<-15                  #  detailed output!
OP@stochastic.recruitment[]<-1  # Stochastic recrruitment
OP@rec.noise['lower',]<--2
OP@rec.noise['upper',]<- 2

write.FLOP.control(OP,file='OP.dat',nice=T)   # write the OP.dat file with updated values

res.N<-NULL
res.all<-NULL
res.anno<-NULL
for (y in (1:30) ) {

 cat(file='op_seed.in',round(runif(1, min=0, max=1E5)),'\n')
 system("op -maxfn 0 -nohess",show.output.on.console=T)
 Nnew<-readN()
 tmp<-arr2df(Nnew)
 tmp$year<-y
 res.N<-rbind(tmp,res.N)
 
 tmp<-read.table(file=file.path(data.path,"OP_summary.out"),header=T)
 tmp$Year<-y
 res.all<-rbind(tmp,res.all)

 tmp<-read.table(file=file.path(data.path,"OP_summary_anno.out"),header=T)
 tmp$Year<-y
 res.anno<-rbind(tmp,res.anno)

 ## simulate the assessment

 writeN(Nnew)  # initial population     # write Real N (recived from Operating model) to be used for next round
}


names(res.N)<-c("Species","Age","N","year")
xyplot(N~year |Species,groups=Age,scales=list(relation='free') ,type='b',data=res.N)

OP_plot(res.all)

# sprat dies out!!

#########################
# get a fresh copy of initial stock numbers and F at age
file.copy(file.path(data.path,'backup','OP_N.in'),file.path(data.path,'OP_N.in'), overwrite = TRUE)
file.copy(file.path(data.path,'backup','OP_F.in'),file.path(data.path,'OP_F.in'), overwrite = TRUE)

f<-read_area_file('OP_F.in')
# decrease sprat F
f[,,'sp_3',] <-f[,,'sp_3',]/2

# increase F on cod
f[,,'sp_1',] <-f[,,'sp_1',]*1.5

write_area_file('OP_F.in',f)

# just testing
ftable(apply(f,c(1,2,3,4),sum))


res.N<-NULL
res.all<-NULL
res.anno<-NULL
for (y in (1:40) ) {

cat(file='op_seed.in',round(runif(1, min=0, max=1E5)),'\n')
 system("op -maxfn 0 -nohess",show.output.on.console=T)
 Nnew<-readN()
 tmp<-arr2df(Nnew)
 tmp$year<-y
 res.N<-rbind(tmp,res.N)

 tmp<-read.table(file=file.path(data.path,"OP_summary.out"),header=T)
 tmp$Year<-y
 res.all<-rbind(tmp,res.all)

 tmp<-read.table(file=file.path(data.path,"OP_summary_anno.out"),header=T)
 tmp$Year<-y
 res.anno<-rbind(tmp,res.anno)

 ## simulate the assessment

 writeN(Nnew)  # initial population     # write Real N (recived from Operating model) to be used for next round
}


names(res.N)<-c("Species","Age","N","year")
xyplot(N~year |Species,groups=Age,scales=list(relation='free') ,type='b',data=res.N)

xyplot(N~year |Species,groups=Age,scales=list(relation='free') ,type='b',data=res.N,subset=(Species=='Cod' & Age=='age2'))

OP_plot(res.all)
