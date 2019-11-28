
My.dir<-file.path(root,'bw-sam-2014','sam')
run<-'run'
#run<-'baserun'

# make summary_table_raw.out file from SAM output
fit<-read.fit(file=file.path(My.dir,run,'sam'))
summary(fit)
a<-data.frame(Species.n=1,Year=fit[['years']], Rec=fit[['R']][,1] ,SSB=fit[['ssb']][,1],TSB=fit[['tsb']][,1],SOP=0,SOP.hat=1,mean.F=fit[['fbar']][,1])
write.table(a,file=file.path(data.path,'summary_table_raw.out'),row.names = F, quote = F)

minAge<-min(fit$res[,3])
maxAge<-max(fit$res[,3])
noN<-maxAge-minAge+1
noFleet<-max(fit$res[,2])

setwd(file.path(My.dir,'data'))

read.ices<-function(filen){
  # Function to read ices data files
  head<-scan(filen, skip=1, n=7, quiet=TRUE)
  minY<-head[3]
  maxY<-head[4]
  minA<-head[5]
  maxA<-head[6]
  C<-as.matrix(read.table(filen, skip=5, header=F))
  C<-C[,1:(maxA-minA+1)]
  rownames(C)<-minY:maxY
  colnames(C)<-minA:maxA
  return(C)
}

# MV variable names are changed, and obs yield is calculated
C<-read.ices('cn.dat')
WECA<-read.ices('cw.dat')
WEST<-read.ices('sw.dat')
propmat<-read.ices('mo.dat')
M<-read.ices('nm.dat')
obsYield<- apply(C*WECA[1:nrow(C),],1,sum)


setwd(data.path)

dimC<-dimnames(C)
dimCplus<-dimC
dimCplus[[1]]<-c(dimCplus[[1]],max(as.numeric(dimCplus[[1]]))+1)

FF<-exp(fit$stateEst[-nrow(fit$stateEst),-c(1:noN)])
FF<-cbind(FF,FF[,dim(FF)[[2]]]) # add age 10 as copy og age 9


N<-exp(fit$stateEst[,c(1:noN)])
#N<-exp(fit$stateEst[-nrow(fit$stateEst),c(1:noN)])
dimnames(N)<-dimCplus

#M<-rbind(M[dimC[[1]],],rep(-1,noN))
#WEST<-rbind(WEST[dimC[[1]],],rep(-1,noN))
#WECA<-rbind(WECA[dimC[[1]],],rep(-1,noN))
#propmat<-rbind(propmat[dimC[[1]],],rep(-1,noN))

dimnames(FF)<-dimC
FF<-rbind(FF[dimC[[1]],],rep(-1,noN))
CCC<-rbind(C[dimC[[1]],],rep(-1,noN))
dimnames(CCC)<-dimCplus
a<-data.frame(Year=rep(dimCplus[[1]],each=noN),
                 Quarter=1,Species.n=1,Age=rep(dimC[[2]],times=length(dimCplus[[1]])),
                 M1=0,
                  M=as.vector(t(M)),
                  M2=0,
                  C.obs=CCC,
                  F=as.vector(t(FF)), 
                  N=as.vector(t(N)),
               west=as.vector(t(WEST)),
               weca=as.vector(t(WEST)),    # should use WECA, but they are identical
            propmat=as.vector(t(propmat))
            )
            
a<-data.frame(a,Z=a$M+a$F,N.bar=a$N*(1-exp(-a$M-a$F))/(a$M+a$F),BIO=a$N*a$west)
a<-data.frame(a,SSB=a$BIO*a$propmat,C.hat=a$N.bar*a$F,Yield=a$N.bar*a$F*a$weca)
write.table(a,file=file.path(data.path,'summary.out'),row.names = F, quote = F)

########################

FF<-exp(fit$stateEst[-nrow(fit$stateEst),-c(1:noN)])
colnames(FF)<-paste(1:ncol(FF),c(rep('',ncol(FF)-1),'+'),sep='')
rownames(FF)<-fit.current$years[-length(fit.current$years)]

rw<-FF[-1,1]-FF[1:dim(FF)[1]-1,]

qpl<-function(x) {
 #cleanup()
 print(hist(x))
 X11()
 qqnorm(x)
 qqline(x)
}
qpl(rw)

Rec<-fit$R[,1]

rw<-Rec[-1]-Rec[1:length(Rec)-1]
qpl(rw)


a<-read.table( file=file.path(My.dir,'sam.std'),header=F,skip=1)
names(a)=c('no','param','value','std')
head(a)


a$expval<-exp(a$value)
head(a)

a$low<-exp(a$value-2*a$std)*1000
a$high<-exp(a$value+2*a$std)*1000
a$CV<-(a$high-a$low)/4/a$expval
head(a)
