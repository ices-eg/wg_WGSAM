inPath<-file.path('C:','MV','SMS','nop-mse-2013','Scen_112')

catch2012<-75000
rec2012<-104629000
capFQ4<-0.28; capFQ1Q3<-0.32; capF<-capFQ4+capFQ1Q3
TACQ1Q3.2013<-50000
BPA<-150000

fa<-1; la<-4;    # first age (e.g 0) has always  age =1
plus<-T
fa.F<-2; la.F<-3  #first and last age (index) in mean F

fq<-1;lq<-4; recq<-2 
fy<-2012; ny<-3; ly<-fy+ny-1

SR<-read.table(file=file.path(inPath,'SSB_R.out'),header=T)
SR<-subset(SR,select=c(alfa,beta,std)) 

a<-read.csv(file=file.path(inPath,'forecast-input-detailed.out'))
a<-subset(a, Species ==1,select=c(-Species))
a
ll<-list(q=a$Quarter,age=a$Age)
west<-tapply(a$West,ll,sum)
weca<-tapply(a$Weca,ll,sum)
M<-tapply(a$M,ll,sum)
expp<-tapply(a$FI,ll,sum)
pm<-tapply(a$Prop_Mature,ll,sum)
N<-tapply(a$N,ll,sum)

west[is.na(west)]<-0
weca[is.na(weca)]<-0
M[is.na(M)]<-0
expp[is.na(expp)]<-0
pm[is.na(pm)]<-0
N[is.na(N)]<-0

tmp<-M   # copy structure
tmp[,]<-0


################################
calc_Yield_from_Fscaling<-function(firstQ=fq,lastQ=lq,scaling=1,M=M,expp=expp, N, weca=weca){

  FF<-expp*scaling;
  Z<-FF+M;
  if (lastQ>firstQ) for (q in(firstQ:(lastQ-1))) {
    if (q>=recq) aRange<-1:la else  aRange<-2:la
     N[q+1,aRange]<-N[q,aRange]*exp(-Z[q,aRange])  
  }
  yield<-sum(N[firstQ:lastQ,]*(1-exp(-Z[firstQ:lastQ,]))/Z[firstQ:lastQ,]*FF[firstQ:lastQ,]*weca[firstQ:lastQ,],na.rm=T)
  return(yield); 
}
    
###
# find a scaling factor to mean F status quo to obtain a target yield
Fscaling.from.yield<-function(firstQ=fq,lastQ=lq,lower=0,upper=1,target.yield=100,N,M=M,west=west,weca=weca,FI=expp) {

  dif<-100.0;
  iter<-0;
  x<-1.0;

  while ((dif>1E-8) && (iter<100) && (x >=1E-8)) {
    x<-(upper+lower)/2;
    y=calc_Yield_from_Fscaling(firstQ=firstQ,lastQ=lastQ,scaling=x,M=M,expp=FI, N=N, weca=weca);
    if (y>=target.yield) upper<-x else lower<-x;
    dif<-abs(upper-lower);
    iter<-iter+1;
  }
  if ((iter<100) || (x<=1E-8))  return(x)  else return(-1000.0);
}

update.true.N<-function(y,firstQ=fq,lastQ=lq) { #calc N within the year
  for (q in(firstQ:(lastQ-1))) {
    if (q>=recq) aRange<-1:la else  aRange<-2:la
     N.true[q+1,aRange,y]<<-N.true[q,aRange,y]*exp(-Z.true[q,aRange,y])  
  } 
}

update.N<-function(N,Z,firstQ=fq,lastQ=lq) { #calc N within the year
  for (q in(firstQ:(lastQ-1))) {
    if (q>=recq) aRange<-1:la else  aRange<-2:la
     N[q+1,aRange]<-N[q,aRange]*exp(-Z[q,aRange])  
  } 
  return(N)
}

##

calc.rec<-function(SSB,noise=1) {
 if (SSB>SR[1,'beta']) rec<- SR[1,'beta']*exp(SR[1,'alfa'])*noise else rec<- SSB*exp(SR[1,'alfa'])*noise
 return(rec)
}
###

birthday<-function(y) { #calc N and recruitment in the beginning of year y+1
  yPlus<-as.character(as.numeric(y)+1)
  N.true[fq,2:la,yPlus]<<-N.true[lq,fa:(la-1),y]*exp(-Z.true[lq,fa:(la-1),y])
  if (plus) N.true[fq,la,yPlus]<<-N.true[fq,la,yPlus]+N.true[lq,la,y]*exp(-Z.true[lq,la,y])
  SSB<-sum(N.true[fq,,yPlus]*west[fq,]*pm[fq,],na.rm=T)
  N.true[recq,fa,yPlus]<<-calc.rec(SSB)
}



find_Fscaling_from_target_SSB_two_years<-function(firstQ=4,lastQ=3, lower=0,upper=2, target=BPA, M, expY1, expY2, N.obs, west, weca, pm) {

  dif=100.0;
  iter=0;
  x=1.0;
  expY1[1:3,]<0
  expY2[4,]<-0
  
  while ((dif>1E-5) && (iter<100) && (x >=1E-8)) {
    x=(upper+lower)/2;
    FI<-expY1*x;
    Z<-FI+M;
    N<-N.obs
    
    yieldQ4<-sum(N[4,]*(1.0-exp(-Z[4,]))/Z[4,]*FI[4,]*weca[4,],na.rm=T)

    #birthday
    N[fq,2:la]<-N[lq,fa:(la-1)]*exp(-Z[lq,fa:(la-1)])
    if (plus) N[fq,la]<-N[fq,la]+N[lq,la]*Z[lq,la]
    SSB<-sum(N[fq,]*west[fq,]*pm[fq,],na.rm=T)
    N[recq,fa]<-calc.rec(SSB)
    cat("scaling:",formatC(x,digits = 5, width = 8, format = "f", ),"YiledQ4:",yieldQ4,"SSB1:",round(SSB),"rec:",N[recq,fa],' ')
  

    FI<-expY2*x;
    Z<-FI+M;

    N<-update.N(N,Z,firstQ=fq,lastQ=lq)  #calc N within the year
    yieldQ1Q3=sum(N[fq:lastQ,]*(1.0-exp(-Z[fq:lastQ,]))/Z[fq:lastQ,]*FI[fq:lastQ,]*weca[fq:lastQ,],na.rm=T)

    #birthday
    N[fq,2:la]<-N[lq,fa:(la-1)]*Z[lq,fa:(la-1)]
    if (plus) N[fq,la]<-N[fq,la]+N[lq,la]*exp(-Z[lq,la])
    y<-sum(N[fq,]*west[fq,]*pm[fq,],na.rm=T)  #SSB
    cat("SSB2:",round(y),'\n')
    
    if (y>=target) lower=x else upper=x;
    dif<-abs(upper-lower);
 
    iter=iter+1;
  }
  if (!((iter<100) | (x<=1E-6)))  x<- -1000;
  
  return(list(x=x,yieldQ4=yieldQ4, yieldQ1Q3=yieldQ1Q3));   
}


####################################
N[recq,fa]<-rec2012

F.true<-array(0,dim=c(lq,la,ny),dimnames=list(fq:lq,(fa-1):(la-1),fy:ly))
Z.true<-F.true
N.true<-F.true
TAC.obs<-array(0,dim=c(3,ny),dimnames=list(fy:ly,c('Q1Q3','Q4','TAC')))
TAC.obs[2,'Q1Q3']<-TACQ1Q3.2013

yield.true<-matrix(0,nrow=ny,ncol=3,dimnames=list(fy:ly,c("Q1Q3","Q4","Yield")))
rec.true<-matrix(0,nrow=ny,ncol=1,dimnames=list(fy:ly,'Recruit'))
F.mean<-matrix(0,nrow=ny,ncol=1,dimnames=list(fy:ly,'mean F'))

y<-as.character(2012)
scaling<-Fscaling.from.yield(firstQ=fq,lastQ=lq,lower=0,upper=2,target.yield=catch2012,N=N,M=M,west=west,weca=weca,FI=expp) 
cat("F scaling:",scaling,'\n')
F.mean[y,1]<-min(scaling,capF)
F.true[,,y]<-expp*min(scaling,capF)
Z.true[,,y]<-F.true[,,y]+M
N.true[recq,fa,y]<-rec2012
N.true[fq,,y]<-N[fq,]

yield.true[y,'Yield']<-calc_Yield_from_Fscaling(firstQ=fq,lastQ=lq,scaling=F.mean[y,1],M=M,expp=expp, N, weca=weca)

update.true.N(y)
#print(N.true[,,y])
birthday(y)
#print(N.true[,,as.character(as.numeric(y)+1)])

# various invariants
mean.F.Q1Q3<-sum(expp[1:3,fa.F:la.F])/(la.F-fa.F+1)
upperScalQ1Q3<-capFQ1Q3/mean.F.Q1Q3

mean.F.Q4<-sum(expp[4,fa.F:la.F])/(la.F-fa.F+1)
upperScalQ4<-capFQ4/mean.F.Q4


for (yy in((fy+1):ly)){
  y<-as.character(yy)
  cat("year:",y,'\n')
  
  # step 3, calc Realised TAC and update true stock N to 1st July
  N<-N.true[,,y]
 
  scaling<-Fscaling.from.yield(firstQ=fq,lastQ=3,lower=0,upper=upperScalQ1Q3,target.yield=TAC.obs[y,'Q1Q3'],N,M=M,west=west,weca=weca,FI=expp) 
  cat("scaling Q1Q3:",scaling,'\n')
  F.true[1:3,,y]<-scaling*expp[1:3,]
  yield.true[y,"Q1Q3"]<-calc_Yield_from_Fscaling(firstQ=fq,lastQ=3,scaling=scaling,M=M,expp=expp, N, weca=weca)
  Z.true[,,y]<-M+F.true[,,y]
  update.true.N(y,firstQ=fq,lastQ=3) #calc N within the year
  #print(N.true)
 
  #step 4, simulate the September assessment
  N[,]<-0
  N[3,]<-N.true[3,,y] # HER SKAL DER OBS STØJ PÅ
  
  # step 5: calculate the observed F for Q3 and update obs N to 1st October
  real.Q3.catch<-sum(N.true[3,,y]*(1-exp(-Z.true[3,,y]))/Z.true[3,,y]*F.true[3,,y]*weca[3,])
  print(real.Q3.catch)
  scaling<-Fscaling.from.yield(firstQ=3,lastQ=3,lower=0,upper=5,target.yield=real.Q3.catch,N=N,M=M,west=west,weca=weca,FI=expp) 
  cat("scaling Q3:",scaling,'\n')
  F.obs<-expp*scaling   # Q3 is the only one used
  Z.obs<-F.obs+M
  N<-update.N(N,Z.obs,firstQ=3,lastQ=4) 
  print(N)
  
  # step 6 calculate TAC for Q4 and Q1Q3
  a<-find_Fscaling_from_target_SSB_two_years(firstQ=4,lastQ=3, lower=0,upper=3,target=BPA, M, expY1=expp, expY2=expp, N.obs=N, west=west, weca=weca, pm=pm) 
  TAC.obs[y,'Q4']<-a$yieldQ4

  if (yy<ly) TAC.obs[as.character(as.numeric(y)+1),'Q1Q3']<-a$yieldQ1Q3
  
   
  # step 7 calc true F and yield for Q4
  N<-N.true[,,y]
 
  scaling<-Fscaling.from.yield(firstQ=4,lastQ=4,lower=0,upper=upperScalQ4,target.yield=TAC.obs[y,'Q4'],N,M=M,west=west,weca=weca,FI=expp) 
  cat("scaling Q4:",scaling,'\n')
  F.true[4,,y]<-scaling*expp[4,]
  yield.true[y,"Q4"]<-calc_Yield_from_Fscaling(firstQ=4,lastQ=4,scaling=scaling,M=M,expp=expp, N, weca=weca)
  Z.true[4,,y]<-M[4,]+F.true[4,,y]
  
}
  
print(cbind(F.mean,TAC.obs,yield.true,rec.true))
