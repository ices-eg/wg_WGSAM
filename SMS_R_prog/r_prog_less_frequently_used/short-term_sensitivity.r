#  stochastic, short term forecast,
# lines begining with # are comments and can be put anywhere
#
#
# Specification of year range 
# 1. first year for inclusion in mean
# 2. second year for inclusion in mean
# 3. include variance. 1=TRUE, 0=FALSE

###################################################
# Input
options<-c(
 2006, 2008, 1,        # Mean weight in the stock
 2006, 2008, 1,        # mean weight in the catch
 2006, 2008, 0,        # proportion mature
 2006, 2008, 0        # Natural mortality
)

all.covariance<-F      # include (T|F) covariance or the four parameters 
                       # if TRUE remember to selct the same year range for all parameters
                       
Ry2<- 8.820E6     #Recruitment in the second year after the last assessment year
Ry2.cv<-0.57    # CV of recruitment in the second year after the last assessment year
Ry3<-8.820E6     #Recruitment in the third year after the last assessment year
Ry3.cv<-0.57    # CV of recruitment in the third year after the last assessment year


TACintermidiatYear<-1200000        # TAC or F-multiplier for intermidiate year
FmultIntermidiatYear<-NA           #   one of them needs to be NA
###################################################

if (is.na(FmultIntermidiatYear)) doCalcFmult<-T else doCalcFmult<-F

la<-SMS.control@species.info[1,"last-age"]         #last age
fa<-  SMS.control@first.age                        # first age
na<-la-fa+1                                        # no of age-groups

ly<-SMS.control@last.year.model                    # last assessment year
agesFbar<-as.vector(SMS.control@avg.F.ages)
  

options<-matrix(options,ncol=3,byrow = T)
dimnames(options)[2]<-list(c('first year','last year','Include variance'))
dimnames(options)[1]<-list(c('WS','WC','PM','M'))


read_input<-function(inp="west.in",lab='',
       fy=SMS.control@first.year, ly=SMS.control@last.year
) {
  w<-scan(file=file.path(data.path,inp),comment.char = "#" )
  la<-SMS.control@species.info[1,"last-age"]
  fa<-  SMS.control@first.age

  w<-matrix(w,ncol= la-fa+1,byrow = T)
  w<-w[1:(ly-fy+1),]  # cut off

  dimnames(w)[2]<-list(paste(lab,formatC(seq(fa,la),flag='0',width=2),sep=''))
  dimnames(w)[1]<-list(seq(fy,ly))
  w
}

WS<-read_input ("west.in",lab='WS')
WC<-read_input ("weca.in",lab='WC')
PM<-read_input ("propmat.in",lab='PM')
 M<-read_input ("natmor.in",lab='M')

#filter data
WS<- WS[paste(seq(options[1,1],options[1,2])),]
WC<- WC[paste(seq(options[2,1],options[2,2])),]
PM<- PM[paste(seq(options[3,1],options[3,2])),]
M<-   M[paste(seq(options[4,1],options[4,2])),]

if (all.covariance) {
   a<-cbind(WS,WC,PM,M)
   cov1<-cov(a) 
} 
if (is.matrix(WS)) {WS.var<-apply(WS,2,var); WS<-apply(WS,2,mean)} else WS.var<-rep(0,na)
if (options['WS','Include variance']==0) WS.var<-rep(0,na)
if (is.matrix(WC)) {WC.var<-apply(WC,2,var); WC<-apply(WC,2,mean)} else WC.var<-rep(0,na) 
if (options['WC','Include variance']==0) WC.var<-rep(0,na)
if (is.matrix(PM)) {PM.var<-apply(PM,2,var); PM<-apply(PM,2,mean)} else PM.var<-rep(0,na) 
if (options['PM','Include variance']==0) PM.var<-rep(0,na)
if (is.matrix(M))  {M.var<-apply(M,2,var);   M<-apply(M,2,mean)} else M.var<-rep(0,na) 
if (options['M','Include variance']==0)  M.var<-rep(0,na)
   
a<-c(WS,WC,PM,M)
varNames1<-names(a)

if (!all.covariance) {  
  cov1<-matrix(0,nrow=length(a),ncol=length(a))
  dimnames(cov1)[2]<-list(varNames1)
  dimnames(cov1)[1]<-list(varNames1)
  diag(cov1)<-c(WS.var,WC.var,PM.var,M.var)     # use only the variance (no co-variance)
}


#####################################
# Make covariance matrix from SMS SMS.cor and SMS.std files

#Open SMS correlation file
file<-file.path(data.path,"sms.cor")
ofil<-file.path(data.path,"sms_out.cor")

a<-readLines(file)

len<-length(a)
a<-a[3:len]
len<-len-2
write.table(a,file=ofil,quote=FALSE,col.names=FALSE,row.names=FALSE)

var.name<-substr(a,start=10,stop=23)

b<-substr(a,start=49,stop=1e5)
write.table(b,file=ofil,quote=FALSE,col.names=FALSE,row.names=FALSE)
c<-scan(file=ofil)
cor<-matrix(NA,nrow=len,ncol=len)

j<-1
for (i in (1:len)) {
 cor[i,1:(i)]<-c[j:(j+i-1)]
 j<-j+i
}

# fill the upper triangle
for (i in (1:len)) {
  for (j in (min(i+1,len):len)) {
    cor[i,j]<-cor[j,i]
  }
}


# read SMS.std
a<-read.table(file.path(data.path,"sms_sensi.std"),comment.char = "#",header=FALSE,skip=1)
std<-a$V4
value<-a$V3
var.name<-a$V2


# make covariance from correlation
cov2<-matrix(NA,nrow=len,ncol=len)  # co-variance
for (i in (1:len)) {
  for (j in (1:len)) {
    if (i!=j) cov2[i,j]=cor[i,j]*std[i]*std[j]
  }
}
diag(cov2)<-std^2

vars<- 'term'
varList<-grep(vars,var.name)
cov2<-cov2[varList,varList]


varNames2<-sub('term2_N','N',var.name[varList])
varNames2<-sub('term_F','F',varNames2)
varNames2<-paste(varNames2,formatC(seq(fa,la),flag='0',width=2),sep='')

dimnames(cov2)[2]<-list(varNames2)
dimnames(cov2)[1]<-list(varNames2)

#just checking. Get the correlation matrix from the new covariance matrix
# round(cov2cor(cov2),2)

#######################

# We have now two covariance matrices (cov1 and cov2)
# combine those into one, assuming no corelation between the two matrices


allNames<-sort(c(varNames1,varNames2,'Ry2','Ry3'))

# the combined covariance matrix
cov12<-matrix(0,nrow=length(allNames),ncol=length(allNames))
dimnames(cov12)[2]<-list(allNames)
dimnames(cov12)[1]<-list(allNames)

cov12[varNames1,varNames1]<-cov1
cov12[varNames2,varNames2]<-cov2

cov12["Ry2","Ry2"]<-(Ry2.cv*Ry2)^2  # Recruitment variance
cov12["Ry3","Ry3"]<-(Ry3.cv*Ry3)^2

#cov12

#######################################
# Collect input parameters
#


FN<-value[varList]              # F and stock numbers
names(FN)<-varNames2
rec<-c(Ry2,Ry3)                 #recruitment
names(rec)<-list('Ry2','Ry3')

values<-c(WS,WC,PM,M,FN,rec)    # all of them
values<-values[order(names(values))]

#########################################

#Utility to extract the various data types from the list of parameters 
clip<-matrix(NA,nrow=2,ncol=8)
dimnames(clip)[2]<-list(c('FI','M','N','PM','Ry2','Ry3','WC','WS'))
 clip[,'FI']<-c(1,na)
 i<-1+na
 clip[,'M']<-c(i,i+na-1)
 i<-i+na
  clip[,'N']<-c(i,i+na-1)
 i<-i+na
 clip[,'PM']<-c(i,i+na-1)
 i<-i+na
 clip[,'Ry2']<-i
 i<-i+1
 clip[,'Ry3']<-i   # recruits
 i<-i+1
  clip[,'WC']<-c(i,i+na-1)
 i<-i+na
  clip[,'WS']<-c(i,i+na-1)

#####################################
# F bars
Fs<-values[clip[1,'FI']:clip[2,'FI']]
Fbar<-mean(Fs[paste("F",formatC(seq(agesFbar[1],agesFbar[2]),flag='0',width=2),sep='')])

##################


# prediction

predict<-function(pars,Fmult1=1,Fmult2=1) {
 FI<-matrix(NA,nrow=2,ncol=na)
 N<-matrix(NA,nrow=3,ncol=na)
 FI[1,]<-pars[clip[1,'FI']:clip[2,'FI']]
 M<-pars[clip[1,'M']:clip[2,'M']]
 N[1,]<-pars[clip[1,'N']:clip[2,'N']]
 prop<-pars[clip[1,'PM']:clip[2,'PM']]
 N[2,1]<-pars[clip[1,'Ry2']]
 N[3,1]<-pars[clip[1,'Ry3']]
 Weca<-pars[clip[1,'WC']:clip[2,'WC']]
 West<-pars[clip[1,'WS']:clip[2,'WS']]
 
  if (doCalcFmult) {
   calcYield<-function(Fmult=1,FF=FI[1,]) {
     FF<-FF*Fmult;
     ZZ<-FF+M
     sum(N[1,]*(1-exp(-ZZ))*FF/ZZ*Weca)
    }
 
  dif<-100.0;
  iter<-0;
  x<-1.0;
  target<-TACintermidiatYear
  upper<-3; lower<-0
  while ((dif>1E-8) & (iter<100) & (x >=1E-12)) {
    x<-(upper+lower)/2;
    y<-calcYield(Fmult=x);
    if (y>=target) upper<-x else lower<-x;
    dif<-abs(upper-lower);
    iter<-iter+1;
  }
  if ((iter<100) | (x<=1E-8))  Fmult1<-x else Fmult1<- -1000.0;
 } 
  
 FI[2,]<-FI[1,]*Fmult2
 FI[1,]<-FI[1,]*Fmult1
 
 Z<-FI[,]+M
  
 for (y in (1:2)) {
   for (a in ((fa:(la-1))))  N[y+1,a+1]<-N[y,a]*exp(-Z[y,a])
   N[y+1,la]<-N[y+1,la]+N[y,la]*exp(-Z[y,la])  #plusgroup
 }
 
 SSB<-N*rep(West,each=3)*rep(prop,each=3)
 TSB<-N*rep(West,each=3)
 Yield<-N[1:2,]*(1-exp(-Z))/Z*FI *rep(Weca,each=2)
 list(TSB,SSB,Yield,N,FI,Fmult1)
}

# test
# sum(predict(values)[[2]][1,])
###################

 
##########

# Partial derivatives 
##########
partialDerivatives<-function(parm, varNo,varYno,Fmult2=1) {

# Values input values for prediction 
# varNo  1=TSB, 2=SSB, 3=Yield, 4=N, 5=F
# varYno  1=intermidiate year, 2=TAC year, 3=TAC year+1
# Fmult F-multiplier in the TAC year
   
  grad<-rep(NA,length(parm))
  
  B1<-sum(predict(pars=parm,Fmult2=Fmult2)[[varNo]][varYno,])
  
  delta<-0.01
  for (i in (1:length(parm))) {
    localPar<-values
    localPar[i]<- localPar[i]*(1.0+delta)
    B2<-sum(predict(pars=localPar,Fmult2=Fmult2)[[varNo]][varYno,])   
    grad[i]<-(B2-B1)/(delta*parm[i])
  }
  
   # without covariance
   var1<-sum(diag(cov12)*grad^2)
   # with covariance
   var2<-as.numeric(t(grad) %*% cov12 %*% grad)
  list(B1,var1,var2,grad)
}



# sensitivity 
sensitivity<-function(pars,varNo=2,varYno=2) {

  varGrad<-partialDerivatives(pars,varNo=varNo,varYno=varYno)
  
  # rate sensitivity coefficients
  sens<-varGrad[[4]]*pars/ varGrad[[1]]
  sens2<-sens[order(abs(sens),decreasing = T)]
  # varNo  1=TSB, 2=SSB, 3=Yield, 4=N, 5=F
  if (varNo==1) tit<-'TSB'
  if (varNo==2) tit<-'SSB' 
  if (varNo==3) tit<-'Yield'
  tit2<-paste(tit, ly+varYno ,"\n Liniar coefficients")
  barplot(sens2[sens2>0.1],cex.names=0.8,las=2,ylab='sensitivity',main=tit2,cex.main = 0.8)
  
  
  #partial Variance
  
  p<-varGrad[[4]]^2*diag(cov12)/ varGrad[[2]]
  p1<-p[order(p)]
  sepa<-0.9
  p2<-c(sum(p1[1:(length(p1)*sepa)]),p1[(length(p1)*sepa+1):length(p1)])
  names(p2)[1]<-"other"
  tit2<-paste(tit, ly+varYno ,"\nProportion of variance")
  pie(p2, main=tit2,cex.main=0.8)
 
}
 
cleanup()
nox<-3; noy<-4;
dev<-"print"
dev<-"screen"
newplot(dev,nox,noy);

sensitivity(values,varNo=3,varYno=2) 
sensitivity(values,varNo=2,varYno=3)
################################################
#  input values to forecast

 B0<-predict(values)
 CV<-sqrt(diag(cov12))/values
 out<- cbind(seq(fa,la),round(WS,d=3)) ; lab<-c('Age','Weight in the stock (kg)')
 if (sum(CV[clip[1,'WS']:clip[2,'WS']]) >0) { out<-cbind(out, round(CV[clip[1,'WS']:clip[2,'WS']],2)); lab<-c(lab,'CV');}
 out<- cbind(out,round(WC,d=3)) ; lab<-c(lab,'Weight in the catch (kg)')
 if (sum(CV[clip[1,'WC']:clip[2,'WC']]) >0) { out<-cbind(out, round(CV[clip[1,'WC']:clip[2,'WC']],2)); lab<-c(lab,'CV');}
 out<- cbind(out,round(PM,d=3)) ; lab<-c(lab,'Proportion mature')
 if (sum(CV[clip[1,'PM']:clip[2,'PM']]) >0) { out<-cbind(out, round(CV[clip[1,'PM']:clip[2,'PM']],2)); lab<-c(lab,'CV');}
 out<- cbind(out,round(B0[[4]][1,],d=3)) ; lab<-c(lab,'F')
 if (sum(CV[clip[1,'FI']:clip[2,'FI']]) >0) { out<-cbind(out, round(CV[clip[1,'FI']:clip[2,'FI']],2)); lab<-c(lab,'CV');}
  out<- cbind(out,round(B0[[3]][1,],d=3)) ; lab<-c(lab,'Stock numbers (thousands)')
 if (sum(CV[clip[1,'N']:clip[2,'N']]) >0) { out<-cbind(out, round(CV[clip[1,'N']:clip[2,'N']],2)); lab<-c(lab,'CV');}

 dimnames(out)[2]<-list(lab) 
 dimnames(out)[1]<-list(seq(fa,la))
 write.table(out,row.names = F,file.path(data.path,"short-term_sensitivity_input.out"))
 out
 
 ######################################################
 
 # Make forcast
 incl.CV<-T
 make.contrib.plot<-T
 
 steps<-seq(0.0,1.0,0.2)
 steps<-0.5
 
 results<-matrix(NA,ncol=15,nrow=length(steps))
 dimnames(results)[2]<-list(c('TSB1','SSB1','Fmult1','Fbar1','Land1','TSB2','SSB2','Fmult2','Fbar2','Land2','CV(Land2)',
                   'TSB3','CV(TSB3)','SSB3','CV(SSB3)'))
 i<-0
 for (s in (steps)) {
   a<-predict(pars=values,Fmult2=s)
   i<-i+1
   results[i,'TSB1']<-  round(sum(a[[1]][1,]))
   results[i,'SSB1']<-  round(sum(a[[2]][1,])) 
   results[i,'Fmult1']<-  round(a[[6]],2) 
   results[i,'Fbar1']<-   round(Fbar,2)*results[i,'Fmult1']
   results[i,'Land1']<-round(sum(a[[3]][1,]))

   
   results[i,'TSB2']<-  round(sum(a[[1]][2,])) 
   if (incl.CV) {
     #varGrad<-partialDerivatives(parm=values,varNo=1,varYno=2,Fmult2=s)
     #results[i,'CV(TSB2)']<- round(sqrt(varGrad[[3]])/varGrad[[1]],2) 
   }

   results[i,'SSB2']<-  round(sum(a[[2]][2,])) 
   if (incl.CV) {
     #varGrad<-partialDerivatives(parm=values,varNo=2,varYno=2,Fmult2=s)
     #results[i,'CV(SSB2)']<- round(sqrt(varGrad[[3]])/varGrad[[1]],2) 
   }

   results[i,'Fmult2']<-   s 
   results[i,'Fbar2']<-    round(Fbar*s,3)
   results[i,'Land2']<-round(sum(a[[3]][2,]))
   if (incl.CV) {
     varGrad<-partialDerivatives(parm=values,varNo=3,varYno=2,Fmult2=s)
     if (varGrad[[1]]>0) results[i,'CV(Land2)']<- round(sqrt(varGrad[[3]])/varGrad[[1]],2) 
   }

   if (make.contrib.plot) {

     yield<-a[[3]][1,]    #landings
     names(yield)<-paste('age',seq(fa,la))
     tit2<-paste(ly+1 ,"Yield")
     pie(yield, main=tit2,cex.main=1)
     
     ssb<-a[[2]][1,]    #SSB
     names(yield)<-paste('age',seq(fa,la))
     tit2<-paste(ly+1 ,"SSB")
     pie(yield, main=tit2,cex.main=1)

     yield<-a[[3]][2,]    #landings
     names(yield)<-paste('age',seq(fa,la))
     tit2<-paste(ly+2 ,"Yield")
     pie(yield, main=tit2,cex.main=1)
     
     ssb<-a[[2]][2,]    #SSB
     names(yield)<-paste('age',seq(fa,la))
     tit2<-paste(ly+2 ,"SSB")
     pie(yield, main=tit2,cex.main=1)

     ssb<-a[[2]][3,]    #SSB
     names(yield)<-paste('age',seq(fa,la))
     tit2<-paste(ly+3 ,"SSB")
     pie(yield, main=tit2,cex.main=1)
  }

   results[i,'TSB3']<-  round(sum(a[[1]][3,])) 
   if (incl.CV) {
     varGrad<-partialDerivatives(parm=values,varNo=1,varYno=3,Fmult2=s)
     results[i,'CV(TSB3)']<- round(sqrt(varGrad[[3]])/varGrad[[1]],2) 
   }
   
   results[i,'SSB3']<-  round(sum(a[[2]][3,])) 
   if (incl.CV) {
     varGrad<-partialDerivatives(parm=values,varNo=2,varYno=3,Fmult2=s)
     results[i,'CV(SSB3)']<- round(sqrt(varGrad[[3]])/varGrad[[1]],2) 
   }
   
     ssb<-a[[2]][3,]    #landings
     names(yield)<-paste('age',seq(fa,la))
     tit2<-paste(ly+3 ,"SSB")
     pie(yield, main=tit2,cex.main=1)

 }

 results
 
