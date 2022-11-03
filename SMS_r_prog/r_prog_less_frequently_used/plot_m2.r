
paper<-T                   # output on paper (=TRUE) or screen (=FALSE)
Portrait<-T                 # graphical output orientation

first.year<- 1974                #first year on plot, negative value means value defined by data
last.year<- 2021                #last year on plot

OperatingModel<-F
##########################################################################

cleanup()
 
if (paper) dev<-"png" else dev<-"screen"
file.name<-'M2summary'
#dev<-"dummy"
 nox<-3; noy<-3;
noxy<-nox*noy

ref<-Read.reference.points()

Init.function()

if (!OperatingModel) dat<-Read.summary.data(read.init.function=F)
if (OperatingModel) {
  dat1<-Read.summary.data(extend=F,read.init.function=F)

  dat<-Read.summary.data(infile="OP_summary.out",read.init.function=F)
  if (SMS.control@no.areas >1) {
    dat$N.bar<-dat$N*(1-exp(-dat$Z))/dat$Z
    dat$DM<-dat$M*dat$N.bar
    dat$DM1<-dat$M2*dat$N.bar
    dat$DM2<-dat$M2*dat$N.bar
    dat$DF<-dat$F*dat$N.bar
    dat$DZ<-dat$Z*dat$N.bar
    dat$Nwest<-dat$N*dat$west
    dat$Cweca<-dat$C*dat$weca

    dat<-aggregate(cbind(DM,DM1,DM2,DF,DZ,N,C,Nwest,Cweca,Yield,CWsum,BIO,SSB)~Species+Year+Quarter+Species.n+Age,sum,na.rm=T,data=dat)
    dat$Z<- -log((dat$N-dat$DZ)/dat$N)
    dat$M<-dat$DM/dat$DZ*dat$Z
    dat$M1<-dat$DM1/dat$DZ*dat$Z
    dat$M2<-dat$DM2/dat$DZ*dat$Z
    dat$F<-dat$DF/dat$DZ*dat$Z

    dat$weca<-dat$Cweca/dat$C
    dat[is.na(dat$weca),'weca']<-0
    dat$west<-dat$Nwest/dat$N
   }

  dat$N.bar<-dat$N*(1-exp(-dat$Z))/dat$Z
  dat$C<-NULL
  dat$N_dist<-NULL
  dat$Area<-NULL
  dat1<-subset(dat1,select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,BIO,SSB))
  dat <-subset(dat, select=c(Species,Year,Quarter,Species.n,Age,M1,M2,M,F,Z,N,N.bar,west,weca,Yield,CWsum,BIO,SSB))

  dat<-rbind(dat1,dat)
}
#tapply(dat$Yield,list(dat$Year,dat$Species),sum)


dat<-subset(dat,Year<=last.year )
if (first.year>0) dat<-subset(dat,Year>=first.year )

for (sp in (first.VPA:nsp)){
    sp.name<-sp.names[sp]

    newplot(dev,nox,noy,filename=paste(file.name,'_',sp.name,sep=''),Portrait=Portrait,w8=8,w11=10)
    par(mar=c(3,4,3,2))

    s<-subset(dat,Species.n==sp)
    b<-tapply(s$M2,list(s$Year,s$Age),sum)
    b<-apply(b,2,mean)
    M2<-tapply(s$M2,list(s$Quarter,s$Year,s$Age),sum)
    M2[is.na(M2)]<-0
    for (a in (1:length(b))) {
      if (b[a]>0.001 & a<=noxy) barplot(M2[,,a],space=0.5,xlab='',ylab='',main=paste(sp.name,", Age ",a-1,sep=''),col=c(1,2,3,4))
    }
    if (paper) cleanup()
}

dat$M1M2<-dat$M1+dat$M2
library(mgcv)
b<-aggregate(M1M2~Species.n+Species+Year+Age,sum,data=dat )

  by(b,list(b$Species),function(x) {
   # png(filename=file.path(data.path,paste0('M2_smooth_',b[1,"Species"],'.png')),width=600,height=600 )
   X11()
     print(ggplot(x,aes(Year,M1M2)) +
            theme_bw() +
            geom_point() +
            geom_smooth(method = "gam",formula = y ~ s(x, bs = "cs")) +
            #facet_wrap(~ paste0(Species,' Age:',Age), scale="free_y") +
            facet_wrap(~ paste0(Species,' Age:',formatC(Age,width=2))) +
            labs(x="Year", y="M=M1+M2",title=""))
    
  })
 # cleanup()

 # a<-gam(M1M2 ~s(Year,bs='cs'),data=subset(b,Species=='Herring' & Age==1))
#  a
  #predict(a)
  

 bb<- by(b,list(b$Species,b$Age),function(x) {
     a<-gam(M1M2 ~s(Year,bs='cs'),data=x)
     return(data.frame(Species=x$Species,Species.n=x$Species.n,Year=x$Year,Age=x$Age,M=predict(a)))
  })

 M<-do.call(rbind,bb)
 
 
 
 tab_annual_sum_data<-function(data,vari='C.obs',outfile,title,decimals=0,tableNo=1,my.species=c(first.VPA:SMS.control@no.species)) {
   if (file.exists(file.path(data.path,outfile))) file.remove(file.path(data.path,outfile))
   for (sp in my.species) {
     a<-subset(data,Species.n==sp)
     if (dim(a)[1] >0) {
       if (vari=='C.obs') tab1<-tapply(a$C.obs,list(a$Year,a$Age),sum)
       if (vari=='F') tab1<-tapply(a$F,list(a$Year,a$Age),sum)
       if (vari=='M2') tab1<-tapply(a$M2,list(a$Year,a$Age),sum)
       if (vari=='M1M2') tab1<-tapply(a$M2+a$M1,list(a$Year,a$Age),sum)
       if (vari=='M') tab1<-tapply(a$M,list(a$Year,a$Age),sum)
       if (vari=='ration') tab1<-tapply(a$ration,list(a$Year,a$Age),sum)
       if (vari=='consum') tab1<-tapply(a$consum,list(a$Year,a$Age),sum)
       
       if (sum(tab1,na.rm=T)>0) {
         colnames(tab1)<-sort(unique(a$Age))
         xtab(tab1, caption=paste("Table ",tableNo,'  ',sp.names[sp],title),
              cornername='Year/Age',
              file=file.path(data.path,'_tmp.html'), dec=rep(decimals,length(unique(a$Age))), width='"100%"')
         if (length(my.species)==1) {
           file.copy(file.path(data.path,'_tmp.html'), file.path(data.path,outfile), overwrite = T, recursive = FALSE)
         } else file.append(file.path(data.path,outfile), file.path(data.path,'_tmp.html'))
       }
     }
   }
 }
 
 tab_annual_sum_data(data=M,vari='M',outfile='Msmooth.html',title='Smoothed M',decimals=3)


   