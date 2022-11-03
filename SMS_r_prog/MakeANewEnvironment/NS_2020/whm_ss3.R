
# library(devtools)
# devtools::install_github("r4ss/r4ss")

library(r4ss)

species<-"W_H"
ss3_dir<-file.path(root,exchangeDir,'ByStock',species,'SS3')
myreplist <- SS_output(dir=ss3_dir,
                       repfile = "Report.sso",
                       compfile = "CompReport.sso",covar = FALSE,
                       wtfile = "wtatage.ss_new", warnfile = "warning.sso",forecast=FALSE)


# make a lot of plots
# SS_plots(myreplist)

str(myreplist,1,list.len=200)

#head(myreplist$natlen)
#myreplist$lbins
#myreplist$lbinspop

#head(myreplist$lendbase)
#head(myreplist$cpue)
#head(myreplist$age_comp_fit_table)
#tail(myreplist$condbase)
#summary(myreplist$condbase)


#ALK<-myreplist$ALK
#dim(ALK)  # length, age, fleet  (but no year)
#a<-ALK[,,2]
#a
#colSums(a)  # adds up to one



myreplist$Growth_Parameters


von_Bertalanffy<-function(age) {
  l<-myreplist$Growth_Parameters$Linf *(1 - exp(-myreplist$Growth_Parameters$K*(age-myreplist$Growth_Parameters$A_a_L0)))
  return(l)
}

von_Bertalanffy(age=4)
#plot(1:20,von_Bertalanffy(age=(1:20)))

#head(myreplist$M_at_age)
#head(myreplist$Z_at_age)


# Data for SMS (all by length classess)


nl<-myreplist$lbinspop    # length bins used for population
write.csv(nl,file.path(ss3_dir,'lengthBin.csv'),row.names =FALSE)


# N at length
n<-myreplist$natlen
cn<-colnames(n)
cn<-gsub("Beg/Mid","BM",cn)
colnames(n)<-cn
n14<-subset(n, Era=='TIME')
n1<-subset(n14,BM=='B') %>% mutate(Seas=1)
n3<-subset(n14,BM=='M') %>% mutate(Seas=3)

n<-rbind(n1,n3)


convL<-function(x,v.names='cn') {
  x<-subset(x,select=c("Yr","Seas",as.character(nl)))
  colnames(x)<-c("Yr","Seas",as.character(nl))
  
  x<-reshape(x,direction='long',varying=as.character(nl),v.names=v.names)
  x$len<-nl[x$time]
  x<-subset(x,select=c("Yr","Seas",'len',v.names))
  return(x)
}


N<-convL(x=n,v.names='n')
head(N)

#head(myreplist$M_at_age)
N2<-mutate(N,Seas=if_else(Seas==1,2,4),n=n*exp(-0.15/4))  # Ignore F and growth

N<-rbind(N,N2)

N$w<-myreplist$Growth_Parameters[1,'WtLen1']*N$len**myreplist$Growth_Parameters[1,'WtLen2']

#plot(N$len,N$w)

head(N)

summary(N)
write.csv(N,file=file.path(ss3_dir,'SS3_results.csv'),row.names =FALSE)




