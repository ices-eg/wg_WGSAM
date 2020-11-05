
nox<-3; noy<-3;
paper<-T        # graphics on paper=file (TRUE) or on screen (FALSE)
if (makeAllGraphs) paper<-TRUE

sumQuarerly<-T  # use sum of quarterly M2, or calc M2 from annual numbers dead.


run.ID<-'M2'         # file id used for paper output
cleanup()

first.year.on.plot<-1974
last.year.on.plot<-2018
doGrid<-T

#incl.sp<-c("Cod")                      # species to be included. Name(s) or "all"
incl.sp<-"all"

first.pch<-1    # first pch symbol
first.color<-1   # first color

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only


if (F) {    # sometimes the dir and labels are defined outside this script !
  dirs<-c("baltic-2012-keyRun-updated","baltic-2012-keyRun-4-areas")
  labels<-c("Key run 2012 updated","4 areas 2012")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V01_new_stom")
labels<-c("key-run 2012","2019 new stomachs")


dirs<-c("Baltic-2019-V01_new_stom","Baltic-2019-V02_new_stom","Baltic-2019-V03_new_stom")
labels<-c("V01","V02","V03")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V01_new_stom","Baltic-2019-V01_newY10_stom")
labels<-c("2012 key run","new stom","newY10 stom")


dirs<-c("Baltic-2019-V04_new_stom","Baltic-2019-V05_new_stom")
labels<-c("V04","V05 cons=0.84")


dirs<-c("Baltic-2019-V02_new_stom","Baltic-2019-V02b_new_stom")
labels<-c("V02","V02b")


#dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V02_new_stom","Baltic-2019-V04_new_stom","Baltic-2019-V05_new_stom")
#labels<-c("key-run 2012","V02 (wrong) size selection","V04 settings as in 2012","V05 cons fac (0.84) ")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V04_new_stom","Baltic-2019-V05_new_stom")
labels<-c("2012 key run","V04 settings as in 2012","V05 cons fac (0.84) ")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V01_old_stom","Baltic-2019-V02_old_stom","Baltic-2019-V04_old_stom")
labels<-c("2012 key run","V01","V02","V04")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V02b_new_stom","Baltic-2019-V02b_newY10_stom")
labels<-c("2012 key run","V02b by year","V02b by 10 Years")


dirs<-c("Baltic-2019-V02b_new_stom","Baltic-2019-V02bOverlap_new_stom")
labels<-c("V02b","V02b with overlap")

 #####

dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V04_new_stom","Baltic-2019-V05_new_stom")
labels<-c("2012 key run","V04 settings as in 2012","V05 cons fac (0.72) ")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V04_new_stom","Baltic-2019-V04_old_stom")
labels<-c("2012 key run","V04 new stomachs","V04 old stomachs")



dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-V02b_new_stom","Baltic-2019-V04_new_stom")
labels<-c("2012 key run","V02b size slection","V04 within range")


dirs<-c("Baltic-2019-V02b_new_stom","Baltic-2019-V02bOverlap_new_stom")
labels<-c("V02b","V02b with overlap")


dirs<-c("Baltic-2019-V02b_new_stom","Baltic-2019-V02bCons_new_stom")
labels<-c("V02b","V02b cons estimate (0.56")


dirs<-c("Baltic-2019-V02b_new_stom","Baltic-2019-key-run")
labels<-c("V02b","key-run")


dirs<-c("Baltic-2019-V02b_new_stom","Baltic-2019-test2")
labels<-c("key-run","spline")


dirs<-c("Baltic-2019-key-run-M1","Baltic-2019-test01","Baltic-2019-test02","Baltic-2019-test03")
labels<-c("key-runM1","test01","test02","test03")


dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-keyRun")
labels<-c("2012 key run","2019 key run")

dirs<-c("Baltic-2019-no_size_selection0","Baltic-2019-no_size_selection4","Baltic-2019-keyRun")
labels<-c("Uniform","Constraint uniform","2019 key run")

dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-keyRun")
labels<-c("2012 keyrun","2019 keyrun")


}


dirs<-c("Baltic-2019-old_stomachs","Baltic-2019-old-new_stomachs","Baltic-2019-keyRun")
labels<-c("old stomachs","old & new stomachs","2019 keyrun")


dirs<-c("Baltic-2019-stom10","Baltic-2019-keyRun")
labels<-c("Aggregated","2019 keyrun")


dirs<-c("Baltic-2019-OtherFoodOverlap","Baltic-2019-keyRun")
labels<-c("OtherFood change","2019 keyrun")


dirs<-c("Baltic-2019-consumptionEstimate","Baltic-2019-keyRun")
labels<-c("Lower consum","2019 keyrun")


dirs<-c("Baltic-2019-stom5","Baltic-2019-stom10","Baltic-2019-keyRun")
labels<-c("5 years stomach","10 years stomach","2019 keyrun")




Init.function() # get SMS.contol object  including sp.names


if (sumQuarerly) { # calc M2 as sum of quarterly M2
  for (dir in dirs) {
     Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
     a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & M2>0),
               select=c(Species, Year,Age,M2))
     M2<-data.frame(scen=labels[which(dirs==dir)],vari='M2',a)
     names(M2)<-c("scenario","Variable","Species","Year","Age","Value")

      

   if (dir==dirs[1]) all<-rbind(M2) else all<-rbind(all,M2)
  }
  values<-tapply(all$Value,list(all$Year,all$scenario,all$Species,all$Variable,all$Age),sum)

} else {  # calc M2 an an annual basis
  for (dir in dirs) {
     Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
     a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
     a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & Z>0))
     a<-data.frame(scenario=labels[which(dirs==dir)],Variable="M2",Year=a$Year, Species=a$Species, Age=a$Age,Z=a$Z,
           dead=a$N.bar*a$Z,deadm2=a$N.bar*a$M2)
     if (dir==dirs[1]) alld<-a else alld<-rbind(alld,a)
  }

  list.ZF<- list(alld$Year,alld$scenario,alld$Species,alld$Variable,alld$Age)
  dead<-tapply(alld$dead,list.ZF,sum,na.rm=T)
  deadM2<-tapply(alld$deadm2,list.ZF,sum)
  Z<-tapply(alld$Z,list.ZF,sum)
  values<-deadM2/dead*Z
}

y<-as.numeric(dimnames(values)[[1]])


if (paper) dev<-"png" else dev<-"screen"
if (incl.sp=="all") sp.plot<-sp.names else sp.plot<-incl.sp

len.dir<-length(dirs)


 plotvar<-function(sp=sp,vari='M2',ylab='') {
   if (sp %in% dimnames(values)[[3]]) {
    v<-values[,,sp,vari,]
    #print(v)
    maxval<-max(v,na.rm=T)
    if (maxval>0) {
      if ((gi %% (nox*noy))==0  | gi==0) {
        filename<-paste0("compareM2_",run.ID,'_',sp)
        if (makeAllGraphs) filename=file.path(compare.dir,filename)
        newplot(dev,nox,noy,filename=filename,Portrait=F);

         par(mar=c(0,0,0,0))
        # make legends
        if (paper) lwds<-2 else  lwds<-2
        plot(10,10,axes=FALSE,xlab=' ',ylab=' ',xlim=c(0,1),ylim=c(0,1))
        legend("center",legend=labels,col=first.color:(first.color+len.dir-1),
              pch=first.pch:(first.pch+len.dir-1),cex=2,title=paste0('M2: ',sp))
        gi<<-gi+1

        par(mar=c(3,3,3,1)) # c(bottom, left, top, right)
      }
   
      gi<<-gi+1

      ages<-dimnames(v)[[3]][1:min(nox*noy-1,dim(v)[[3]])]
      for (ag in ages) {
        maxval<-max(v[,,ag],na.rm=T)
        minval<-min(0,v[,,ag],na.rm=T)
        if (maxval>0) {
          plot(y,v[,labels[1],ag],main=paste("age",ag),xlab="",ylab=ylab,type='b',lwd=lwds,ylim=c(minval,maxval),
                    col=first.color,pch=first.pch)
          if (doGrid) grid()
          for (i in (2:len.dir)) {
            if (paper) lwds<-2
            else  lwds<-2;
            lines(y,v[,labels[i],ag],col=first.color+i-1,pch=first.pch+i-1,type='b',lwd=lwds)
           }
        }
       }
     }
   }
  }
  
 for (sp in (sp.plot)) {
  gi<-0
  plotvar(sp=sp,vari="M2",ylab="M2")
}

if (paper) cleanup();
Init.function()


##########################
if (F) {
aa<-arr2dfny(values)
names(aa)<-list("Year","label","Species","variable","Age","M2")

all<-subset(aa,Age %in% c("0","1","2")  & !(Species %in% c("NS Mackerel","Saithe","W Mackerel")))

all$Species.n<-match(all$Species,sp.names)
all$Lab<-match(all$label,labels)
all$label<-'abc'
all$Species<-"abc"

fi<-"a.dat"
write.table(all,file=fi,quote=F,row.names=F)
all<-read.table(file=fi,header=T)
all$Species<-sp.names[all$Species.n]
all$label<-labels[all$Lab]
all$age<-paste("Age ",all$Age,all$Species)
#all$age<-paste("Age ",all$Age)

allZero<-subset(all,Year==1990)
allZero$Year<-0
allZero$M2<-0

all<-rbind(all,allZero)
all$label<-substring(all$label,4) 

all<-all[order(all$label,all$Species.n,all$Age,all$Year),]
              


cleanup() 

by(all,list(all$Species.n),function(x ) {
trellis.device(device = "windows", 
               color = F, width=5, height=9,pointsize = 2,
               new = T, retain = FALSE)


print(xyplot( M2~Year|age,groups=label, data=x,
  type='b',lwd=1 , layout=c(1,3),   ylab='Predation mortality',
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.5),
   auto.key = list(space = "bottom", points = T, lines = F,cex=0.9, columns = 2) ,
     xlim=c(1976,2000),
    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='free')
))
})
###################
# more species per plot

cleanup() 
all$age<-paste(all$Species,", age ",all$Age,sep='')
all$ageRev<-paste("age ",all$Age,", ",all$Species,sep='')
all<-all[order(all$label,all$Age,all$Species.n,all$Year),]            


all1<-subset(all,Species %in% c('Cod','Whiting','Haddock'))

trellis.device(device = "windows", 
               color = F, width=9, height=9,pointsize = 2,
               new = T, retain = FALSE)
 
print(xyplot( M2~Year|age,groups=label, data=all1,
  type='b',lwd=1 , layout=c(3,3), as.table=F,  ylab='Predation mortality',
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.5),
   auto.key = list(space = "bottom", points = T, lines = F,cex=0.9, columns = 3) ,
     xlim=c(1976,2000),
    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='free')
))

 
 ###################
# more species per plot, used for paper
cleanup()
trellis.device(device = "windows", 
               color = F, width=10, height=11,pointsize = 2,
               new = T, retain = FALSE)
 
#all1<-subset(all,Species %in% c('Cod','Whiting','Haddock'))
all1<-subset(all,Species %in% c('Herring','Nor. pout','Sandeel'))

xyplot( M2~Year|ageRev,groups=label, data=all1, 
 as.table=T,   ylab='Predation mortality',
   layout=c(3,3), 
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=0),
   auto.key = list(space = "bottom", points = T, lines = T,cex=1, columns = 2,title='Prey size selection', pch=c(1,2,3,4,5)) ,
     xlim=c(1976,2000),   # to allow relation='free' and having (0,0) included
    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='free'),
        panel = function(x, y, subscripts, groups) {
                 ltext(x=1976,y=0,labels=all1[subscripts,'age'],pos=4,cex=1.1)
         panel.superpose(x, y,subscripts=subscripts, groups=groups,type='b')  
         }
)


 
 ###################
# same as previous, but in color
cleanup()
trellis.device(device = "windows",
               color = T, width=12, height=12,pointsize = 12,
               new = T, retain = FALSE)
 

myCol<- c(1,2,3,4,5)
myLwd<-rep(3,5)
myLty<-c(1,2,3,4,5)

all1<-subset(all,Species %in% c('Cod','Whiting','Haddock'))
all1<-subset(all,Species %in% c('Herring','Nor. pout','Sandeel'))
all1<-subset(all,Species %in% c('Cod','Herring','Sandeel'))

print(xyplot( M2~Year|ageRev,groups=label, data=all1, 
 as.table=T,   ylab='Predation mortality',
   layout=c(3,3),      between = list(y = c(1, 1),x = c(1, 1)),     
   strip = strip.custom( bg='white'),par.strip.text=list(cex=0, lines=0), main=NA,
   key = list(space = "bottom", points = F, lines = T,cex=1, columns = 2,title='Prey size selection',col=myCol, lwd=myLwd*1.5,pch=NA,
               text = list(lab = as.character(unique(all1$label)),col=1) ),
   xlim=c(1976,2000),   # to allow relation='free' and having (0,0) included
   scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,relation='free'),

        panel = function(x, y, subscripts, groups) {
                 ltext(x=1976,y=0,labels=all1[subscripts,'age'],pos=4,cex=1.0 ,col=1,font=2)
         panel.superpose(x, y,subscripts=subscripts, groups=groups,type='l',
                          col=myCol,lwd=myLwd) 
         }
))

}