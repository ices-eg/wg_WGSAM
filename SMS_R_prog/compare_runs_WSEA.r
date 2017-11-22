
nox<-2; noy<-3;
paper<-F        # graphics on paper=file (TRUE) or on screen (FALSE)
cleanup()

first.year.on.plot<-1974
last.year.on.plot<-2016
doGrid<-T

first.pch<-1    # first pch symbol
first.color<-1   # first color

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only

dirs<-c("NS_63-10-sep-2014","NS_key-2014-ver15_codage3","NS_key-2014-ver17")
labels<-c("2011", "2014-key","2015-key")

dirs<-c("NS_key-2011","NS_key-2014-ver13-final-key-run", "NorthSeaKeyRun")
labels<-c("2011-run", "2014-run","2015-run")



dirs<-c("NS_key-2014-ver17","NS_key-2017-ver02")
labels<-c("2014-run", "2017-run")



Init.function() # get SMS.contol object  including sp.names

for (dir in dirs) {
   Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
   a<-Read.summary.data(dir=file.path(root,dir),read.init.function=F)
   a<-subset(a,(Year>=first.year.on.plot & Year<=last.year.on.plot & Z>0))
   a<-data.frame(scenario=labels[which(dirs==dir)],Variable="M2",Year=a$Year, quarter=a$Quarter,Species=a$Species, Age=a$Age,west=a$west)
   if (dir==dirs[1]) alld<-a else alld<-rbind(alld,a)
}

head(a)
all<-subset(a,Species=='Cod' & quarter==1)
by(all,list(all$Species),function(x ) {
trellis.device(device = "windows", 
               color = T, width=5, height=9,pointsize = 2,
               new = T, retain = FALSE)


print(xyplot( west~Year|paste('Age:',Age),groups=paste(quarter,scenario), data=x,
  type='b',lwd=1 , layout=c(3,3),   ylab='Weight in the stock',
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.5),
   auto.key = list(space = "bottom", points = T, lines = F,cex=0.9, columns = 2) ,
     xlim=c(1976,2017),
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