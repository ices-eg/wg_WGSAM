
paper<-F                   # output on paper (=TRUE) or screen (=FALSE)
file.name<-'ICES stock summary'             # graphical output file if paper<-TRUE
Portrait<-F                 # graphical output orientation
include.terminal.year <- F          # plot terminal year as well?
first.year<- 1963                #first year on plot, negative value means value defined by data
last.year<- 2007                 #last year on plot
incl.M2.plot<-T

##########################################################################

cleanup()
 
if (paper) dev<-"wmf" else dev<-"screen"
nox<-1; noy<-1;
noxy<-nox*noy

ref<-Read.reference.points()

Init.function()

dat<-Read.summary.data(extend=include.terminal.year,read.init.function=F)

# by length
d<-subset(dat, Lsea>0 &Lsea<600 & Species %in% c('Cod','Haddock','Whiting','Herring','Sandeel','Nor. pout'))
sort(unique(d$Lsea))
step<-3
d$size<-trunc(d$Lsea/step/10)*step
sort(unique(d$Lsea))
 
a<-tapply(d$BIO,list(d$Species,d$size),sum,na.r=T)/sum(d$BIO,na.rm=T)*100  
a[is.na(a)]<-0
rs<-rowSums(a,na.rm=T)
b<-a[rs>0,]
barplot(b,main='Biomass, all years, all quarters combined',ylab='%',xlab='Length(cm)',col=rainbow(10), legend = rownames(b), )


# by weight
d<-subset(dat, west>0  & Species %in% c('Cod','Haddock','Whiting','Herring','Sandeel','Nor. pout'))

step<-1
d$size<-trunc(log(d$west)/step)*step

a<-tapply(d$BIO,list(d$Species,d$size),sum,na.r=T)/sum(d$BIO,na.rm=T)*100  
a[is.na(a)]<-0
rs<-rowSums(a,na.rm=T)
b<-a[rs>0,]
barplot(b,main='Biomass, all years, all quarters combined',ylab='%',xlab='log(weight)',col=rainbow(10), legend = rownames(b), )



# by weight all species
d<-subset(dat, west>0 )

step<-1
d$size<-trunc(log(d$west)/step)*step

a<-tapply(d$BIO,list(d$Species,d$size),sum,na.r=T)/sum(d$BIO,na.rm=T)*100  
a[is.na(a)]<-0
rs<-rowSums(a,na.rm=T)
b<-a[rs>0,]
barplot(b,main='Biomass, all years, all quarters combined',ylab='%',xlab='log(weight)',col=rainbow(10), legend = rownames(b), )
