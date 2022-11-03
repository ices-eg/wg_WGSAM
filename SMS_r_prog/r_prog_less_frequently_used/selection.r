
# compare weight in the sea West.in and weight from length in the sea (lsea.in) and length weight relation

a<-Read.summary.data()
b<-Read.length.weight.relation()

ab<-merge(a,b)
ab$size<-ab$a*ab$Lsea^ab$b

cleanup()
by(ab,list(ab$Species),function(x) {
X11()
print(
 xyplot(west~size| paste(Species,Quarter), 
  auto.key = list(points = T, rectangles = F, space = "right"),
         panel = function(x,y) {
           panel.grid(h=-1, v= -1)
           panel.xyplot(x, y)
           panel.loess(x,y, span=1)
           panel.lmline(x, y)
           panel.abline(a =0, b = 1,col='red')
       },

 data=x)
 )
})

cleanup()
X11()

 xyplot(west~size| paste(Species), 
  auto.key = list(points = T, rectangles = F, space = "right"),scales = "free", 
         panel = function(x,y) {
           panel.grid(h=-1, v= -1)
           panel.xyplot(x, y)
           panel.loess(x,y, span=1)
           panel.lmline(x, y)
           panel.abline(a =0, b = 1,col='red')
       },
 data=ab)
                    
                      

 ################################
 # compare length distribution at age, with and without mesh size selection correction
cleanup()
a<-Read.LAK()

a<-subset(a,year==1991 & (((quarter==3 | quarter==4)& Age==0 ) |((quarter==1 | quarter==2)& Age==1 )))

a1<-subset(a,select=c(year,quarter, Age,Species,Length,LengthGroup ,proportion) )
a1$type<-'1) obs.'

a2<-subset(a,select=c(year,quarter, Age,Species,Length,LengthGroup ,proportion.adjusted) )
a2$type<-'2) adj.'
names(a2)=c('year','quarter','Age','Species','Length','LengthGroup','proportion','type')

aa<-rbind(a1,a2)

by(aa,list(aa$Species),function(x) {
X11()
print(barchart(proportion~as.factor(LengthGroup)| paste(Species,' age:', Age,' q:',quarter,sep=''), groups=type, 
  auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),
 data=x))
})

#######################
# compare mean length  at age, with and without mes size selection correction 

 
cleanup()

# length from weigt in the stock and length weight relation
a<-Read.summary.data()
b<-Read.length.weight.relation()

ab<-merge(a,b)
ab$Length<-(ab$west/ab$a)^(1/ab$b)
ab$type<-"4 west"
ab<-subset(ab,select=c(Year,Quarter,Species,Age,type,Length))
names(ab)=c('year','quarter','Species',"Age",'type','Length')

# read observed and adjusted length proportions
a<-Read.LAK()

a1<-subset(a,select=c(year,quarter, Age,Species,Length,proportion) )
a1$type<-'1 obs'

a2<-subset(a,select=c(year,quarter, Age,Species,Length,proportion.adjusted) )
a2$type<-'2 adj.'
names(a2)=c('year','quarter','Age','Species','Length','proportion','type')

aa<-rbind(a1,a2)
bb<-aggregate(aa$proportion*aa$Length,list(aa$year,aa$quarter,aa$Species,aa$Age,aa$type),sum)
names(bb)<-c('year','quarter','Species','Age','type','Length')
##

# length in the sea data
a<-Read.summary.data()
a$type<-'3 Lsea'
a<-subset(a,select=c(Year,Quarter,Species,Age,type,Lsea))
names(a)<-c('year','quarter','Species','Age','type','Length')

##

bb<-rbind(bb,a,ab)
bb2<-subset(bb,year==1991 & quarter==3 & Age<=2)

barchart(Length~as.factor(Age)| paste(Species,' q:',quarter,sep=''), groups=type, xlab='Age',
  auto.key = list(points = FALSE, rectangles = TRUE, space = "right",col=1:3),
 data=bb2)

 #######################
# Compare sum of relative stomach contents (observed and expected)  
cleanup()

stom<-Read.stomach.data()
stom<-transform(stom,year.range=ifelse(Year<=1981,'1977-81',ifelse(Year<1990,'1983-1987','1990-91')),year=paste("Y",Year,sep=''))

a<-subset(stom,Prey.length.mean>0,select=c(Prey, Prey.length.class,year.range,Quarter, stomcon, stomcon.hat ))

b1<-aggregate( a$stomcon,list(a$year.range, a$Prey, a$Prey.length.class,a$Quarter),mean)
names(b1)<-c('year.range','Prey','Length','Quarter','stom')
b1$type='obs'


b2<-aggregate( a$stomcon.hat,list(a$year.range,a$Prey, a$Prey.length.class,a$Quarter),mean)
names(b2)<-c('year.range','Prey','Length','Quarter','stom')
b2$type='sms'

bb<-rbind(b1,b2)


by(bb,list(bb$Prey),function(x) {
 X11()
 print(barchart(stom~as.factor(Length)| paste(Prey,year.range,Quarter), groups=type,
  auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),
  data=x))
})
 
 
 #######################
# By predator,  Compare sum of relative stomach contents (observed and expected)  

stom<-Read.stomach.data()
stom<-transform(stom,year.range=ifelse(Year<=1981,'1977-81',ifelse(Year<1990,'1983-1987','1990-91')),year=paste("Y",Year,sep=''))

a<-subset(stom,Prey.length.mean>0,select=c(Predator,Prey, Prey.length.class,year.range,Quarter, stomcon, stomcon.hat ))

b1<-aggregate( a$stomcon,list(a$Predator,a$year.range, a$Prey, a$Prey.length.class,a$Quarter),mean)
names(b1)<-c('Predator','year.range','Prey','Length','Quarter','stom')
b1$type='obs'


b2<-aggregate( a$stomcon.hat,list(a$Predator,a$year.range,a$Prey, a$Prey.length.class,a$Quarter),mean)
names(b2)<-c('Predator','year.range','Prey','Length','Quarter','stom')
b2$type='sms'

bb<-rbind(b1,b2)

cleanup()
by(bb,list(bb$Predator,bb$Prey),function(x) {
 X11()
 print(barchart(stom~as.factor(Length)| paste(Predator,Prey,year.range,Quarter), groups=type,
  auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),
 data=x))
})
 

cleanup()
by(bb,list(bb$Quarter,bb$Prey),function(x) {
 X11()
 print(barchart(stom~as.factor(Length)| paste(Predator,Prey,year.range,Quarter), groups=type,
  auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),
 data=x))
})


#######################
s1<-15
L50<-85

###
s2<-s1/L50
L75<-(s1+log(3))/s2
cat("s1:",s1,"  s2:",s2,"  L50:",L50," L75:",L75,"\n")

l<-seq(50,150,1)

sel<-1/(1+exp(s1-s1/L50*l))
plot(l,sel,col='blue')

for (s1 in (seq(5,12,1))) {
 lines(l,1/(1+exp(s1-s1/70*l)))
 s2<-s1/L50
L75<-(s1+log(3))/s2
cat("s1:",s1,"  s2:",s2,"  L50:",L50," L75:",L75,"\n")

}


plot(l,1/sel,col='blue')

for (s1 in (seq(5,12,1))) {
 lines(l,(1+exp(s1-s1/70*l)))
}


