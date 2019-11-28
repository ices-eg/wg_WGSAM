

first.year.on.plot<-1974
last.year.on.plot<-2013

nonFish<-c(1:8,15,16)
#select.sp<-c(7:17)

palette("default")                # good for clolorfull plots
#palette(gray(seq(0,.9,len=6)))  # gray scale for papers, use len =500 to get black only



dirs<-c("NS_key-2011","NS_key-2014-ver10")
labels<-c("2011","2014")

for (dir in dirs) {
  if ( file.access(file.path(root,dir,"sms.dat"), mode = 0)!=0)  stop(paste('Directory',dir,'does not exist'))
} 

Init.function() # get SMS.contol object  including sp.names

a2<-NULL
for (dir in dirs) {
    Init.function(dir=file.path(root,dir)) # get SMS.contol object  including sp.names
    print(dir)
    print(sp.names)
    file<-file.path(root,dir,'summary_table_raw.out')
    a<-read.table(file,header=TRUE)
    a$species<-sp.names[a$Species.n]
    a$label<-labels[ which(dirs==dir)]
    
    if (dir==dirs[1]) a2<-a else a2<-rbind(a2,a)
}

a<-subset(a2,species %in% c('Sandeel','N. sandeel','S. sandeel'))

a[a$label=="2014",'species']<-'Sandeel'

a2<-aggregate(cbind(SSB,TSB,SOP,Yield,Rec)~ Year+species+label,data=a,sum)
 
             

trellis.device(device = "windows", 
               color = T, width=9, height=9,pointsize = 2,
               new = TRUE, retain = FALSE)

print(xyplot( SSB/1000~Year|species,groups=label, data=a2,
  type='b',lwd=3 , layout=c(1,1), ylab='SSB',
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.7),
   auto.key = list(space = "bottom", points = T, lines = F,cex=1, columns = 2) ,

    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,
    relation='same'
    )
))

trellis.device(device = "windows", 
               color = T, width=9, height=9,pointsize = 2,
               new = TRUE, retain = FALSE)

print(xyplot( Rec/1E6~Year|species,groups=label, data=a2,
  type='b',lwd=3 , layout=c(1,1), ylab='Recruitment',
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.7),
   auto.key = list(space = "bottom", points = T, lines = F,cex=1, columns = 2) ,

    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,
    relation='same'
    )
))


trellis.device(device = "windows", 
               color = T, width=9, height=9,pointsize = 2,
               new = TRUE, retain = FALSE)

print(xyplot( Yield/1000~Year|species,groups=label, data=a2,
  type='b',lwd=3 , layout=c(1,1), ylab='Yield',
   strip = strip.custom( bg='white'),par.strip.text=list(cex=1, lines=1.7),
   auto.key = list(space = "bottom", points = T, lines = F,cex=1, columns = 2) ,

    scales = list(x = list( cex=0.8), y= list(cex=0.8),alternating = 1,
    relation='same'
    )
))

######################

Init.function()

