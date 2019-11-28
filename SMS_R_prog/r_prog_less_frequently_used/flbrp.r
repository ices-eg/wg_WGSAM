
library(FLCore)

##############################
SMS.dat<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))

SMS.stocks<-SMS2FLStocks(sumfile=file.path(data.path,'summary.out'),
            bio.interact=F, read.input=TRUE, read.output=TRUE,control=SMS.dat)
summary(SMS.stocks)
summary(SMS.stocks[[1]])

slotNames(SMS.stocks[[1]])


# does not work? plot(SMS.stocks[[1]])

SMS.stocks<-lapply(SMS.stocks,function(x){ window(x, start = 1974, end = 2010)} )

if (F) {
  a<-window(SMS.stocks[[1]], start = 2007, end = 2010)

  a@catch
  a@landings
  a@discards
  
  a[,,,1,]@catch.n
  a[,,,1,]@landings.n
  a[,,,1,]@discards.n
  a[,,,1,]@catch.n/a[,,,1,]@landings.n

  a[,,,1,]@catch.wt
  a[,,,1,]@landings.wt
  a[,,,1,]@discards.wt
}

###########################################################################
# make objects with "annual data"

sp<-1 # select species

if (sp==3) SMS.stocks[[sp]]<- setPlusGroup(SMS.stocks[[sp]],7)

stockDims <- FLQuant(dimnames=list(age = SMS.stocks[[sp]]@range["min"]:SMS.stocks[[sp]]@range["max"],
                                   year = SMS.stocks[[sp]]@range["minyear"]:SMS.stocks[[sp]]@range["maxyear"], unit = 'thousands',
                                   season = 'all', area = 'unique'))
             
a<- FLStock(name =sp.names[sp], desc = "test",catch.n = stockDims)

a@range['minfbar']<-SMS.dat@avg.F.ages[sp,1]
a@range['maxfbar']<-SMS.dat@avg.F.ages[sp,2]
if (sp==3) a@range['plusgroup']<-NA

a@catch<-seasonSums(SMS.stocks[[sp]]@catch)
a@catch.n<-seasonSums(SMS.stocks[[sp]]@catch.n)
units(a@catch.n)<-"thousands"
a@catch.wt<-apply(SMS.stocks[[sp]]@catch.n[,,,,,]*SMS.stocks[[sp]]@catch.wt[,,,,,],c(1,2),sum,na.rm=T)/seasonSums(SMS.stocks[[sp]]@catch.n)
units(a@catch.wt)<-"kg"
#a@catch.wt[1,,,,,]<-0

a@discards<-seasonSums(SMS.stocks[[sp]]@discards)
a@discards.n<-seasonSums(SMS.stocks[[sp]]@discards.n)
units(a@discards.n)<-"thousands"
a@discards.wt<-apply(SMS.stocks[[sp]]@discards.n[,,,,,]*SMS.stocks[[sp]]@discards.wt[,,,,,],c(1,2),sum,na.rm=T)/seasonSums(SMS.stocks[[sp]]@discards.n)
units(a@discards.wt)<-"kg"
#a@discards.wt[1,,,,,]<-0

a@landings<-seasonSums(SMS.stocks[[sp]]@landings)
a@landings.n<-seasonSums(SMS.stocks[[sp]]@landings.n)
units(a@landings.n)<-"thousands"
a@landings.wt<-apply(SMS.stocks[[sp]]@landings.n[,,,,,]*SMS.stocks[[sp]]@landings.wt[,,,,,],c(1,2),sum,na.rm=T)/seasonSums(SMS.stocks[[sp]]@landings.n)
units(a@landings.wt)<-"kg"
#a@landings.wt[1,,,,,]<-0


a@stock<-SMS.stocks[[sp]]@stock[,,,1,,] # first quarter data
units(a@stock)<-'tonnes'
a@stock.n<-SMS.stocks[[sp]]@stock.n[,,,1,,] # first quarter N
a@stock.n[1,,,,,]<-SMS.stocks[[sp]]@stock.n[1,,,3,,] # recruit age 0 (index 1) in quarter 3

a@stock.wt<-SMS.stocks[[sp]]@stock.wt[,,,1,,] # first quarter N
a@stock.wt[1,,,,,]<-SMS.stocks[[sp]]@stock.wt[1,,,3,,] # recruit age 0 (index 1) in quarter 3

units(a@stock.n)<-'thousands'

a@m<-seasonSums(SMS.stocks[[sp]]@m)
a@mat<-SMS.stocks[[sp]]@mat[,,,1,,]

a@harvest<-seasonSums(SMS.stocks[[sp]]@harvest)
units(a@harvest)<-"f"

# one stock only a<-SMS.stocks
a@harvest.spwn[,,,,,]<-0
a@m.spwn[,,,,,]<-0

summary(a)

if (F) {
  summary(a)
  b<-window(a, start = 2007, end = 2010)
  b@catch
  b@landings
  b@discards
  b@catch.n
  b@landings.n
  b@discards.n
  b@catch.wt
  b@landings.wt
  b@discards.wt

}

# stock recruitment
sr<-as.FLSR(a)

summary(sr)

frange<-seq(0, 1, 0.01)
# select regression model
if (sp==1) {
  sr<-window(sr, start = 1989, end = 2009)
  #  sr<-window(sr, start = 1974, end = 2009)

  model(sr)<-segreg()
  #  model(sr)<-ricker()
}
if (sp==2) {
  sr<-window(sr, start = 1974, end = 2009)
  model(sr)<-ricker()
}
if (sp==3) {
#  sr<-window(sr, start = 1990, end = 2009)
    sr<-window(sr, start = 1974, end = 2009)
    sr<-transform(sr,ssb=ssb/1000,rec=rec/1000)
  model(sr)<-segreg()
   #model(sr)<-ricker()
}

cleanup()
#
model(sr)
sr<-fmle(sr)
plot(sr)
summary(sr)
 ## Check fit
#profile(sr)


#  reference points
library(FLBRP)

## ## estimate reference points. Create the corresponding FLBRP object
bfy<-2000  # period for calculating mean weight and exploitation pattern
bly<-2010

X11()
# without S/R function
Brp  <-FLBRP(window(a,start=bfy, end=bly),fbar =frange )
#summary(Brp)
# And fit it
Brp <-brp(Brp)
plot(Brp)
refpts(Brp)

X11()
# with S/R function
Brp.sr  <-FLBRP(window(a,start=bfy, end=bly),sr=sr,fbar =frange)
#model(Brp.sr)
#params(Brp.sr)
# And fit it
Brp.sr<-brp(Brp.sr)
plot(Brp.sr)
refpts(Brp.sr)
 
