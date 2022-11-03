
library(FLCore)
data(ple4)  # to get an "approved" object

summary(ple4)


##############################
SMS.dat<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))

SMS.stocks<-SMS2FLStocks(sumfile=file.path(data.path,'summary.out'),
            bio.interact=F, read.input=TRUE, read.output=TRUE,control=SMS.dat)
summary(SMS.stocks)

summary(SMS.stocks[[1]])

summary(ple4)

slotNames(SMS.stocks[[1]])
slotNames(ple4)

plot(ple4)
plot(SMS.stocks[[1]])

seasonSums(SMS.stocks[[1]]@catch)
SMS.stocks[[1]]@catch.n
SMS.stocks[[1]]@stock.n
SMS.stocks[[1]]@stock.wt
SMS.stocks[[1]]@mat

## what methods are available
showMethods(class="FLSR")
################################################################################


#create an FLSR object
ple4.sr<-as.FLSR(ple4)
summary(ple4.sr)


ms.sr<-as.FLSR(SMS.stocks[[1]])

ms.sr<-lapply(SMS.stocks,as.FLSR)
summary(ms.sr)
summary(ms.sr[[1]])

# The rec and ssb slots have now been filled
ssb(ms.sr[[1]])
rec(ms.sr[[1]])
# ssb does not work !

# do it without quarters

SMS.stocks<-lapply(SMS.stocks,function(x){ window(x, start = 1974, end = 2010)} )


sp<-2
SMS.stocks[[sp]]@range

stockDims <- FLQuant(dimnames=list(age = SMS.stocks[[sp]]@range["min"]:SMS.stocks[[sp]]@range["min"],
                                   year = SMS.stocks[[sp]]@range["minyear"]:SMS.stocks[[sp]]@range["maxyear"], unit = 'thousands',
             season = 'all', area = 'unique'))
sr <- FLSR(name =sp.names[sp], desc = "test",
            rec = stockDims)

summary(sr)
sr@rec<-SMS.stocks[[sp]]@stock.n[1,,,3,,]  #third quarter data age 0
sr@ssb<-apply(SMS.stocks[[sp]]@stock.n[,,,1,,]*SMS.stocks[[sp]]@stock.wt[,,,1,,]*SMS.stocks[[sp]]@mat[,,,1,,],2,sum)
units(sr@rec)<-  "thousands"
units(sr@ssb)<-  "tonnes"

model(sr)<-ricker()
model(sr)
summary(sr)
summary(ple4.sr)  # compare

sr<-fmle(sr)
plot(sr)


# det virker ikke med kvartals data
library(FLBRP)

## ## estimate reference points. Create the corresponding FLBRP object
# plaice example
pleBrp<-FLBRP(ple4)
summary(pleBrp)

harvest(pleBrp)

## check that it worked!
sweep(harvest(pleBrp),1,catch.sel(pleBrp),"/")
apply(harvest(pleBrp)[range(pleBrp)[["minfbar"]]:range(pleBrp)[["maxfbar"]],],2,mean)

## other estimated quantities-at-age
stock.n(pleBrp)
landings.n(pleBrp)
discards.n(pleBrp)

yield(pleBrp)
rec(pleBrp)
ssb(pleBrp)
profit(pleBrp)
pleBrp<-brp(pleBrp)
## inspect reference points
refpts(pleBrp)


# SMS
Brp<-FLBRP(SMS.stocks[[sp]])
summary(Brp)

Brp<-brp(Brp)
refpts(Brp)

## historic observations
fbar.obs(    Brp)
yield.obs(   Brp)
landings.obs(Brp)
discards.obs(Brp)
rec.obs(     Brp)
ssb.obs(     Brp)
profit.obs(  Brp)