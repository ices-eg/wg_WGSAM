# see https://github.com/ices-tools-prod/icesSAG

#icesSAG can be installed from CRAN using the install.packages command  
#  install.packages("icesSAG")

library(icesSAG)
# ?icesSAG

stocks <- getListStocks(2018)
subset(stocks,SpeciesName=='Scomber scombrus')
#write.csv(stocks,file=file.path(data.path,'a.csv'))

# read list of stocks to be processed
my.stocks<-read.csv(file=file.path(data.path,'ICEStockList.in'),stringsAsFactors=FALSE)
head(my.stocks)

y<-2017
b<-NULL
for (i in (1:dim(my.stocks)[[1]])) {
  stock<-my.stocks[i,'StockKeyLabel']
  cat(my.stocks[i,1],'\n')
  a<-getSAG(stock=stock, y)
  if (length(a)>0) {
    a<-subset(a,select=c(-high_recruitment,-low_recruitment,-StockPublishNote,-Fage,-fishstock,-units,-stockSizeDescription,-stockSizeUnits,-fishingPressureDescription ,-fishingPressureUnits))
    a$SMS<-my.stocks[i,'SMS']
    a$Species.n<-my.stocks[i,'nr']
    b<-rbind(a,b)
  }
}

unique(b$SMS)  
head(b)

#head(Read.summary.table())



#Species Species.n Year     Rec    SSB    TSB    SOP SOP.hat  Yield Yield.hat   mean.F Eaten
a<-data.frame(Species=sp.names[b$Species.n],Species.n=b$Species.n,Year=b$Year,SSB=b$SSB,mean.F=b$F,Rec=b$recruitment,Yield=ifelse(!is.na(b$catches),b$catches,b$landings))
a$SOP.hat<-a$Yield
a$SOP<-a$Yield

head(a)


write.table(a,row.names = FALSE, col.names = TRUE,file=file.path(root,'ICESsingle','summary_table_raw.out'))

