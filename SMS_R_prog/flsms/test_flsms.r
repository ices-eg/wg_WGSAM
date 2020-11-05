
##  test   #########################

if (do.test) {
    HCR<-new("FLSMS.predict.control") 
    HCR<-FLSMS.predict.control(
        first.prediction.year=2014, 
        last.prediction.year=2015,
        no.species=28,
        no.other.predators=19,
        species.names=sp.names
    ) 
   print(HCR)

    HCR<-FLSMS.predict.control(
        first.prediction.year=2012, 
        last.prediction.year=2020,
        no.species=1,
        species.names=sp.names
    ) 
   print(HCR)
  
   SMS.dat<-read.FLSMS.control(file='SMS.dat')
   #write.FLSMS.control(SMS.dat,file="tSMS.dat",write.multi=F,nice=T,writeSpNames=F)
   write.FLSMS.control(SMS.dat,file="t3SMS.dat",write.multi=T,nice=T,writeSpNames=F,expand=T)
   
   HCR<-read.FLSMS.predict.control(control=SMS.dat,file='HCR_options.dat')
   print(HCR)
   write.FLSMS.predict.control(HCR,SMS=SMS.dat,file='a.dat')
}  


##  test   #########################
   

if (do.test) {  

    #SMS<-new("FLSMS")
       SMS.dat<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))

    SMS.stocks<-SMS2FLStocks(sumfile=file.path(data.path,'summary.out'),
                    bio.interact=F, read.input=TRUE, read.output=TRUE,control=SMS.dat)
    
    summary(SMS.stocks[[1]])  
    summary(SMS.stocks)
    seasonSums(SMS.stocks[[1]]@catch)

    # update an existing FLStocksMulti
    SMS.pred.stocks<-SMS2FLStocks(sumfile=file.path(data.path,'FLR_CNF.out'),FLStocksMulti=SMS.stocks,
                    bio.interact=TRUE, read.input=TRUE, read.output=TRUE,control=SMS.dat)
    
    summary(SMS.pred.stocks)
}



##  test   #########################

if (do.test) {
    SMS<-new("FLSMS.control")    
    SMS.dat<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))
    write.FLSMS.control(SMS.dat,file=file.path(data.path,"tSMS.dat"),write.multi=T,nice=T)
  
    write.FLSMS.control(SMS.dat,file=file.path(data.path,"tSMS.dat"),write.multi=T,nice=T)
  
    write.FLSMS.control(SMS.dat,file=file.path(data.path,"tSMS.dat"),write.multi=T,nice=F)

    print(validFLSMS.control(SMS.dat))
    a<-FLSMS.control(first.year=1976,last.year=2000,no.species=3,
           no.other.predators=1,no.VPA.predators=1,species.names=c('Bird','Cod','Herring'))
    write.FLSMS.control(a,file=file.path(data.path,'tSMS.dat'))
 }  

if (do.test) {  
    SMS.dat<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))

    SMS.indices<-SMS2FLIndices(SMS.dat)
    
    out.path<-file.path(root,"junk")

    FLIndices2SMS(out.path=out.path,indices=SMS.indices,control=SMS.dat)

    print(SMS.indices[[1]]@range.SMS)
    
      # creates empty FLIndices object
    a <- FLIndices()       
    a[[1]]<-SMS.indices[[1]]
     FLIndices2SMS(out.path=out.path,indices=a,control=SMS.dat)


}
 if (do.test) {  
       a<-FLStockMulti()
       
       SMS.dat<-read.FLSMS.control(file=file.path(data.path,'SMS.dat'))

    SMS.stocks<-SMS2FLStocks(sumfile=file.path(data.path,'summary.out'),
                    bio.interact=F, read.input=TRUE, read.output=TRUE,control=SMS.dat)                   
    summary(SMS.stocks)
  
    SMS.stocks<-SMS2FLStocks(sumfile=file.path(data.path,'summary.out'),
                    bio.interact=F, read.input=TRUE,read.output=FALSE, control=SMS.dat)                   
    summary(SMS.stocks)
 
      
    is.FLQuant(SMS.stocks[[1]]@catch.n)
    
    FLStocks2SMS(FLStock=SMS.stocks,control=SMS.dat,path=file.path(data.path,'FLtest'), bio.interact=FALSE)
 }