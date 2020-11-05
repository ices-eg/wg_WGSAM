
setwd(data.path)
last.y<-2012        # last year in simulation
years.in.assess<-20

# please note: you should allready have made a SMS run to produce input for forcast!


#read options from SMS.dat file
SMS.dat<-read.FLSMS.control(file='SMS.dat')
first.y.assess<-SMS.dat@first.year
first.y<-SMS.dat@last.year.model+1  #First year of prediction
recq<-SMS.dat@rec.season            # recruitment season
years.in.assess<-min(years.in.assess,first.y-first.y.assess)

all.TACs<-NULL
all.SMS.yields<-NULL

# read output from SMS (parameter estimation mode) into a FLStocks object
SMS.stocks<-SMS2FLStocks(sumfile='summary.out',bio.interact=TRUE,
                         read.input=TRUE, read.output=TRUE,control=SMS.dat)
SMS.nsp<-length(SMS.stocks)

#assessAllStocks(SMS.stocks)

# read prediction options
HCR<-read.FLSMS.predict.control(control=SMS.dat,file='HCR_options_source.dat')

for (y in (first.y:last.y)) {

    # use the stock object for assessment to estimate TACs
    assess.stocks<-SMS.stocks
    first.y.assess<-y-years.in.assess
    for (i in (1:SMS.nsp)) {
       assess.stocks[[i]]<-window(SMS.stocks[[i]],first.y.assess,y-1)
       assess.stocks[[i]]@m<- assess.stocks[[i]]@m1+ assess.stocks[[i]]@m2
    }    

    # make an assessment using SMS.stocks as input
    TACs<- assessAllStocks(assess.stocks)  # from Bruce

    #Prepare for one year step of operating model=SMS    
    in.TACs<-TACs
    all.TACs<-rbind(all.TACs,TACs)
    in.TACs[is.na(TACs)]<--1
    HCR@constant.TAC<-in.TACs     # constant.TAC (really not used)

    HCR@TAC.first<-in.TACs      # TAC in the (first) prediction year
    #if (y>first.y) HCR@TAC.first<-c(1,-1,-1)   #TEST
 
    in.TACs[is.na(TACs)]<-1
    in.TACs[!is.na(TACs)]<--1
    HCR@F.first<-in.TACs      # F in the (first) prediction year
    HCR@constant.F<--in.TACs  # F in the next years

    in.HCRs<-in.TACs
    in.HCRs[in.TACs==0]<-1       # Use fixed F HCR for species without TAC
    in.HCRs[in.HCRs!=1]<-2       #   and used fixed TAC for the rest
    
    HCR@last.prediction.year<-y
     
    if (y==first.y) {
      HCR@read.F.first.y<-0
      HCR@read.F.last.y<-0
      HCR@read.stock.N.first.y<-0
      HCR@read.stock.N.last.y<-0
    } else {
      HCR@read.F.first.y<-first.y
      HCR@read.F.last.y<-y-1
      HCR@read.stock.N.first.y<-first.y
      HCR@read.stock.N.last.y<-y-1
    }

    write.FLSMS.control(HCR,file='HCR_options.dat')

    # execute sms.exe
    command<-'"..\\program\\sms" -mceval'
    system(command, show.output.on.console =TRUE)
    
    #read results of prediction into FLStocks object
    SMS.stocks<-SMS2FLStocks(sumfile="FLR_CNF.out",control=SMS.dat) 
 
    # just for checking, extract yields
    SMS.yield<-rep(NA,SMS.nsp)
    for (s in (1:SMS.nsp)) {
      SMS.yield[s]<-sum(SMS.stocks[[s]]@catch[,as.character(y),,,])    
    }
    all.SMS.yields<-rbind(all.SMS.yields,SMS.yield)
     
print(round(all.TACs))
print(round(all.SMS.yields))
  
 # write sms input files for recruitment and F
 if (y<last.y) {
    # write predict_stock_N.in
    n.file<-"predict_stock_N.in"
    cat(paste("# Stock number 1. Jan (or at recruitment time for recruits) at age",
            "\n# Negative value are not used",sep=""),file=n.file)

    #write exploitation_pattern.in
    f.file<-"exploitation_pattern.in"
    cat(paste("# Exploitation pattern or F at age\n",sep=""),file=f.file)

    for (s in SMS.stocks) {
        N<-aperm(s@stock.n[,as.character(first.y:y),,,],c(2,1,4,5,3))
        FF<-aperm(s@harvest[,as.character(first.y:y),,,],c(2,4,1,5,3))
        for (yy in (first.y:y)) {
            # stock numbers
            cat(paste("\n# ",s@name,yy,"\n"),file=n.file,append=TRUE)
            cy<-as.character(yy)
            N[cy,1,1,1,1]<-N[cy,1,recq,1,1] # copy recruit from recruitment season to first quarter
            N[cy,-1,1,1,1]<--9  # set all age (except the recruits) N to -9
            cat(file=n.file,N[cy,,1,1,1],append=TRUE)
            # Fishing mortalities
            cat(paste("\n# ",s@name,yy,"\n"),file=f.file,append=TRUE)
            write.table(FF[cy,,,1,1],quote=FALSE,row.names=FALSE,col.names=FALSE, file=f.file,append=TRUE)
        }
    }
   }

   #print(SMS.stocks[[1]]@stock.n[,as.character(y),1,3,])
}

check<-matrix(NA,ncol=SMS.nsp,nrow=last.y-first.y+1)
for (s in (1:SMS.nsp)) {
   for (y in (first.y:last.y)) {
      check[y-first.y+1,s]<-sum(SMS.stocks[[s]]@catch[,as.character(y),,,])    
    }
}
cat("\nCheck: output from last SMS run\n")
print(round(check))
source(file.path(prog.path,"HCR_output_multi.R"))
