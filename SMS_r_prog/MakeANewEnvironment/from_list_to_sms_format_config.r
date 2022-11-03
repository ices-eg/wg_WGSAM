source(file.path(prog.path,'From_list_to_SMS_format.R'))


## Western Baltic with whiting
if (FALSE) {

  code.name<-c('OTH','WHG','COD','HER','SPR')
  code.name.pred<- c("WHG","COD")
  code.name.prey<- c("OTH",'COD','HER','SPR')
  min.stomach.sampled.in.stratum<-10
  
  #you have to use selected.years or year.q as stomach selection criterion
  
  selected.years<-NULL            # use this if year.q ( defined below) is used
  selected.years<-c(1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,2011,2012)      # all
  
  
  year.q<-c(
  "1977q1", "1977q2", "1977q3", "1977q4",
  "1978q1", "1978q2", "1978q3", "1978q4",
  "1979q1", "1979q2", "1979q3", "1979q4",
  "1980q1", "1980q2", "1980q3", "1980q4",
  "1981q1", "1981q2", "1981q3", "1981q4",
  "1982q1", "1982q2", "1982q3", "1982q4",
  "1983q1", "1983q2", "1983q3", "1983q4",
  "1984q1", "1984q2", "1984q3", "1984q4",
  "1985q1", "1985q2", "1985q3", "1985q4",
  "1986q1", "1986q2", "1986q3", "1986q4",
  "1987q1", "1987q2", "1987q3", "1987q4",
  "1988q1", "1988q2", "1988q3", "1988q4",
  "1989q1", "1989q2", "1989q3", "1989q4",
  "1990q1", "1990q2", "1990q3", "1990q4",
  "1991q1", "1991q2", "1991q3", "1991q4",
  "1992q1", "1992q2", "1992q3", "1992q4",
  "1993q1", "1993q2", "1993q3", "1993q4",
  "1994q1", "1994q2", "1994q3", "1994q4")
  
  # you have to set one of them to NULL
  year.q<-NULL
  #selected.years<-NULL
  
  
  # used for scaling of N hauls
  var.groups.size<- c( 160, 210, 260, 310, 360, 410, 460, 510, 560, 610, 660, 710)
  var.groups<-      c(    1,   1,   1,   2,   2,  2,   3,   3,   4,   4,   4,   4)
  min.stom.groups<-5
  
  # used for scaling of N hauls, not used for WB yet
  var.groups.size<-NULL 
  var.groups<-     NULL
  
  if (T) {   ## no tails
    SMS.data.transform(list.data.path=file.path(root,"data_baltic_WB","data-2014-one-area"),
          trans.bio=F, trans.catch=F,
          trans.meanL=F,  trans.meanL.from.weight=F,  trans.stockDist=F,
          trans.stomach=T, trans.ALK.stomach=T, use.stom.meanl.ALK=FALSE, trans.other=F, trans.Consum=F,
          stom.first=1E-08, stom.mid=1E-08, stom.last=1E-08, stom.min.abs=1E-08,delete.tails=T, 
          inserted.haul.no.propor=1.0, 
          formatted.output=T,selected.years=selected.years,year.q=year.q)
  }
  if (F) {   ## tails
    SMS.data.transform(list.data.path=file.path(root,"data_baltic","2013-data"),
          trans.bio=F, trans.catch=F,
          trans.meanL=F,  trans.meanL.from.weight=F,  trans.stockDist=F,
          trans.stomach=T, trans.ALK.stomach=T, use.stom.meanl.ALK=FALSE, trans.other=F, trans.Consum=T,
          stom.first=1E-06, stom.mid=1E-05, stom.last=1E-06, stom.min.abs=1E-04, delete.tails=F,
          inserted.haul.no.propor=1.0, 
          formatted.output=T,selected.years=selected.years,year.q=year.q)
  }
## end  Western Baltic
}
###############################
## Western Baltic (without whiting)
if (FALSE) {
  
  code.name<-c('OTH','COD','HER','SPR')
  code.name.pred<- c("COD")
  code.name.prey<- c("OTH",'COD','HER','SPR')
  min.stomach.sampled.in.stratum<-10
  
  #you have to use selected.years or year.q as stomach selection criterion
  
  selected.years<-NULL            # use this if year.q ( defined below) is used
  selected.years<-c(1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993)      # all
  
  
  year.q<-c(
  "1977q1", "1977q2", "1977q3", "1977q4",
  "1978q1", "1978q2", "1978q3", "1978q4",
  "1979q1", "1979q2", "1979q3", "1979q4",
  "1980q1", "1980q2", "1980q3", "1980q4",
  "1981q1", "1981q2", "1981q3", "1981q4",
  "1982q1", "1982q2", "1982q3", "1982q4",
  "1983q1", "1983q2", "1983q3", "1983q4",
  "1984q1", "1984q2", "1984q3", "1984q4",
  "1985q1", "1985q2", "1985q3", "1985q4",
  "1986q1", "1986q2", "1986q3", "1986q4",
  "1987q1", "1987q2", "1987q3", "1987q4",
  "1988q1", "1988q2", "1988q3", "1988q4",
  "1989q1", "1989q2", "1989q3", "1989q4",
  "1990q1", "1990q2", "1990q3", "1990q4",
  "1991q1", "1991q2", "1991q3", "1991q4",
  "1992q1", "1992q2", "1992q3", "1992q4",
  "1993q1", "1993q2", "1993q3", "1993q4",
  "1994q1", "1994q2", "1994q3", "1994q4")
  
  # you have to set one of them to NULL
  year.q<-NULL
  #selected.years<-NULL
  
  
  # used for scaling of N hauls
  var.groups.size<- c( 160, 210, 260, 310, 360, 410, 460, 510, 560, 610, 660, 710)
  var.groups<-      c(    1,   1,   1,   2,   2,  2,   3,   3,   4,   4,   4,   4)
  min.stom.groups<-5
  
  # used for scaling of N hauls, not used for WB yet
  var.groups.size<-NULL 
  var.groups<-     NULL
  
  if (TRUE) {   ## no tails
    SMS.data.transform(list.data.path=file.path(root,"data_baltic_WB","data-2014-one-area"),
          trans.bio=T, trans.catch=T,
          trans.meanL=F,  trans.meanL.from.weight=T,  trans.stockDist=F,
          trans.stomach=T, trans.ALK.stomach=T, use.stom.meanl.ALK=FALSE, trans.other=F, trans.Consum=F,
          stom.first=1E-08, stom.mid=1E-08, stom.last=1E-08, stom.min.abs=1E-08,delete.tails=T, 
          inserted.haul.no.propor=1.0, 
          formatted.output=T,selected.years=selected.years,year.q=year.q)
  }
  if (FALSE) {   ## tails
    SMS.data.transform(list.data.path=file.path(root,"data_baltic","2013-data"),
          trans.bio=F, trans.catch=F,
          trans.meanL=F,  trans.meanL.from.weight=F,  trans.stockDist=F,
          trans.stomach=T, trans.ALK.stomach=T, use.stom.meanl.ALK=FALSE, trans.other=F, trans.Consum=T,
          stom.first=1E-06, stom.mid=1E-05, stom.last=1E-06, stom.min.abs=1E-04, delete.tails=F,
          inserted.haul.no.propor=1.0, 
          formatted.output=T,selected.years=selected.years,year.q=year.q)
  }
## end  Western Baltic
}


########################################
 

## Baltic paper
if (FALSE) {

  code.name<-c('OTH','COD','HER','SPR')
  code.name.pred<- c("COD")
  code.name.prey<- c("OTH",'COD','HER','SPR')
  min.stomach.sampled.in.stratum<-10
  
  #you have to use selected.years or year.q as stomach selection criterion
  #
  
  selected.years<-c(1977,1978,1979,1980,1981)
  selected.years<-c(1981,1982,1984,1987,1990)  #Stephan
  selected.years<-c(1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994)
  #
  selected.years<-NULL            # use this if year.c ( defined below) is used
  selected.years<-c(1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994)
  
  
  year.q<-c(
  "1977q1", "1977q2", "1977q3", "1977q4",
  "1978q1", "1978q2", "1978q3", "1978q4",
  "1979q1", "1979q2", "1979q3", "1979q4",
  "1980q1", "1980q2", "1980q3", "1980q4",
  "1981q1", "1981q2", "1981q3", "1981q4",
  "1982q1", "1982q2", "1982q3", "1982q4",
  "1983q1", "1983q2", "1983q3", "1983q4",
  "1984q1", "1984q2", "1984q3", "1984q4",
  "1985q1", "1985q2", "1985q3", "1985q4",
  "1986q1", "1986q2", "1986q3", "1986q4",
  "1987q1", "1987q2", "1987q3", "1987q4",
  "1988q1", "1988q2", "1988q3", "1988q4",
  "1989q1", "1989q2", "1989q3", "1989q4",
  "1990q1", "1990q2", "1990q3", "1990q4",
  "1991q1", "1991q2", "1991q3", "1991q4",
  "1992q1", "1992q2", "1992q3", "1992q4",
  "1993q1", "1993q2", "1993q3", "1993q4",
  "1994q1", "1994q2", "1994q3", "1994q4")
  
  # better quality stomachs!
  year.q<-c(
            "1977q2",
  "1978q1",                     "1978q4",
            "1979q2", "1979q3", "1979q4",
  
            "1981q2", "1981q3", "1981q4",
  "1982q1", "1982q2", "1982q3", "1982q4",
  "1983q1", "1983q2",           "1983q4",
  "1984q1", "1984q2",           "1984q4",
  "1985q1",
  "1986q1",
  "1987q1", "1987q2",           "1987q4",
  "1988q1", "1988q2",           "1988q4",
  "1989q1", "1989q2", "1989q3", "1989q4",
            "1990q2",           "1990q4",
  "1991q1",
  "1992q1",           "1992q3",
  "1993q1",                     "1993q4")
  
  
  # you have to set one of them to NULL
  #year.q<-NULL
  selected.years<-NULL
  
  # used for scaling of N hauls
  var.groups.size<- c( 160, 210, 260, 310, 360, 410, 460, 510, 560, 610, 660, 710)
  var.groups<-      c(    1,   1,   1,   2,   2,  2,   3,   3,   4,   4,   4,   4)
  min.stom.groups<-5
  
  if (TRUE) {   ## no tails
    SMS.data.transform(list.data.path=file.path(root,"data_baltic","2013-data"),
          trans.bio=T, trans.catch=T,
          trans.meanL=T,  trans.meanL.from.weight=F,  trans.stockDist=F,
          trans.stomach=T, trans.ALK.stomach=T, use.stom.meanl.ALK=FALSE, trans.other=F, trans.Consum=T,
          stom.first=1E-08, stom.mid=1E-08, stom.last=1E-08, stom.min.abs=1E-08,delete.tails=T, 
          inserted.haul.no.propor=1.0, 
          formatted.output=T,selected.years=selected.years,year.q=year.q)
  }
  if (FALSE) {   ## tails
    SMS.data.transform(list.data.path=file.path(root,"data_baltic","2013-data"),
          trans.bio=F, trans.catch=F,
          trans.meanL=F,  trans.meanL.from.weight=F,  trans.stockDist=F,
          trans.stomach=T, trans.ALK.stomach=T, use.stom.meanl.ALK=FALSE, trans.other=F, trans.Consum=T,
          stom.first=1E-06, stom.mid=1E-05, stom.last=1E-06, stom.min.abs=1E-04, delete.tails=F,
          inserted.haul.no.propor=1.0, 
          formatted.output=T,selected.years=selected.years,year.q=year.q)
  }
## end Baltic
}



########################################################################################
## North Sea 4 predators, 3 preys
if (FALSE) {
    min.stomach.sampled.in.stratum<-5
    code.name<-       c("OTH",'COD','WHG','HAD','POK','HER','SAN','NOP')
    code.name.pred<-        c('COD','WHG','HAD','POK')
    code.name.prey<-  c("OTH",'COD','WHG','HAD','HER','SAN','NOP')
    selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991)
    selected.years<-c(1991)

    year.q<-NULL
    SMS.data.transform( list.data.path=file.path(root,"data_northSea"),
        trans.bio=T, trans.catch=T,
        trans.meanL=T,  trans.meanL.from.weight=FALSE,
        trans.stomach=F, trans.ALK.stomach=F, trans.other=T, trans.Consum=T,
        stom.first=1E-04, stom.mid=1E-04, stom.last=1E-04, stom.min.abs=1E-04,
        delete.tails=TRUE,
        inserted.haul.no.propor=1.0, 
        formatted.output=TRUE,selected.years=selected.years,year.q=year.q)
}


########################################################################################
## North Sea 4 predators, 4 preys  (incl sprats)+ western and North Sea Mackerel
if (FALSE) {
    min.stomach.sampled.in.stratum<-5
    code.name<-       c('OTH','W_M','N_M','COD','WHG','HAD','POK','HER','SAN','NOP','SPR')
    code.name.pred<-        c('W_M','N_M','COD','WHG','HAD','POK','SPR')
    code.name.prey<- c('OTH','COD','WHG','HAD','HER','SAN','NOP','SPR')
    selected.years<-c(1981,1985,1986,1987,1991)  #stomachs

    year.q<-NULL
     #

    SMS.data.transform(list.data.path=file.path(root,"data_northSea"),
        trans.bio=F, trans.catch=F,
        trans.meanL=T,  trans.meanL.from.weight=FALSE,
        trans.stomach=T, trans.ALK.stomach=T, trans.other=T, trans.Consum=FALSE,
        stom.first=1E-08, stom.mid=1E-08, stom.last=1E-08, stom.min.abs=1E-08,
        delete.tails=T,
        inserted.haul.no.propor=1.0, 
        formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}
########################################################################################


########################################################################################
## North Sea 4 predators, 3 preys + western and North Sea Mackerel
if (FALSE) {
    min.stomach.sampled.in.stratum<-5
    code.name<-       c('OTH','W_M','N_M','COD','WHG','HAD','POK','HER','SAN','NOP')
    code.name.pred<-        c('W_M','N_M','COD','WHG','HAD','POK')
    code.name.prey<- c('OTH','COD','WHG','HAD','HER','SAN','NOP')
    selected.years<-c(1981,1985,1986,1987,1991)  #stomachs

    year.q<-NULL
     #

    SMS.data.transform(list.data.path=file.path(root,"data_northSea"),
        trans.bio=FALSE, trans.catch=FALSE,
        trans.meanL=T,  trans.meanL.from.weight=F,
        trans.stomach=T, use.stom.meanl.ALK=T,
        trans.ALK.stomach=T, trans.ALK.all=F, trans.other=FALSE, trans.Consum=FALSE,
        stom.first=1E-05, stom.mid=1E-05, stom.last=1E-05, stom.exp=1E-06, stom.min.abs=1E-06,
        delete.tails=F,
        inserted.haul.no.propor=1.0, 
        formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}
########################################################################################


########################################################################################
## North Sea all other pred, SPR and Grey Seals and Harbour porpoise are included.    2011 WGSAM
if (FALSE) {
    min.stomach.sampled.in.stratum<-5
    code.name<-       c("OTH","FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'COD','WHG','HAD','POK','HER','SAN','NOP','SPR','PLE','SOL')
    code.name.pred<-        c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'COD','WHG','HAD','POK')
    code.name.prey<- c("OTH",'COD','WHG','HAD','HER','SAN','SPR','NOP')
    selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1995,2000,2002,2005)

    year.q<-NULL
     #

    SMS.data.transform(list.data.path=file.path(root,"data_northSea"),
        trans.bio=T, trans.catch=T,
        trans.meanL=T,  trans.meanL.from.weight=FALSE,
        trans.stomach=T, trans.ALK.stomach=T, trans.other=T, trans.Consum=T, trans.ALK.all=T,
        stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
        inserted.haul.no.propor=1.0, 
        formatted.output=TRUE,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}
########################################################################################


########################################################################################
## North Sea all other pred, SPR and Grey Seals and Harbour porpoise are included.    2011 WGSAM  but with tails
if (FALSE) {
    min.stomach.sampled.in.stratum<-1
    code.name<-       c("OTH","FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'COD','WHG','HAD','POK','HER','SAN','NOP','SPR','PLE','SOL')
    code.name.pred<-        c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'COD','WHG','HAD','POK')
    code.name.prey<- c("OTH",'COD','WHG','HAD','HER','SAN','SPR','NOP')
    selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1995,2000,2002,2005)

    year.q<-NULL
     #

     
# used for scaling of N hauls, not used for NS
var.groups.size<-NULL 
var.groups<-     NULL

min.stom.groups<-5


    SMS.data.transform(list.data.path=file.path(root,"data_northSea","SMS-input-2013"),
        trans.bio=F, trans.catch=F,
        trans.meanL=F,  trans.meanL.from.weight=FALSE,
        trans.stomach=T, trans.ALK.stomach=F, trans.other=F, trans.Consum=F, trans.ALK.all=F,
        stom.first=1E-05, stom.mid=1E-04, stom.last=1E-05, stom.min.abs=1E-05, delete.tails=F,
        inserted.haul.no.propor=1.0, 
        formatted.output=TRUE,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}


########################################################################################
# Sprat
 if (FALSE) {
    min.stomach.sampled.in.stratum<-5
    code.name<-       c("OTH","SPR")
    code.name.pred<-        c()
    code.name.prey<- c()
    selected.years<-c(2010)

    year.q<-NULL
     #

    SMS.data.transform(list.data.path=file.path(root,"data_northSea"),
        trans.bio=TRUE, trans.catch=T,
        trans.meanL=F,  trans.meanL.from.weight=FALSE,
        trans.stomach=F, trans.ALK.stomach=F, trans.other=F, trans.Consum=F,
        stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
        inserted.haul.no.propor=1.0, 
        formatted.output=TRUE,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}
 ########################################################################################
 ## North Sea all other pred, TWO sandeel stocks 
if (FALSE) {
    min.stomach.sampled.in.stratum<-5
    code.name<-       c("OTH","FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'COD','WHG','HAD','POK','HER','NSA','SSA','NOP','SPR','PLE','SOL')
    code.name.pred<-        c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'COD','WHG','HAD','POK')
    code.name.prey<- c("OTH",'COD','WHG','HAD','HER','NSA','SSA','SPR','NOP')
    selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1995,2000,2002,2005)

    year.q<-NULL
    var.groups.size<-NULL
    var.groups<-     NULL
    min.stom.groups<-5
     #

    SMS.data.transform(list.data.path=file.path(root,"data_northSea","SMS-input-2014"),
        trans.bio=T, trans.catch=T,
        trans.meanL=T,  trans.meanL.from.weight=FALSE,
        trans.stomach=T, trans.ALK.stomach=T, trans.other=T, trans.Consum=T, trans.ALK.all=T,
        stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
        inserted.haul.no.propor=1.0, 
        formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}
########################################################################################
 ## North Sea all other pred, TWO sandeel stocks and Hake 2014 WGSAM
if (FALSE) {
    min.stomach.sampled.in.stratum<-5
    code.name<-       c("OTH","FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP","HKE",'COD','WHG','HAD','POK','HER','NSA','SSA','NOP','SPR','PLE','SOL')
    code.name.pred<-        c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP","HKE",'COD','WHG','HAD','POK')
    code.name.prey<- c("OTH",'COD','WHG','HAD','HER','NSA','SSA','SPR','NOP')
    selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1995,2000,2002,2005,2013)

    year.q<-NULL
    var.groups.size<-NULL
    var.groups<-     NULL
    min.stom.groups<-5
     #

    SMS.data.transform(list.data.path=file.path(root,"data_northSea","SMS-input-2014"),
        trans.bio=T, trans.catch=T,
        trans.meanL=T,  trans.meanL.from.weight=FALSE,
        trans.stomach=F, trans.ALK.stomach=F, trans.other=T, trans.Consum=F, trans.ALK.all=F,
        stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
        inserted.haul.no.propor=1.0, 
        formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}
########################################################################################

## North Sea all other pred (one Mackerel), Two sandeel stocks and hake 2017 WGSAM
if (TRUE) {
  min.stomach.sampled.in.stratum<-5
  code.name<-       c("OTH","FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP","HKE",'COD','WHG','HAD','POK','MAC','HER','NSA','SSA','NOP','SPR','PLE','SOL')
  code.name.pred<-        c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP","HKE",'COD','WHG','HAD','POK','MAC')
  code.name.prey<- c("OTH",'COD','WHG','HAD','HER','NSA','SSA','SPR','NOP')
  selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1995,2000,2002,2005,2013)
  
  year.q<-NULL
  var.groups.size<-NULL
  var.groups<-     NULL
  min.stom.groups<-5
  #
  
  SMS.data.transform(list.data.path=file.path(root,"data_northSea","SMS-input-2017"),
                     trans.bio=T, trans.catch=T,
                     trans.meanL=T,  trans.meanL.from.weight=FALSE,
                     trans.stomach=F, trans.ALK.stomach=F, trans.other=T, trans.Consum=F, trans.ALK.all=F,
                     stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
                     inserted.haul.no.propor=1.0, 
                     formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)
}
########################################################################################
