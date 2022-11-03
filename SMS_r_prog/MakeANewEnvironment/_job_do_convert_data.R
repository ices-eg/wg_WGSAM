# this script make 
#  1. write a number of of files on a spredsheet like format with data extracted from the current environment
# 2. convert files produced under 1) to files with data on SMS format

# between 1) and 2) you are supposed to update data !

# start by submitting init.R, so file paths are establibsed



exchangeDir<-"SMS-input-key-run-2017"   # directory with data  files on spread sheet format
#exchangeDir<-"macExch"   # directory with data  files on spread sheet format
RexchangeDir<-"MakeANewEnvironment"     # directory with R scripts to convert files etc.


options(stringsAsFactors = FALSE)

#  first 1.


if (FALSE) {  # write a number of of files on a spredsheet like format with data extracted from the current environment
  source(file.path(prog.path,RexchangeDir,'from_sms_format_to_list.r'))
 
  my.code.name<-c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_M","N_M","W_H","N_H","GSE","HBP",'HAK','COD','WHG','HAD','POK','HER','NSA','SSA','NOP','SPR','PLE','SOL')
  #my.code.name<-c("MAC")
  
  From_SMS_format_to_list(otherPredExist=T,catchMultiplier=1,code.name=my.code.name,exchangeDir=file.path(root,exchangeDir),addfn='key2013')
}

# Update mackerel catches. Use old 4M data for the period 1974-1979 and ICES SAM data from 1980 onwards
source(file.path(prog.path,RexchangeDir,'mackerel.r'))  # makes SAM (catch) and bio data for Mackerel 
###

#####################################################################################
# make basis input catach file for 2017 keyrun

# catches
CAT.01<-read.csv(file=file.path(root,exchangeDir,'vpa_ca01_key2013.csv'))
CAT.01$cat<-NULL
head(CAT.01)
MAC$PROP_CAT<-1
CAT.01<-subset(CAT.01,species != 'MAC')
CAT.01<-rbind(CAT.01,MAC)
save(CAT.01,file=file.path(root,exchangeDir,'CAT_01.Rdata'))

# Biological data (Wsea,propmat, M and M1)
BIO.01<-read.csv(file=file.path(root,exchangeDir,'vpa_bi01_key2013.csv'))
BIO.01<-subset(BIO.01,species != 'MAC')
BIO.01<-rbind(BIO.01,bio)
BIO.01$PROP_N<-1  
save(BIO.01,file=file.path(root,exchangeDir,'BIO_01.Rdata'))

# Other predators
BIO.02<-read.csv(file=file.path(root,exchangeDir,'other_sp_key2013.csv'))
BIO.02<-subset(BIO.02,!(species %in% c('W_M','N_M')))
save(BIO.02,file=file.path(root,exchangeDir,'BIO_02.Rdata'))




writeLists<-function(years=1974:2013) {
  # BIO 01
  load(file.path(root,exchangeDir,'BIO_01.Rdata'),verbose=T) 
  BIO.01<-subset(BIO.01,year %in% years)
  write.table(BIO.01,file=file.path(root,exchangeDir,paste0('VPA_Bi01.IN')),row.names = F,quote = T,sep=',')

  # BIO 02
  load(file.path(root,exchangeDir,'BIO_02.Rdata'),verbose=T) 
  BIO.02<-subset(BIO.02,year %in% years)
  write.table(BIO.02,file=file.path(root,exchangeDir,paste0('VPA_Bi02.IN')),row.names = F,quote = T,sep=',')
  
  
  # CAT 01
  load(file.path(root,exchangeDir,'CAT_01.Rdata'),verbose=T) 
  CAT.01<-subset(CAT.01,year %in% years)
  write.table(CAT.01,file=file.path(root,exchangeDir,paste0('VPA_Ca01.IN')),row.names = F,quote = T,sep=',')
}
writeLists()



source(file.path(prog.path,RexchangeDir,'From_list_to_SMS_format.R'))

## North Sea all other pred (one Mackerel), Two sandeel stocks and hake 2017 WGSAM
if (TRUE) {
  min.stomach.sampled.in.stratum<-5
  code.name<-       c("OTH","FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP","HAK",'COD','WHG','HAD','POK','MAC','HER','NSA','SSA','NOP','SPR','PLE','SOL')
  code.name.pred<-        c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP","HAK",'COD','WHG','HAD','POK','MAC')
  code.name.prey<- c("OTH",'COD','WHG','HAD','HER','NSA','SSA','SPR','NOP')
  selected.years<-c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1995,2000,2002,2005,2013)
  
  year.q<-NULL
  var.groups.size<-NULL
  var.groups<-     NULL
  min.stom.groups<-5
  #
  
  SMS.data.transform(list.data.path=file.path(root,"SMS-input-key-run-2017"),
                     trans.bio=T, trans.catch=T,
                     trans.meanL=F,  trans.meanL.from.weight=FALSE,
                     trans.stomach=F, trans.ALK.stomach=F, trans.other=T, trans.Consum=F, trans.ALK.all=F,
                     stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
                     inserted.haul.no.propor=1.0, 
                     formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)
  
}
########################################################################################
