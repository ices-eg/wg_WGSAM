# this script makes 
#  1. write a number of of files on a spredsheet like format with data extracted from the current environment
#  2. read the ICES single species assessment input/output
#  3. update the old data set with new data
#  4. convert files produced under 1) to files with data on SMS format

# between 1) and 2) you are supposed to update data !

# start by submitting init.R, so file paths are establibsed

do.1<-FALSE
do.2<-TRUE
do.stock.dist.basic<-FALSE

newEnv<-'NS_2020'
exchangeDir<-file.path( "Data_NorthSea","input_NS_2020")   # directory with data  files on spread sheet format
RexchangeDir<-"MakeANewEnvironment"     # directory with R scripts to convert files etc.
finalExchangeDir<-file.path(root,"Data_NorthSea","final_input_NS_2020")

addfn<-'key2020'  # a label for files 
old_key_label<-'key_2017'

firstY<-1974    # first year in key run
lastY<-2019     # last year in (new) key run
newYear<-firstY:lastY 
lastYold<-2016  # last year in previous key run
OutputDir<-file.path(root,'Output')
makeGraphs<-TRUE

new.code.name<-c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP",'HAK','COD','WHG','HAD','POK','MAC','HER','NSA','SSA','NOP','SPR','PLE','SOL')



options(stringsAsFactors = FALSE)


#  first 1.

if (do.1) {  # write a number of of files on a spredsheet like format with data extracted from the current environment
  source(file.path(prog.path,RexchangeDir,newEnv,'from_sms_format_to_list.r'))
 
  my.code.name<-c("FUL","GLT","HEG","KTW","GBG","GNT","PUF","RAZ","RAJ","GUR","W_H","N_H","GSE","HBP",'HAK','COD','WHG','HAD','POK','MAC','HER','NSA','SSA','NOP','SPR','PLE','SOL')
  #my.code.name<-c("MAC")
  
  From_SMS_format_to_list(otherPredExist=T,catchMultiplier=1,code.name=my.code.name,exchangeDir=file.path(root,exchangeDir),addfn=old_key_label)
}



###   Change to directory to new key-run
#my.stock.dir<-'NorthSeaKeyRun_2020'
#data.path<-file.path(root,my.stock.dir)



#####################################################################################
# Then 2, read ICES assessment data and update SMS data
if (do.2) {
   source(file.path(prog.path,RexchangeDir,newEnv,"read_update_assessment_data_NS_2020.R"))
  
  #catch  round(tapply(CAT.01$CATCHN*CAT.01$WCATCH,list(CAT.01$year,CAT.01$species),sum))
  
}

######################################################################################




# stock distribution
if (do.stock.dist.basic) source(file.path(prog.path,RexchangeDir,newEnv,'stock_distribution_NS_2020sp.r')) # Stock ditribution by stock, produces maps and cod Rdata


# Other predators
source(file.path(prog.path,RexchangeDir,newEnv,'Horse_mack.R'))
source(file.path(prog.path,RexchangeDir,newEnv,'hake.R'))

source(file.path(prog.path,RexchangeDir,newEnv,'other_predators.R'))

################
# mean length
source(file.path(prog.path,RexchangeDir,newEnv,'mean_length.R'))

source(file.path(prog.path,RexchangeDir,newEnv,'stock_distribution_NS_2020.r')) # Stock distribution by stock


################
# consum
source(file.path(prog.path,RexchangeDir,newEnv,'consum.R'))

################
# stomach data
#not used in 2020  source(file.path(prog.path,RexchangeDir,newEnv,'stomachs.R'))


writeLists<-function(years=firstY:lastY) {
  # BIO 01
  load(file.path(root,exchangeDir,'BIO_01.Rdata'),verbose=T) 
  BIO.01<-subset(BIO.01,year %in% years,select=c(species,year,age,quarter,sub_area,WSEA,PROPMAT,M,M1,PROP_M2))
  write.table(BIO.01,file=file.path(finalExchangeDir,paste0('VPA_Bi01.IN')),row.names = F,quote = T,sep=',')
  
  # BIO 02
  load(file.path(root,exchangeDir,'BIO_02.Rdata'),verbose=T) 
  BIO.02<-subset(BIO.02,year %in% years)
  write.table(BIO.02,file=file.path(finalExchangeDir,paste0('VPA_Bi02.IN')),row.names = F,quote = T,sep=',')
  
  # CAT 01
  load(file.path(root,exchangeDir,'CAT_01.Rdata'),verbose=T) 
  round(tapply(CAT.01$CATCHN*CAT.01$WCATCH,list(CAT.01$year,CAT.01$species),sum))
  ftable(round(tapply(CAT.01$CATCHN*CAT.01$WCATCH,list(CAT.01$year,CAT.01$species,CAT.01$quarter),sum)))
  CAT.01<-subset(CAT.01,year %in% years)
  write.table(CAT.01,file=file.path(finalExchangeDir,paste0('VPA_Ca01.IN')),row.names = F,quote = T,sep=',')
}
writeLists()



# harbour porpoise og grey seals
#old data
mam<-read.table(file.path(root,"Data_NorthSea","Old_data","key-2017","stomcon_list_evac_Y.dat"),header=TRUE)
head(mam)
unique(mam$pred)
mam<-subset(mam,pred %in% c('GSE','HBP'))

#new 2020 data from SAS, but without mammals
stomData<-file.path(finalExchangeDir,"stomcon_list_evac_Y.dat")


old<-read.table(stomData,header=TRUE)

subset(old,pred %in% c('GSE','HBP'))
# it should be empty
old<-subset(old,!(pred %in% c('GSE','HBP'))) # but cleanup anyway
new<-rbind(old,mam)
write.table(new,file=stomData,col.names=TRUE,row.names=FALSE,sep=' ')


source(file.path(prog.path,RexchangeDir,newEnv,'From_list_to_SMS_format.R'))



## North Sea all other pred (one Mackerel), Two sandeel stocks and hake 2020 WGSAM
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
  stomMark<-c("_evac_Y","_FishStomach")[1]
  #
  rm(species)  # to make the next call work
  
  SMS.data.transform(list.data.path=file.path(finalExchangeDir),stomMark=stomMark,
                     trans.bio=T, trans.catch=T,
                     trans.meanL=T,  trans.meanL.from.weight=FALSE,  
                     trans.stomach=T, trans.ALK.stomach=T, trans.other=T, trans.Consum=T, trans.ALK.all=F,
                     stom.first=1E-06, stom.mid=1E-06, stom.last=1E-06, stom.min.abs=1E-06, delete.tails=TRUE,
                     inserted.haul.no.propor=1.0, 
                     formatted.output=T,selected.years=selected.years,year.q=year.q,min.pred.length=0)
  
}
########################################################################################
if (FALSE) {
s<-subset(SS,species=='W_H')
head(s)
subset(s,year==1986)
tapply(s$WSEA,list(s$year,s$quarter,s$species,s$age),sum)
WW[as.character(1986:1990),,'W_H',as.character(0:2)]

source(file.path(prog.path,RexchangeDir,'write_surveys.R'))
}