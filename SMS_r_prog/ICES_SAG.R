# Read ICES summary for a number of stocks

library(icesSAG)
# ?icesSAG
assessmentYear<-2022


st<-getListStocks(assessmentYear) %>% filter(Purpose=="Advice") %>% dplyr::select(StockKeyLabel,StockKey,AssessmentKey,Status)
st

# stock codes wanted
stocks<-c("her.27.25-2932","spr.27.22-32")
codes<-c('HER','SPR')
names(codes)<-stocks

# do it yourself to get all the years you want
doIt<-function(stock,year = assessmentYear,  vars=c("AssessmentKey", "AssessmentYear","Year","Recruitment","FishingPressure","Landings","Catches","Fpa","Flim","Blim", "Bpa", "FMSY", "MSYBtrigger","StockSize")) {
  #  stock<-"cod.27.47d20"; year = assessmentYear;   vars<-c("AssessmentKey", "AssessmentYear", "StockKeyLabel","Year","Recruitment","FishingPressure","Fpa","Flim","Blim", "Bpa", "FMSY", "MSYBtrigger","StockSize")
  
  
  key<-findAssessmentKey(stock, year = year,full=TRUE)
  
  if (dim(key)[[1]]==0) {
    cat('Stock:', stock, ' is not found. NULL is returned\n')
    return(NULL)
  } 
  if (length(key)==1) {
    key<-key$AssessmentKey
  } else {
    key<-subset(key,Purpose=='Advice')
    if (dim(key)[[1]] !=1) {
      cat('Stock:', stock, "Key:",key, ' is not unique. NULL is returned\n')
      return(NULL)
    } else   key<-key$AssessmentKey
  }
  cat(stock,' Key:',key,'\n')
  
  x<-getStockDownloadData(key)
  nam<-colnames(x[[1]])
  found<-vars %in% nam
  found_vars<-vars[found]
  x<-x[[1]] %>% dplyr::select(!!found_vars) 
  
  #insert NA for missing values
  not_found_vars<-vars[!found]
  vars_mis<-NA
  for (xx in not_found_vars) {
    names(vars_mis)<-xx
    x<- x %>% mutate(!!!vars_mis)
  }
  
  x <- x %>% mutate(stock=!!stock,Species=codes[stock])
  
  return(x)
}

her<-doIt("her.27.25-2932")

# and now for all stocks
out2<-lapply(stocks,doIt)

out<-as_tibble(do.call(rbind,out2))

out


# write SMS file summary_table_raw.out
#Species.n Year  Rec SSB TSB SOP SOP.hat SOP.core Yield Yield.hat Yield.core mean.F Eaten

out3<-out %>% transmute(Species.n=if_else(Species=='HER',2,3),Year=Year, Rec=Recruitment,SSB=StockSize,mean.F=FishingPressure,SOP=Catches,SOP.hat=Catches,Yield=Catches,yield.core=Catches,Eaten=0)
out3

write_delim(out3,file="summary_table_raw.out")
