
if (FALSE) {

  dirs<-c("NorthSeaKeyRun_2020_ICES", "NorthSeaKeyRun_2020_single")  
  labels<-c("ICES","SMS single sp.")
  
dirs<-c("NorthSeaKeyRun_2017", "NorthSeaKeyRun_2020")   # directory files to be compared
labels<-c("2017 keyrun","2020 keyrun") # output legends

dirs<-c("NorthSeaKeyRun_2020", "NorthSeaKeyRun_2020_check","NorthSeaKeyRun_2020_final")  
labels<-c("2020 keyrun","2020 check","2020 final")


dirs<-c("NS_2020_APP","NS_2020_APP_half")
labels<-c("app", "half")


dirs<-c("NorthSeaKeyRun_2020", "NorthSeaKeyRun_2020_her02")  
labels<-c("M1=0.1","M1=0.2")


dirs<-c("NS_2020_multi", "NS_2020_multi_all_ricker")  
labels<-c("multi","multi all ricker")

dirs<-c("Baltic-2019-keyRun", "Baltic-2022-same_stomachs_as_in_2019","Baltic-2022")  
labels<-c("2019 keyrun","2022 same stom" ,"2022")


dirs<-c("Baltic-2019-keyRun","Baltic-2022-keyRun","Baltic-2022-same_stomachs_as_in_2019")  
labels<-c("2019 keyrun","2022 keyrun","2022, 2019 stom")



dirs<-c("Baltic-2022-ICES","Baltic-2022-singleS","Baltic-2022-singleS_new_configuration")  
labels<-c("ICES-single","SMS-single","SMS-single new config.")

dirs<-c("Baltic-2022-ICES","Baltic-2022-singleS","Baltic-2022-singleS - Copy")  
labels<-c("ICES-single","SMS-single","Copy")


dirs<-c("Baltic-2022-singleS","Baltic-2022-singleS - Copy")  
labels<-c("SMS-single","Copy")


dirs<-c("Baltic-2022-keyRun","Baltic-2022-V08_age1")  
labels<-c("2022 key run","age 1")


}


compare_runs(
  dirs=dirs,
  labels=labels,
  nox=2, noy=2,
  paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
  run.ID='ICES_com',         # file id used for paper output
  doGrid=TRUE,
  extent.SSB=FALSE,  # plot SSB for the year after last assessment year
  first.year.on.plot=1974,
  last.year.on.plot=2021,
  plot.MCMC=FALSE,                        # plot values from MCMC scenarios. FALSE=plot hindcast values from "summary_table_raw.out"
  single.species=TRUE,                   # single species mode or multi species mode
  include.assess.forcast.line=FALSE,      # vertical line at last assessment year
  include.F.reference.points=FALSE,
  include.SSB.reference.points=FALSE,
  include.1.std=FALSE,                   # Include values plus/minus 1 times the standard deviation
  include.2.std=FALSE,
  #incl.sp=c('Herring'),                      # species number to be included. Numbers or "all"
  #incl.sp="all",
  first.pch=0,    # first pch symbol
  first.color=1,   # first color
  palette="default"               # good for colour full plots
  #palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only
)  
  