
if (FALSE) {

dirs<-c("NorthSeaKeyRun_2017", "NorthSeaKeyRun_2020")  
labels<-c("2017 keyrun","2020 keyrun")

dirs<-c("NorthSeaKeyRun_2020", "NorthSeaKeyRun_2020_check")  
labels<-c("2020 keyrun","2020 check")



dirs<-c("NS_2020_APP","NS_2020_APP_half","NS_2020_APP_quart")
labels<-c("app", "half",'Quart')


dirs<-c("NorthSeaKeyRun_2020", "NorthSeaKeyRun_2020_her02")  
labels<-c("M1=0.1","M1=0.2")


dirs<-c("Baltic-2019-keyRun", "Baltic-2022-same_stomachs_as_in_2019","Baltic-2022")  
labels<-c("2019 keyrun","2022 same stom" ,"2022")

dirs<-c("Baltic-2019-keyRun","Baltic-2022-same_stomachs_as_in_2019","Baltic-2022","Baltic-2022_all_new_stom")  
labels<-c("2019 keyrun","2022 2019 stom","2022","2022 all new stom")

dirs<-c("Baltic-2019-keyRun","Baltic-2022-keyRun")  
labels<-c("2019 keyrun","2022 keyrun")


dirs<-c("Baltic-2019-keyRun","Baltic-2022-keyRun")  
labels<-c("2019 keyrun","2022 keyrun")


dirs<-c("Baltic-2019-keyRun","Baltic-2022-keyRun","Baltic-2022-same_stomachs_as_in_2019")  
labels<-c("2019 keyrun","2022 keyrun","2022 and 2019 stom")

dirs<-c("Baltic-2019-keyRun","Baltic-2022-keyRun","Baltic-2022-keyRun - Copy")  
labels<-c("2019 keyrun","2022 keyrun","2022 copy")


dirs<-c("Baltic-2019-keyRun","Baltic-2022-V01_same_stomachs_as_in_2019")  
labels<-c("2019 keyrun","2022 assess. update")



dirs<-c("Baltic-2022-V01_same_stomachs_as_in_2019","Baltic-2022-V02_Updated_stomachs")  
labels<-c("2022 assess. update","same, but updated diet")



dirs<-c("Baltic-2022-V02_Updated_stomachs","Baltic-2022-V02_Updated_stomachs - Copy")  
labels<-c("stom","stom copy")


dirs<-c("Baltic-2022-V01_same_stomachs_as_in_2019","Baltic-2022-V02_Updated_stomachs","Baltic-2022-V03_Updated_consum")  
labels<-c("2022 assess. update","same, but updated diet", "and updated ration")


dirs<-c("Baltic-2022-V01_same_stomachs_as_in_2019","Baltic-2022-V02_Updated_stomachs","Baltic-2022-V03_Updated_consum","Baltic-2022-V04_Updated_Fmodel_2")  
labels<-c("2022 assess. update","same, but updated diet", "and updated ration","and updated F-model")


dirs<-c("Baltic-2022-keyRun","Baltic-2022-V05_new05","Baltic-2022-V06_new10")  
labels<-c("2022 key run","stomachs 5 years", "stomachs 10 years")

dirs<-c("Baltic-2012-keyRun-results","Baltic-2019-keyRun","Baltic-2022-keyrun")  
labels<-c("2012 keyrun","2019 keyrun","2022 keyrun")


}



compare_runs_M2(
  dirs=dirs,
  labels=labels,
  sumQuarterly=FALSE,  # calc M2 as sum of quarterly M2
  nox=3, noy=2,
  paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
  run.ID='SMS',         # file id used for paper output
  doGrid=TRUE,
  extent.SSB=FALSE,  # plot SSB for the year after last assessment year
  first.year.on.plot=1974,
  last.year.on.plot=2021,
  include.assess.forcast.line=FALSE,      # vertical line at last assessment year
  include.F.reference.points=FALSE,
  include.SSB.reference.points=FALSE,
  include.1.std=FALSE,                   # Include values plus/minus 1 times the standard deviation
  include.2.std=FALSE,
  #incl.sp=c('Herring'),                      # species number to be included. Numbers or "all"
  incl.sp="all",
  first.pch=0,    # first pch symbol
  first.color=1,   # first color
  palette="default"               # good for clolorfull plots
  #palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only
) 
  
  