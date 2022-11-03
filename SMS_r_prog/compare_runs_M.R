
if (FALSE) {

dirs<-c("NorthSeaKeyRun_2017", "NorthSeaKeyRun_2020")  
labels<-c("2017 keyrun","2020 keyrun")

dirs<-c("NorthSeaKeyRun_2020", "NorthSeaKeyRun_2020_check")  
labels<-c("2020 keyrun","2020 check")



dirs<-c("NS_2020_APP","NS_2020_APP_half","NS_2020_APP_quart")
labels<-c("app", "half",'Quart')


dirs<-c("NorthSeaKeyRun_2020", "NorthSeaKeyRun_2020_her010_mod")  
labels<-c("M1=0.1","M1=0.10mod")

dirs<-c("Baltic-test", "Baltic-test-oth-noise")  
labels<-c("defualt","noise")


dirs<-c("Baltic-2022-keyRun", "Baltic-2022-V07_M1sensiti")  
labels<-c("2022 key run","M1*0.5")
}






compare_runs_M(
  dirs=dirs,
  labels=labels,
  sumQuarterly=FALSE,  # calc M as sum of quarterly M2
  nox=3, noy=2,
  paper=TRUE,      # graphics on paper=file (TRUE) or on screen (FALSE)
  run.ID='SMS',         # file id used for paper output
  doGrid=TRUE,
  extent.SSB=FALSE,  # plot SSB for the year after last assessment year
  first.year.on.plot=1974,
  last.year.on.plot=2020,
  include.assess.forcast.line=FALSE,      # vertical line at last assessment year
  include.F.reference.points=FALSE,
  include.SSB.reference.points=FALSE,
  include.1.std=FALSE,                   # Include values plus/minus 1 times the standard deviation
  include.2.std=TRUE,
 # incl.sp=c('Herring'),                      # species number to be included. Numbers or "all"
  #incl.sp="all",
  first.pch=0,    # first pch symbol
  first.color=1,   # first color
  palette="default"               # good for clolorfull plots
  #palette(gray(seq(0,.9,len=10)))  # gray scale for papers, use len =500 to get black only
) 
  
  