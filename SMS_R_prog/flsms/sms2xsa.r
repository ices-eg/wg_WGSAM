  # load packages
  library(FLCore)
  library(FLEDA)
  library(FLAssess)


  # path of the folder with VPA input data
  input.path <- "C:\\mv\\FLR-kursus\\HER_incl_GR"
  setwd(input.path)

  # file names index and tuning  !!! don't forget the extensions
  index.name  <- "index.txt"
  tuning.name <- "acoustic.tun"
  stock.name  <- "Herring 25-32"
  plusgroup   <- 8 # if no plusgroup set NA
  Fbarmin     <- 3
  Fbarmax     <- 7

## 2. import data
# ---------------

  # 2.1 Catch data
  # --------------

  # read VPA file
  stock <- read.FLStock(file=index.name,name=stock.name)