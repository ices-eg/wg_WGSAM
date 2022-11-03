
#Remove all objects
rm(list = ls())

# Operating System
OS<- .Platform$OS.type
 
# Harddisk drive for SMS, runs
if (OS=="unix") {
  dosDrive<-"~"
  root<-file.path(dosDrive,"SMS")   # root directory for the SMS package, runs
  root.prog<-root                   # root directory for the SMS package, R-programs (r-prog directory
} else {
  dosDrive<-"C:"
  root<-file.path(dosDrive,"_C_drev","SMS-git")
  root.prog<-file.path(dosDrive,"_C_drev","SMS-git")
}

root.copy<-root
#my.stock.dir<-c("NorthSeaKeyRun_2020_single","NorthSeaKeyRun_2020_ICES","NorthSeaKeyRun_2020","NorthSeaKeyRun_2020_withF")[3]
#my.stock.dir<-"NorthSeaKeyRun_2020"



#my.stock.dir<-"NS_2020_multi_test"
#my.stock.dir<-"Baltic-2019-keyRun"
#my.stock.dir<-"Baltic-2022"
my.stock.dir<-"Baltic-2022_all_new_stom"  # all stomachs included (and not scaled number of stomachs)

my.stock.dir<-"Baltic-2022_same_un_scaled" 
my.stock.dir<-"Baltic-2019-keyRun"
my.stock.dir<-"Baltic-2022-keyRun"
my.stock.dir<-"Baltic-2022-keyRun - Copy"
my.stock.dir<-"Baltic-2022-V01_same_stomachs_as_in_2019"
my.stock.dir<-"Baltic-2022-V02_Updated_stomachs"

my.stock.dir<-"Baltic-2022-V03_Updated_consum"
my.stock.dir<-"Baltic-2022-V04_Updated_Fmodel"
my.stock.dir<-"Baltic-2022-keyRun"
my.stock.dir<-"Baltic-2022-V05_new05"
my.stock.dir<-"Baltic-2022-V06_new10"
my.stock.dir<-"Baltic-2022-V07_M1sensiti"
my.stock.dir<-"Baltic-2022-V08_age1"

my.stock.dir<-"Baltic-2022-keyRun_cons_multiplier"
my.stock.dir<-"Baltic-2022-singleS_new_configuration"


my.stock.dir<-"Baltic-2022-keyRun"
my.stock.dir<-"Baltic-2022-V08_age1"



#my.stock.dir<-"Baltic-2022-ICES"
# make a backup of the SMS source code
# file.copy(file.path(root.prog,"program","sms.tpl"),file.path(root.prog,"program",paste("sms_",format(Sys.time(), "%Y_%m_%d-%H-%M"),'.tpl',sep='')),overwrite =TRUE)
# file.copy(file.path(root.prog,"program","op.tpl"),file.path(root.prog,"program",paste("op_",format(Sys.time(), "%Y_%m_%d-%H-%M"),'.tpl',sep='')),overwrite =TRUE)

#Installation of FLR, if needed
# install.packages("FLCore", repos="http://R-Forge.R-project.org")
###################### do not change the code below this line ###############################################################################

makeAllGraphs<-F      # batch job to make "all" graphs and tables after a key-run



# Use all libraries or just a simple configuration (few libraries and limited access to FLR) or full access to 
allLibraries<-FALSE

# Path to data directory
data.path<-file.path(root,my.stock.dir)

# Path to R-programme directory, do not change
prog.path<-file.path(root.prog,"r_prog")
my.FLR.path<-file.path(root.prog,"r_prog","flsms")

                                                                  
# path to the sms.exe file, used for retrospective runs
#sms.command<-file.path("..","program","sms") 

# libraries
library(lattice)
#library(MASS)
#library(gtools)  # for def of macro

if (allLibraries) library(quantreg)
library(tidyverse)
setwd(data.path)

# Path to R-programme directory
prog.path.func<-file.path(root.prog,"r_prog","function")

source(file.path(prog.path.func,"init_r_functions.r"))
cat("active stock directory:",getwd(),"\n");


save(list = ls(all.names = TRUE), file = file.path(data.path,"SMS.RData"), envir = .GlobalEnv)


