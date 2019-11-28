
#Remove all objects
rm(list = ls())

# Operation System
OS<- .Platform$OS.type
 
# Harddisk drive for SMS, runs
if (OS=="unix") {
  dosDrive<-"~"
  root<-file.path(dosDrive,"SMS")   # root directory for the SMS package, runs
  root.prog<-root                   # root directory for the SMS package, R-programs (r-prog directory)
} else {
  dosDrive<-"H:"
  root<-file.path(dosDrive,"_C_drev","SMS-git")
  root.prog<-file.path(dosDrive,"_C_drev","SMS-git")
}


#my.stock.dir<-'NorthSeaKeyRun_2017'
#my.stock.dir<-"R02_NorthSea"



#my.stock.dir<-"Baltic-2019-single"
#my.stock.dir<-"Baltic-2019-keyRun"
#my.stock.dir<-"Baltic-2019-no_size_selection4"
#my.stock.dir<-"Baltic-2019-no_size_selection0"
#my.stock.dir<-"Baltic-2019-keyRun_retro"

#my.stock.dir<-"Baltic-2019-old_stomachs"
#my.stock.dir<-"Baltic-2019-old-new_stomachs"
#my.stock.dir<-"Baltic-2019-stom5"
#my.stock.dir<-"Baltic-2019-stom10"

#my.stock.dir<-"Baltic-2019-OtherFoodOverlap"
#my.stock.dir<-"Baltic-2019-consumptionEstimate"
my.stock.dir<-"Baltic-2019-keyRun"

# make a backup of the SMS source code
# file.copy(file.path(root.prog,"program","sms.tpl"),file.path(root.prog,"program",paste("sms_",format(Sys.time(), "%Y_%m_%d-%H-%M"),'.tpl',sep='')),overwrite =TRUE)
#file.copy(file.path(root.prog,"program","op.tpl"),file.path(root.prog,"program",paste("op_",format(Sys.time(), "%Y_%m_%d-%H-%M"),'.tpl',sep='')),overwrite =TRUE)

#Installation of FLR, if needed
# install.packages("FLCore", repos="http://R-Forge.R-project.org")
###################### do not change the code below this line ###############################################################################

makeAllGraphs<-F      # batch job to make "all" graphs and tables after a key-run



# Use all libraries or just a simple configuartion (few libraries and limited access to FLR) or full access to 
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
library(MASS)
#library(gtools)  # for def of macro

if (allLibraries) library(quantreg)
library(tidyverse)
setwd(data.path)

# Path to R-programme directory
prog.path.func<-file.path(root.prog,"r_prog","function")

source(file.path(prog.path.func,"init_r_functions.r"))
cat("active stock directory:",getwd(),"\n");

