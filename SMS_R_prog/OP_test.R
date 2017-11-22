scenario.dir<-"~/SMS/v01-NS-2015/HCR_2_stoch3_test_01_HCR2_0_Rec3__2124"
setwd(scenario.dir)

load(file =file.path(scenario.dir, "condensed.RData"),verbose=T)
summary(condensed)

load(file =file.path(scenario.dir, "Fcomb.RData"),verbose=T)
summary(Fcomb)
dim(Fcomb)

load(file =file.path(scenario.dir, "indicators.RData"),verbose=T)
summary(indi)
dim(indi)


load(file =file.path(scenario.dir, "a.RData"),verbose=T)
summary(a)

#indi<-read.table( "op_indicator_system_avg.out",header=T)
#summary(indi)

