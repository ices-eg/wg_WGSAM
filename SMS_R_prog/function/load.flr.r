
library(FLCore)
#library(FLAssess)
 #  library(FLEDA)
if (allLibraries) { 
  #ibrary(FLXSA)
  #library(FLBRP)
  #library(FLHCR)
  #library(FLEDA)

 }

do.test<-FALSE


#library(FLSMS)

#overwrite FLSMS library
source(file.path(my.FLR.path,"flsms.control.r"))
source(file.path(my.FLR.path,"flsms.predict.control.r"))

source(file.path(my.FLR.path,"flstockmulti.r"))
source(file.path(my.FLR.path,"flindex.sms.r"))
source(file.path(my.FLR.path,"xsa2sms.r"))
source(file.path(my.FLR.path,"sms2flsmss.r"))

source(file.path(my.FLR.path,"flsms.r"))
source(file.path(my.FLR.path,"flsmss.r"))

source(file.path(my.FLR.path,"flop.control.r"))
source(file.path(my.FLR.path,"flop_msfd.control.r"))



