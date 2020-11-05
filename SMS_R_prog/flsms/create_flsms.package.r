old.path<-getwd()

if (FALSE) {
  # Run only one time
  setwd(root)
  package.skeleton( name="FLSMS",list=c("FLSMS.control"  ))
}

#############################################
setwd(file.path(root,"FLSMS","man"))

    promptClass("FLSMS")
    #prompt(FLSMS)

    promptClass("FLSMSs")

    # done promptClass("FLSMS.control")
    prompt(FLSMS.control)
    prompt(read.FLSMS.control)
    prompt(write.FLSMS.control)

    #promptClass("FLSMS.predict.control")
    prompt(FLSMS.predict.control)
    prompt(read.FLSMS.predict.control)
    prompt(write.FLSMS.predict.control)

    promptClass("FLStockMulti")
    prompt(FLStockMulti)
    prompt(FLStocks2SMS)
    prompt(SMS2FLStocks)

    promptClass("FLIndex.SMS")
    prompt(FLIndex.SMS)
    prompt(FLIndices2SMS)
    prompt(SMS2FLIndices)
 
##########################################
setwd(old.path)
