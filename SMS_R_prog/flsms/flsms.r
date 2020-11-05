
validFLSMS <- function(object){
    # All FLQuant objects must have same dimensions
    Dim <- dim(object@stock.n)
    if (!all(dim(object@harvest) == Dim))
        return("n and f arrays must have same dimensions")
    # Everything is fine
    return(TRUE)
}

# just to show contents of FLAssess

#setClass("FLAssess",
#    representation(
#        catch.n    ="FLQuant",
#        stock.n    ="FLQuant",
#        harvest    ="FLQuant",    An FLQuant that contains estimated fishing mortlaity-at-age.
#        index.name ="character",  A vector containing names for the FLIndexs
#        index.range="list",       A list containing ranges for the FLIndexs
#        index      ="FLQuants",   A list of FLQuants, corresponding to the index values used when fitting.
#        index.res  ="FLQuants",   A list of FLQuants, corresponding to the index residuals.
#        index.hat  ="FLQuants",   A list of FLQuants, corresponding to the fitted index values.
#        index.var  ="FLQuants",   A list of FLQuants, corresponding to the variances of index values in fitting
#        call       ="character",  A \code{"character"} representation of the call that created the object
#        desc       ="character")  Whatever you want.


setClass("FLSMS",
  #contains="FLAssess",
  #contains="FLCore",
    representation(       
        range.SMS="vector",
        catch.hat="FLQuant",
        catch.res="FLQuant",
        catch.var="FLQuant",
        M2       ="FLQuant",
        diagnostics="data.frame",
        control  ="FLSMS.control"),
    prototype=prototype(
        range.SMS=list(season=1, power.age=-1, q.age=0,var.age.group=as.vector(0,mode="list")),
        catch.hat=new("FLQuant"),
        catch.res=new("FLQuant"),
        catch.var=new("FLQuant"),
        M2       =new("FLQuant"),
        diagnostics=new("data.frame"),
        control  =new("FLSMS.control")),
    validity=validFLSMS
)


setValidity("FLSMS", validFLSMS)
# remove(validFLXSA)  # We do not need this function any more


### Methods #############################################################
FLSMS <- function(use.ASCII=TRUE,write.single=TRUE,read.single=TRUE,
                                 write.multi=FALSE, read.multi=FALSE,
                       cmdOptions='-nox -nohess', FLStock=NULL, FLIndices=NULL, control=NULL, desc){

 if (!use.ASCII) {
   if (is.null(FLStock))
      stop("A 'FLStock' or 'FLStocks' must be given. FLStock=")

   if (is.null(FLIndices))
      stop("A 'FLIndices' must be given")

   if (!inherits(FLStock, "FLStock") & !inherits(FLStock, "FLStocks"))
        stop("FLStock must be an 'FLStock'or an 'FLStocks' object! FLIndices=")

   if (!inherits(FLIndices, "FLIndices"))
      stop("FLIndices must be an 'FLIndices' object!, FLIndices=")
    
   if (!inherits(control, "FLSMS.control"))
      stop("control must be an 'FLSMS.control' object!")
   if (!validObject(control)) stop("control is not valid!")
    

   # write the control object on ASCII format for use by SMS
   write.FLSMS.control(control,path=SMS.path,write.multi=write.multi,nice=TRUE)

   # write the FLStockMulti object on ASCII format for use by SMS
   FLStocks2SMS(FLStock=FLStock,control=control,path=SMS.path, bio.interact=write.multi)
 
   # write the SMS input file: fleet_catch.dat, fleet_info.dat and fleet_names.in
   FLIndices2SMS(out.path=SMS.path,indices=FLIndices,control=control) 

 }  #use.ASCII  

 # run SMS
 if (.Platform$OS.type == "windows") {
   shell(paste( file.path(root,"program","sms.exe"),cmdOptions))
   #shell(paste(file.path(.path.package("FLSMS"),"exec","sms.exe"),cmdOptions))
   
 } else stop("error: just Windows systems are supported")
 

}

#FLSMS(use.ASCII=T)
