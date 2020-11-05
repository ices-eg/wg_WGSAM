# FLSMSs.R - FLSMSs class and methods

# Author: 
# Maintainer: Morten Vinther
# Additions:
# Last Change: 

# Reference:
# Notes: Code is copied from FLStocks.R

### class ##############################################################
validFLSMSs <- function(object){
    # If the list is empty, then it is OK
    if (length(object) == 0)
        return(TRUE)
    # Make sure the list contains only FLSMS items
    for (i in 1:length(object))
        if (!inherits(object[[i]], "FLSMS"))
            return("Items must be FLSMS objects!")
    # Everything is fine
    return(TRUE)
}

setClass("FLSMSs",
    representation(
        "list",#  a list of FLSMS objects
        desc ="character"),
    prototype=prototype(
        list(),
        desc =character(0)),
    validity=validFLSMSs
)

setValidity("FLSMSs", validFLSMSs)
remove(validFLSMSs)   # We do not need this function any more
### End class ###########################################################

### FLSMSs()

FLSMSs <- function(..., desc=NULL) {

    new <- new("FLSMSs")
 
    objects <- list(...)

    if(!is.null(desc))
        new@desc <- desc

    if (length(objects) > 0) {
        for (n in 1:length(objects)) {
            object <- objects[[n]]
            if (!inherits(object, "FLSMSs"))
                stop("input must be one or more FLSMS objects")
            new[[n]] <- object
        }
    }
    return(new)
}

### Methods #############################################################

## summary
setMethod("summary", signature(object="FLSMSs"),
    function(object, ...){
        cat("An object of class \"FLSMSs\" with:\n\n")
        cat("Description:", object@desc, "\n\n")
        cat("Containing", length(object), "stocks:\n")
        if (length(object) > 0)
            for (i in 1:length(object))
                cat("Stock", i, ":", object[[i]]@name, "\n")
        # Should we add some summary stats for the different slots here?
    }
)


### End methods ###########################################################

# Test if an object is of FLStock class
is.FLSMSs <- function(x)
    return(inherits(x, "FLSMSs"))
