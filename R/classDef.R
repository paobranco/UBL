################################################################# 
## THIS FILE CONTAINS THE CLASSES AND CONSTRUCTORS              #
## THAT ARE DEFINED IN THIS PACKAGE                             #
#################################################################
## Author : Paula Branco                                        #
## Dez 2017
## License: GPL (>= 2)                                          #
#################################################################

setClassUnion("OptMatrix",c("matrix","NULL"))
setClassUnion("OptNumeric",c("numeric","NULL"))


## ==============================================================
## CLASS: BagModel
##
## Class for storing information concerning a bagging ensemble
## ==============================================================


setClass("BagModel",
         slots = c(form = "formula",
                   train = "data.frame",
                   learner = "character",
                   learner.pars ="list",
                   baseModels = "list",
                   aggregation = "character",
                   rel = "OptMatrix",
                   thr.rel= "OptNumeric",
                   quiet= "logical")
)


## --------------------------------------------------------------
## constructor
## --------------------------------------------------------------

BagModel <- function(form, train, learner, learner.pars, baseModels,
                     aggregation, rel=NULL, thr.rel=NULL, quiet) {
  if ( aggregation != "Average")
    stop("Only average method is available for models aggregation", call. = FALSE)
  
  new("BagModel",
      form = form,
      train = train,
      learner = learner,
      learner.pars = learner.pars,
      baseModels = baseModels,
      aggregation = aggregation,
      rel = rel,
      thr.rel=thr.rel,
      quiet=quiet)
}
