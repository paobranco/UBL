##########################################################################
## Standard Bagging Ensemble for regression
##########################################################################

# standard bagging that trains nmodels of type learner with parameters set
# by learner.pars. The models are trained with a random sample of the 
# training set with size = to the trainin set size.
# Final predictions are obtained through simple average

BaggingRegress <- function(form, train, nmodels, learner, learner.pars,
                           aggregation = "Average", quiet=TRUE){
  n <- nrow(train)
  BL <- lapply(seq_len(nmodels), function(o) {
    samp <- sample(n, n, replace=TRUE)
    do.call(learner, c(list(form, train[samp, ]), learner.pars))
  })
  names(BL) <- paste0("M", seq_along(BL))
  
  BagModel(form, train, learner=learner, learner.pars=learner.pars, baseModels=BL,
           aggregation=aggregation, rel=NULL, thr.rel=NULL, quiet=quiet)
  
}


