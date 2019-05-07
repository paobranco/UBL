
################################################################
## BagModel methods
################################################################


setMethod("show",
          signature("BagModel"),
          function(object) {
            if(is.null(object@rel)){
              cat("Standard Regression Problem.\n")
            } else {
              cat("Imbalanced Regression Problem.\n")
            }
            cat("Ensemble of ", length(object@baseModels), " bagged ", object@learner, ".\n", sep="")
            cat("Target variable: ", (object@form)[[2]], ".\n", sep="")
            cat("Strategy for models aggregation: ", (object@aggregation), ".\n", sep="")
          })


setMethod("predict",
          signature("BagModel"),
          function(object, newdata) {
            switch(object@aggregation,
                   "Average" = {
                     if(object@quiet==FALSE){
                     cat("Strategy for models aggregation: Averaging\n",
                         "Predicting new instances...\n")
                     }
                     if (object@learner == "gbm"){
                       n.trees <- object@learner.pars$n.trees
                       Y_hat <- sapply(object@baseModels, predict, newdata=newdata, n.trees=n.trees)
                       apply(Y_hat, 1, mean)
                     } else {
                       Y_hat <- sapply(object@baseModels, predict, newdata)
                       apply(Y_hat, 1, mean)
                     }
                   })
          })

