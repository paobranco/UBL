##############################################################################
## Functions for evaluating metrics developed for utility-based learning
## These metrics were developed for both classification and regression tasks 
##############################################################################

# matrix/surface provided must always contain the true class/value in 
# the rows and the predicted class/value in the columns

##############################################################################
## Classification metrics
##############################################################################

EvalClassifMetrics <- function(trues, preds, mtr, type = "util", metrics = NULL,
                               thr = 0.5, beta = 1){
# inputs:  
#   trues:   vector with true classes
#   preds:   vector with predicted classes
#   mtr:     one matrix that can be either a cost, a benefit or a utility
#            matrix.
#            The matrix must be always provided with the true class in 
#            the rows and the predicted class in the columns
#   type:    the type of matrix. Can be set to "util" (the default), "ben"
#            or "cost".
#   metrics: metrics to be evaluated
#   thr:     the threshold on the relevance values used for determining which 
#            are the important classes to consider. This threshold is only 
#            necessary for the following metrics: precPhi, recPhi and FPhi. 
#            These metrics are only available when using a utility matrix.
#   beta:    beta parameter for the F-score
#
# outputs:   named list with each metric results
  type <- match.arg(type, c("cost", "benefit", "utility"))
  ## Checking trues and preds
  if (!is.null(dim(trues))) stop("EvalclassifMetrics:: expecting a vector as trues.",
                                 call.=FALSE)
  if (!is.null(dim(preds))) stop("EvalclassifMetrics:: expecting a vector as predictions.",
                                 call.=FALSE)
  
  Cls <- unique(c(levels(as.factor(trues)), levels(as.factor(preds))))
  preds <- factor(preds, levels = Cls)
  trues <- factor(trues, levels = Cls)
  res <- list()

  if (is.null(mtr)) stop("EvalClassifMetrics:: expecting a matrix in param mtr and none provided.",
                         call. = FALSE)
  
  UtilMetrics <- c("totUtil", "MUtil", "NMUtil", "precPhi", "recPhi", "FPhi")
  CostMetrics <- c("totCost", "MCost", "NMCost")
  BenMetrics <- c("totBen", "MBen", "NMBen")

  if(!is.null(metrics)){
    if( (type=="utility" && !(metrics %in% UtilMetrics)) || 
        (type=="cost" && !(metrics %in% CostMetrics)) ||
        (type=="benefit" && !(metrics %in% BenMetrics))  ) { 
      stop("EvalClassifMetrics:: The metrics selected do not match the type of matrix provided.",
           call. = FALSE)
    }
  }
  
  if(is.null(metrics) && type == "utility"){
    metrics <- UtilMetrics
  }
  if(is.null(metrics) && type == "benefit"){
    metrics <- BenMetrics
  }
  if(is.null(metrics) && type == "cost"){
    metrics <- CostMetrics
  }
  
  
  
  if("totUtil" %in% metrics){
    # sum_{i=1}^N U(y_i, \hat(y)_i)
      res[["totUtil"]] <- totUtilClassif(trues, preds, mtr)
  }
  if("totCost" %in% metrics){ 
    # sum_{i=1}^N C(y_i, \hat(y)_i)
      res[["totCost"]] <- totCostClassif(trues, preds, mtr)
  }
  if("totBen" %in% metrics){ 
    # sum_{i=1}^N C(y_i, \hat(y)_i)
    res[["totBen"]] <- totBenClassif(trues, preds, mtr)
  }
  if("MUtil" %in% metrics){ 
    # MU = \frac{sum_{i=1}^N U(y_i, \hat(y_i)) }{N}: range: [-1, 1]
      res[["MUtil"]] <- MUtilClassif(trues, preds, mtr)
  }
  if("MCost" %in% metrics){ 
    # MCost = \frac{sum_{i=1}^N C(y_i, \hat(y)_i)}{N}: range:[0,max(C(y, \hat(y)))]
    res[["MCost"]] <- MCostClassif(trues, preds, mtr)
  }
  if("MBen" %in% metrics){ 
    # MBen = \frac{sum_{i=1}^N B(y_i, \hat(y)_i)}{N}: range:[0,max(B(y, \hat(y)))]
    res[["MBen"]] <- MBenClassif(trues, preds, mtr)
  }
  
  if("NMUtil" %in% metrics){ 
    # NMU= \frac{(\sum_{i=1}^N(U(y_i, \hat(y_i))))+N}{2N}: range:[0,1]
      res[["NMUtil"]] <- NMUtilClassif(trues, preds, mtr)
  }

  if("NMCost" %in% metrics){ 
    # NMC= \frac{(\sum_{i=1}^N(C(y_i, \hat(y_i))))}{N+max(C(y_i, \hat(y_i)))}: range:[0,1]
    res[["NMCost"]] <- NMCostClassif(trues, preds, mtr)
  }
  
  if("NMBen" %in% metrics){ 
    # NMB= \frac{(\sum_{i=1}^N(B(y_i, \hat(y_i))))}{N+max(B(y_i, \hat(y_i)))}: range:[0,1]
    res[["NMBen"]] <- NMBenClassif(trues, preds, mtr)
  }

  if("precPhi" %in% metrics){
    # pre_\phi=\frac{sum_{\forall \hat{y}_i>thr} (1+U(y_i, \hat{y}_i))}
    #               {sum_{\forall \hat{y}_i>thr} (1+\phi(\hat{y}_i))}
    res[["precPhi"]] <- precPhiClassif(trues, preds, mtr, thr)
  }

  if("recPhi" %in% metrics){
    # rec_\phi=\frac{sum_{\forall y_i>thr} (1+U(y_i, \hat{y}_i))}
    #               {sum_{\forall y_i>thr} (1+\phi(y_i))}
    res[["recPhi"]] <- recPhiClassif(trues, preds, mtr, thr)
  }
  
  if("FPhi" %in% metrics){
    # F_\phi\beta=(1+beta^2)*frac{prec*rec}{beta^2*prec+rec}
    res[["FPhi"]] <- FPhiClassif(trues, preds, mtr, thr, beta)
  }

  res
}

totUtilClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and utility matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)
  res
}

MUtilClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and utility matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)/N
  res
}


NMUtilClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues,preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and utility matrices do not match",
         call. = FALSE)
  res <- (sum(confm*mtr)+N)/(2*N)
  res
}



totCostClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and cost matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)
  res
}


MCostClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and cost matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)/N
  res
}

NMCostClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues,preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and cost matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)/(N*max(mtr))
  res
}


totBenClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and benefit matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)
  res
}

MBenClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and benefit matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)/N
  res
}


NMBenClassif <- function(trues, preds, mtr){
  N <- length(trues)
  confm <- as.matrix(table(trues,preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and benefit matrices do not match",
         call. = FALSE)
  res <- sum(confm*mtr)/(N*max(mtr))
  res
}


precPhiClassif <- function(trues, preds, mtr, thr){
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and utility matrices do not match",
         call. = FALSE)
  relClasses <- NULL
  for(i in 1:nrow(mtr)){if(mtr[i,i]>thr) relClasses <- c(relClasses, i)}
  tprod <- confm*(mtr+1)
  res <- sum(tprod[,relClasses]) / 
    sum(diag(mtr+1)[relClasses]*(colSums(confm[,relClasses])))
  res
}

recPhiClassif <- function(trues, preds, mtr, thr){
  confm <- as.matrix(table(trues, preds))
  if(!all(dim(confm)==dim(mtr)))
    stop("EvalclassifMetrics:: dimensions of confusion and utility matrices do not match",
         call. = FALSE)
  relClasses <- NULL
  for(i in 1:nrow(mtr)){if(mtr[i,i]>thr) relClasses <- c(relClasses, i)}
  tprod <- confm*(mtr+1)
  res <- sum(tprod[relClasses,]) / 
    sum(diag(mtr+1)[relClasses]*(rowSums(confm[relClasses,])))
  res
}

FPhiClassif <- function(trues, preds, mtr, thr, beta){
  rec <- recPhiClassif(trues, preds, mtr, thr)
  prec <- precPhiClassif(trues, preds, mtr, thr)
  res <- (1+beta^2)*(prec*rec)/(beta^2*prec + rec)
  res
}

#############################################################################
# Regression metrics
#############################################################################
  
 EvalRegressMetrics <- function(trues, preds, util.vals, type = "util", metrics = NULL,
                                thr = 0.5, control.parms = NULL, beta = 1,
                                maxC = NULL, maxB = NULL){
#   # inputs:  
#   trues:          vector with true values
#   preds:          vector with predicted values
#   util.vals:      the utility values of each pair of points (true, pred) provided
#   type:           the type of surface being considered. Can be set to "util"
#                   (the default), "ben" or "cost".
#   metrics:        metrics to be evaluated
#   thr:            the threshold on the relevance values used to decide which are
#                   the important and unimportant cases
#   beta:           beta parameter for the F-score
#   maxC:           the maximum Cost achievable in the Cost surface
#   maxB:           the maximum Benefit achievable in the Benefit surface
#   control.parms:  the control.parms of the relevance function phi. These are
#                   only necessary for evaluating the following utility metrics:
#                   recPhi, precPhi and FPhi.
#
# outputs:  named list with each metric results

   type <- match.arg(type, c("cost", "benefit", "utility"))

     ## Checking trues, preds and utility values
  if (!is.null(dim(trues))) stop("EvalRegressMetrics:: expecting a vector as trues.",
                                 call. = FALSE)
  if (!is.null(dim(preds))) stop("EvalRegressMetrics:: expecting a vector as predictions.",
                                 call. = FALSE)
  if (!is.null(dim(util.vals))){
    if(type == "utility"){
      stop("EvalRegressMetrics:: expecting a vector with utility values.",
           call. = FALSE)
    } else if (type == "cost"){
      stop("EvalRegressMetrics:: expecting a vector with cost values.",
           call. = FALSE)
    } else if(type == "benefit"){
      stop("EvalRegressMetrics:: expecting a vector with benefit values.",
           call. = FALSE)
    }
  }
   res <- list()
   
   UtilMetrics <- c("totUtil", "MUtil", "NMUtil")
   UtilMetricsL <- c("totUtil", "MUtil", "NMUtil", "precPhi", "recPhi", "FPhi")
   CostMetrics <- c("totCost", "MCost", "NMCost")
   BenMetrics <- c("totBen", "MBen", "NMBen")
 
   if(!is.null(metrics)){
     if( (type=="utility" && !(metrics %in% UtilMetricsL)) || 
         (type=="cost" && !(metrics %in% CostMetrics)) ||
         (type=="benefit" && !(metrics %in% BenMetrics))  ) { 
       stop("EvalRegressMetrics:: The metrics selected do not match the type of matrix provided.",
            call. = FALSE)
     }
   }
   
   if(is.null(metrics) && type=="utility"){
     if (!is.null(control.parms)){
       metrics <- UtilMetricsL
     } else {
       metrics <- UtilMetrics
     }
   }
   if(is.null(metrics) && type=="benefit"){
     metrics <- BenMetrics
   }
   if(is.null(metrics) && type=="cost"){
     metrics <- CostMetrics
   }
   
   if(type=="cost" && is.null(maxC)){
     stop("EvalRegressMetrics:: maxC must be set when evaluating cost based metrics",
          call. = FALSE)
   }
   
   if(type=="benefit" && is.null(maxB)){
     stop("EvalRegressMetrics:: maxB must be set when evaluating benefit based metrics",
          call. = FALSE)
   }
     
   if("totUtil" %in% metrics){
     # sum_{i=1}^N U(y_i, \hat(y)_i)
     res[["totUtil"]] <- totUtilRegress(trues, preds, util.vals)
   }
   if("totCost" %in% metrics){ 
     # sum_{i=1}^N C(y_i, \hat(y)_i)
     res[["totCost"]] <- totCostRegress(trues, preds, util.vals)
   }
   if("totBen" %in% metrics){ 
     # sum_{i=1}^N C(y_i, \hat(y)_i)
     res[["totBen"]] <- totBenRegress(trues, preds, util.vals)
   }
   if("MUtil" %in% metrics){ 
     # MU = \frac{sum_{i=1}^N U(y_i, \hat(y_i)) }{N}: range: [-1, 1]
     res[["MUtil"]] <- MUtilRegress(trues, preds, util.vals)
   }
   if("MCost" %in% metrics){ 
     # MCost = \frac{sum_{i=1}^N C(y_i, \hat(y)_i)}{N}: range:[0,max(C(y, \hat(y)))]
     res[["MCost"]] <- MCostRegress(trues, preds, util.vals)
   }
   if("MBen" %in% metrics){ 
     # MBen = \frac{sum_{i=1}^N B(y_i, \hat(y)_i)}{N}: range:[0,max(B(y, \hat(y)))]
     res[["MBen"]] <- MBenRegress(trues, preds, util.vals)
   }
   
   if("NMUtil" %in% metrics){ 
     # NMU= \frac{(\sum_{i=1}^N(U(y_i, \hat(y_i))))+N}{2N}: range:[0,1]
     res[["NMUtil"]] <- NMUtilRegress(trues, preds, util.vals)
   }
   
   if("NMCost" %in% metrics){ 
     # NMC= \frac{(\sum_{i=1}^N(C(y_i, \hat(y_i))))}{N+max(C(y_i, \hat(y_i)))}: range:[0,1]
     res[["NMCost"]] <- NMCostRegress(trues, preds, util.vals, maxC)
   }
   
   if("NMBen" %in% metrics){ 
     # NMB= \frac{(\sum_{i=1}^N(B(y_i, \hat(y_i))))}{N+max(B(y_i, \hat(y_i)))}: range:[0,1]
     res[["NMBen"]] <- NMBenRegress(trues, preds, util.vals, maxB)
   }
   
   if("precPhi" %in% metrics){
     # pre_\phi=\frac{sum_{\forall \hat{y}_i>thr} (1+U(y_i, \hat{y}_i))}
     #               {sum_{\forall \hat{y}_i>thr} (1+\phi(\hat{y}_i))}
     res[["precPhi"]] <- precPhiRegress(trues, preds, util.vals, thr, control.parms)
   }
   
   if("recPhi" %in% metrics){
     # rec_\phi=\frac{sum_{\forall y_i>thr} (1+U(y_i, \hat{y}_i))}
     #               {sum_{\forall y_i>thr} (1+\phi(y_i))}
     res[["recPhi"]] <- recPhiRegress(trues, preds, util.vals, thr, control.parms)
   }
   
   if("FPhi" %in% metrics){
     # F_\phi\beta=(1+beta^2)*frac{prec*rec}{beta^2*prec+rec}
     res[["FPhi"]] <- FPhiRegress(trues, preds, util.vals, thr, beta, control.parms)
   }
   
   res
}
 
 totUtilRegress <- function(trues, preds, util.vals){
   res <- sum(util.vals)
   res
 }
 
 MUtilRegress <- function(trues, preds, util.vals){
    N <- length(trues)
   res <- sum(util.vals)/N
   res
 }
 
 
 NMUtilRegress <- function(trues, preds, util.vals){
   N <- length(trues)
   res <- (sum(util.vals)+N)/(2*N)
   res
 }
 
 
 
 totCostRegress <- function(trues, preds, util.vals){
   res <- sum(util.vals)
   res
 }
 
 
 MCostRegress <- function(trues, preds, util.vals){
   N <- length(trues)
   res <- sum(util.vals)/N
   res
 }
 
 NMCostRegress <- function(trues, preds, util.vals, maxC){
   N <- length(trues)
   res <- sum(util.vals)/(N*maxC)
   res
 }
 
 
 totBenRegress <- function(trues, preds, util.vals){
   res <- sum(util.vals)
   res
 }
 
 MBenRegress <- function(trues, preds, util.vals){
   N <- length(trues)
   res <- sum(util.vals)/N
   res
 }
 
 
 NMBenRegress <- function(trues, preds, util.vals, maxB){
   N <- length(trues)
   res <- sum(util.vals)/(N*maxB)
   res
 }

 precPhiRegress <- function(trues, preds, util.vals, thr, control.parms){
   imp.ev <- which(phi(preds, control.parms)>thr)
   res <- sum(1+util.vals[imp.ev])/sum(1+phi(preds[imp.ev], control.parms))
   res
 }
 
 recPhiRegress <- function(trues, preds, util.vals, thr, control.parms){
   imp.ev <- which(phi(trues, control.parms)>thr)
   res <- sum(1+util.vals[imp.ev])/sum(1+phi(preds[imp.ev], control.parms))
   res
 }
 
 FPhiRegress <- function(trues, preds, util.vals, thr, beta, control.parms){
   rec <- recPhiRegress(trues, preds, util.vals, thr, control.parms)
   prec <- precPhiRegress(trues, preds, util.vals, thr, control.parms)
   res <- (1+beta^2)*(prec*rec)/(beta^2*prec + rec)
   res
 }
 