##############################################################################
## Functions that evaluate the utility/cost of each case given a probability 
## for each class prediction and choose the best class to predict, i.e.,
## the class that minimizes the cost or maximizes the utility.
## P.Branco Nov 2016
##############################################################################
# matrix provided always with trues in row and predictions in columns

# matU (example of a utility matrix)
# [,1] [,2] [,3]
# [1,]  0.2 -0.5 -0.3
# [2,] -1.0  1.0 -0.9
# [3,] -0.9 -0.8  0.9
# matC (example of a cost matrix)
# [,1] [,2] [,3]
# [1,]  0.0  0.5  0.3
# [2,]  1.0  0.0  0.9
# [3,]  0.9  0.8  0.0
# matB (example of a benefit matrix)
# [,1] [,2] [,3]
# [1,]  0.5    0  0.0
# [2,]  0.0    1  0.0
# [3,]  0.0    0  0.9
# 
# data(ImbC)
# sp <- sample(1:nrow(ImbC), round(0.7*nrow(ImbC)))
# # obtain an imbalanced distributions of Species
# train <- ImbC[sp, ]
# test <- ImbC[-sp,]
# 
# # example with a utility matrix
# # define a utility matrix (true class in rows and pred class in columns)
# matU <- matrix(c(0.2, -0.5, -0.3, -1, 1, -0.9, -0.9, -0.8, 0.9), byrow=TRUE, ncol=3)
# resUtil <- UtilOptimClassif(Class~., train, test, mtr = matU, type="util",
#                        learner = "naiveBayes", predictor.pars = list(type="raw", threshold = 0.01))
# #
# # # learning a model without maximizing utility
# model <- naiveBayes(Class~., train)
# resNormal <- predict(model, test, type="class", threshold = 0.01)
# # #Check the difference in the total utility of the results
# EvalClassifMetrics(test$Class, resNormal, mtr=matU, type= "util")
# EvalClassifMetrics(test$Class, resUtil, mtr=matU, type= "util")
#
# #example with a cost matrix
# # define a cost matrix (true class in rows and pred class in columns)
# matC <- matrix(c(0, 0.5, 0.3, 1, 0, 0.9, 0.9, 0.8, 0), byrow=TRUE, ncol=3)
# resUtil <- UtilOptimClassif(Class~., train, test, mtr = matC, type="cost",
#                            learner = "naiveBayes", predictor.pars = list(type="raw", threshold = 0.01))
# 
# # learning a model without maximizing utility
# model <- naiveBayes(Class~., train)
# resNormal <- predict(model, test, type="class")
# #Check the difference in the total utility of the results
# EvalClassifMetrics(test$Class, resNormal, mtr=matC, type= "cost")
# EvalClassifMetrics(test$Class, resUtil, mtr=matC, type= "cost")
#
#
#
# #example with a benefit matrix
# # define a benefit matrix (true class in rows and pred class in columns)
# matB <- matrix(c(0.2, 0, 0, 0, 1, 0, 0, 0, 0.9), byrow=TRUE, ncol=3)
# 
# resUtil <- UtilOptimClassif(Class~., train, test, mtr = matB, type="ben",
#                            learner = "naiveBayes", predictor.pars = list(type="raw", threshold = 0.01))
# 
# # learning a model without maximizing utility
# model <- naiveBayes(Class~., train)
# resNormal <- predict(model, test, type="class", threshold = 0.01)
# Check the difference in the total utility of the results
# EvalClassifMetrics(test$Class, resNormal, mtr=matB, type= "ben")
# EvalClassifMetrics(test$Class, resUtil, mtr=matB, type= "ben")
#

# table(test$Class,resNormal)
# table(test$Class,resUtil)

UtilOptimClassif <- function(form, train, test, mtr, type = "util",
                           learner = NULL, learner.pars=NULL, predictor="predict",
                           predictor.pars=NULL) {
  #   inputs:
  #   form           a formula
  #   train          the train data
  #   test           the test data
  #   mtr            either a cost matrix, a benefit matrix, a utility matrix 
  #                  or an example dependent matrix (last not implemented yet)
  #                  the matrix must be always provided with the true class in 
  #                  the rows and the predicted class in the columns
  #   type           the type of mtr provided. Can be: "util"(default), "cost",
  #                  "ben" or "ex" ("ex" still not implemented).
  #   learner        the learning algorithm to used
  #   learner.pars   the parameters passed through the learning algorithm
  #   predictor      the predictor used
  #   predictor.pars the predictor parameters 
  #
  #   output:
  #   the predictions for the test data optimized using the matrix provided
 
  
  tgt <- which(names(train) == as.character(form[[2]]))
  n.cl <- length(unique(train[,tgt])) 
  
  if(is.numeric(train$tgt)) stop("This function only deals with classification tasks.",
                                 call. = FALSE)
  
  if (is.null(predictor)) { # no separate prediction phase
    preds <- do.call(eval(parse(text = learner)),
                     c(list(form, train), learner.pars)) 
  } else { 
    model <- do.call(eval(parse(text = learner)),
                     c(list(form, data = train), learner.pars))
    
    preds <- do.call(eval(parse(text = predictor)),
                     c(list(model, test), predictor.pars))
  }
  
  if (is.null(dim(preds))) {
    stop ("Predictions must be provided with probabilities for each class.")
  }
  if(ncol(preds) != ncol(mtr)) stop("Number of classes in predictions probabilites
                                    and matrix provided do not match.", call. = FALSE)
  ps <- apply(preds,1,
              function(ps){ if(type == "util" || type == "ben"){ 
                              which.max(apply(mtr,2,function(cs) sum(ps*cs)))
                              } else if (type == "cost") {
                              which.min(apply(mtr,2,function(cs) sum(ps*cs)))
                              }
                          }
              )

  preds <- levels(train[[tgt]])[ps]
  
  Cls <- levels(as.factor(train[,tgt]))
  preds <- factor(preds, levels=Cls)
  
  preds
}


