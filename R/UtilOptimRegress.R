# function for obtaining the optim prediction of a regression task according to
# a utility surface provided by the user and using Eibe and Markus paper for 
# obtaining a conditional density estimation

# Example using a utility surface:
# data(Boston, package = "MASS")
# 
# form <- as.formula(medv~.)
# 
# tgt <- which(colnames(Boston) == "medv")
# sp <- sample(1:nrow(Boston), as.integer(0.7*nrow(Boston)))
# train <- Boston[sp,]
# test <- Boston[-sp,]
# 
# control.parms <- phi.control(Boston[,tgt], method="extremes", extr.type="both")
# # the boundaries of the domain considered
# minds <- min(train[,tgt])
# maxds <- max(train[,tgt])
# 
# # build m.pts to include at least (minds, maxds) and (maxds, minds) points
# # m.pts must only contain points in [minds, maxds] range.
# m.pts <- matrix(c(minds, maxds, -1, maxds, minds, -1),
#                 byrow=TRUE, ncol=3)
# 
# pred.res <- UtilOptimRegress(form, train, test, type = "util", strat = "interpol",
#                              strat.parms=list(method = "bilinear"),
#                              control.parms = control.parms,
#                              m.pts = m.pts, minds = minds, maxds = maxds)
# 
# # check the predictions
# plot(test$medv, pred.res$optim)
# # assess the performance
# eval.util <- EvalRegressMetrics(test$medv, pred.res$optim, pred.res$utilRes,
#                                 thr=0.8, control.parms = control.parms)
# #
# # train a normal model
# model <- randomForest(form,train)
# normal.preds <- predict(model, test)
# 
# #obtain the utility of the new points (trues, preds)
# NormalUtil <- UtilInterpol(test$medv, normal.preds, control.parms = control.parms,
#                            minds, maxds, m.pts, method=method)
# #check the performance
# eval.normal <- EvalRegressMetrics(test$medv, normal.preds, NormalUtil,
#                                   thr=0.8, control.parms = control.parms)
# 
# plot(test$medv, normal.preds)
# points(test$medv, pred.res$optim, col="blue")

UtilOptimRegress <- function(form, train, test,
                             type = "util", strat = "interpol", 
                             strat.parms = list(method = "bilinear"),
                             control.parms, m.pts,
                             minds, maxds, eps = 0.1){
  #   inputs:
  #   form           a formula
  #   train          the train data
  #   test           the test data
  #   type           the type of surface provided. Can be: "util"(default), "cost" 
  #                  or "ben".
  #   strat
  #   strat.parms
  #   control.parms  the control.parms defined through the function phi.control
  #                  these parameters stablish the diagonal of the surface provided.
  #   m.pts         a 3-column matrix with interpolating points for the cases 
  #                 where y != \hat{y}, provided by the user. The first column
  #                 has the y value, the second column the \hat{y} value and the
  #                 third column has the corresponding utility value. The domain
  #                 boundaries of (y, \hat{y}) must be provided.
  #   minds         the lower bound of the target variable considered
  #   maxds         the upper bound of the target variable considered
  #   eps           a value for the precision considered during the pdf. 
  #
  #   output:
  #   the predictions for the test data optimized using the surface provided
  
  type <- match.arg(type, c("utility", "cost", "benefit"))
  strat <- match.arg(strat, c("interpol", "automatic")) # only interpol implemented for now
  if (strat != "interpol") stop("UBL::Only interpolation is available for now as strat parameter",
                                call. = FALSE)
  tgt <- which(names(train) == as.character(form[[2]]))
  
  if (is.null(minds)){
    minds <- min(train[,tgt])
  }
  if(is.null(maxds)){
    maxds <- max(train[,tgt])
  }
  
  y.true <- seq(minds-0.01, maxds+0.01, by=eps)
  if(y.true[length(y.true)]!=maxds) y.true <- c(y.true, maxds)
  
  if(strat == "interpol"){
    if (length(strat.parms) != 1){
      stop("strat.parms should only provide the method selected for interpolation. 
           No further arguments are necessary.", call. = FALSE)
    }
    method <- match.arg(strat.parms[[1]], c("bilinear", "splines", "idw", "krige"))
    # UtilRes is a lxl matrix with the true utility values on the rows and the
    # predictions on the columns, i.e., resUtil[a,b] provides the utility of
    # predicting b for a true value a.
    UtilRes <- UtilInterpol(NULL, NULL, type, control.parms, 
                          minds, maxds, m.pts, 
                          method = method, visual = FALSE, eps = eps,
                          full.output = TRUE)
  } else if(strat == "auto"){
    # baseseq <- seq(minds-0.01, maxds+0.01, by=eps)
    # if(baseseq[length(baseseq)]!=maxds) baseseq <- c(baseseq, maxds)
    
  }

  resPDF <- getPDFinRange(y.true, test, train, form)
  
  
  optim <- vector("numeric", length=nrow(test))
  for (ex in 1:nrow(test)){
    
    areas <- vector("numeric",length=length(y.true))
    for (case in 1:length(y.true)){
      prod <- resPDF[ex,]*UtilRes[,case]
      idx <- 2:length(y.true)
      areas[case] <- as.double((y.true[idx] - y.true[idx-1]) %*% (prod[idx] + prod[idx-1])) / 2
    }
    if(type == "utility" || type == "benefit"){
      optim[ex] <- y.true[which.max(areas)]
    } else {
      optim[ex] <- y.true[which.min(areas)]
    }
#    print(ex)
  }
 
  #obtain the utility values for the points (test, optim) determined 
  
  utilRes <- UtilInterpol(test[,tgt], optim, type, control.parms, minds, maxds, m.pts, 
                          method = method, visual = FALSE, eps = eps)

  list(optim=optim, utilRes=utilRes)
  
}