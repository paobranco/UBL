\name{EvalRegressMetrics}
\alias{EvalRegressMetrics}

\title{Utility metrics for assessing the performance of utility-based regression tasks. 
}
\description{This function allows to evaluate utility-based metrics in regression problems which have defined a cost, benefit, or utility surface.
}
\usage{
EvalRegressMetrics(trues, preds, util.vals, type = "util",
metrics = NULL, thr = 0.5, control.parms = NULL, 
beta = 1, maxC = NULL, maxB = NULL)
}

\arguments{
  \item{trues}{A vector with the true target variable values of the problem.
  }
  \item{preds}{A vector with the prediction values obtained for the vector of trues.
  }
  \item{util.vals}{Either the cost, benefit or utility values corresponding to 
  the provided points (trues, preds).
  }
  \item{type}{
  A character specifying the type of surface under consideration. Can be set to "cost", 
  "benefit" or "utility" (the default).
  }
  \item{metrics}{A character vector with the metrics names to be evaluated. 
  If not specified (the default), all the metrics avaliable for the type of
  surface provided are evaluated.
  }
  \item{thr}{A numeric value between 0 and 1 setting a threshold on the
  relevance values for determining which are the important cases to consider.
  This threshold is only necessary for the following metrics: precPhi, recPhi
  and FPhi. Moreover, these metrics are only available for problems based on 
  utility surfaces. Defaults to 0.5.
  }
  \item{control.parms}{the control.parms of the relevance function phi. 
  Can be obtained through function \link{phi.control}.
  These are only necessary for evaluating the following utility metrics:
  recPhi, precPhi and FPhi.
  }
  \item{beta}{
  The numeric value of the beta parameter for F-score.
  }
  \item{maxC}{the maximum cost achievable in the cost surface. Parameter 
  only required when the problem depends on a cost surface.
  }
  \item{maxB}{the maximum Benefit achievable in the benefit surface. 
  Parameter only required when the problem depends on a benefit surface.
  }
}
\value{
The function returns a named list with the evaluated metrics results. 
}
\references{Ribeiro, R., 2011. Utility-based regression 
              (Doctoral dissertation, PhD thesis, 
              Dep. Computer Science, Faculty of Sciences - 
              University of Porto).
              
            Branco, P., 2014. Re-sampling Approaches for Regression Tasks under Imbalanced Domains 
            (Msc thesis, Dep. Computer Science, Faculty of Sciences - 
            University of Porto).
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{phi.control}}
}

\examples{
\dontrun{
#Example using a utility surface interpolated and observing the performance of 
# two models: i) a model obtained with a strategy designed for maximizing 
# predictions utility and a model obtained through a standard random Forest.

data(Boston, package = "MASS")

tgt <- which(colnames(Boston) == "medv")
sp <- sample(1:nrow(Boston), as.integer(0.7*nrow(Boston)))
train <- Boston[sp,]
test <- Boston[-sp,]

control.parms <- phi.control(Boston[,tgt], method="extremes", extr.type="both")
# the boundaries of the domain considered
minds <- min(train[,tgt])
maxds <- max(train[,tgt])

# build m.pts to include at least (minds, maxds) and (maxds, minds) points
# m.pts must only contain points in [minds, maxds] range.
m.pts <- matrix(c(minds, maxds, -1, maxds, minds, -1),
                byrow=TRUE, ncol=3)

pred.res <- UtilOptimRegress(medv~., train, test, type = "util", strat = "interpol",
                             strat.parms=list(method = "bilinear"),
                             control.parms = control.parms,
                             m.pts = m.pts, minds = minds, maxds = maxds)

# assess the performance
eval.util <- EvalRegressMetrics(test$medv, pred.res$optim, pred.res$utilRes,
                                thr = 0.8, control.parms = control.parms)

# now train a normal model
model <- randomForest(medv~.,train)
normal.preds <- predict(model, test)

#obtain the utility of this model predictions
NormalUtil <- UtilInterpol(test$medv, normal.preds, type = "util",
                           control.parms = control.parms,
                           minds, maxds, m.pts, method = "bilinear")

#check the performance
eval.normal <- EvalRegressMetrics(test$medv, normal.preds, NormalUtil,
                                  thr=0.8, control.parms = control.parms)

# 3 check visually the utility surface and the predictions of both models 
UtilInterpol(NULL,NULL, type = "util", control.parms = control.parms,
                           minds, maxds, m.pts, method = "bilinear", 
                           visual=TRUE, full.output = TRUE)
points(test$medv, normal.preds) # standard model predition points
points(test$medv, pred.res$optim, col="blue") # model with optimized predictions
}
}

\keyword{evaluation metrics}
