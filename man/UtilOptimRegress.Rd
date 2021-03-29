\name{UtilOptimRegress}
\alias{UtilOptimRegress}

\title{
Optimization of predictions utility, cost or benefit for regression problems.
}
\description{
This function determines the optimal predictions given a utility, cost or benefit surface. This surface is obtained through a specified strategy with some parameters. For determining the optimal predictions an estimation of the conditional probability density function is performed for each test case.
If the surface provided is of type utility or benefit a maximization process is carried out. If the user provides a cost surface, then a minimization is performed.
}
\usage{
UtilOptimRegress(form, train, test, type = "util", strat = "interpol", 
                 strat.parms = list(method = "bilinear"), control.parms, m.pts,
                 minds, maxds, eps = 0.1)
}

\arguments{
  \item{form}{    
  A formula describing the prediction problem.
  }
  \item{train}{
  A data.frame with the training data.
  }
  \item{test}{
  A data.frame with the test data.
  }
  \item{type}{
  A character specifying the type of surface provided. Can be one of: "utility",
  "cost" or "benefit". Defaults to "utility".
  }
  \item{strat}{
  A character determining the strategy for obtaining the surface of the problem. 
  For now, only the interpolation strategy is available (the default).  
  }
  \item{strat.parms}{
  A named list containing the parameters necessary for the strategy previously specified.
  For the interpolation strategy (the default and only strategy available for now),
  it is required that the user specifies wich method sould be used for interpolating the points.
  }
  \item{control.parms}{
  A named list with the control.parms defined through the function \link{phi.control}.
  These parameters stablish the diagonal of the surface provided. If the type of
  surface defined is "cost" this parameter can be set to NULL, because in this 
  case we assume that the accurate prediction, i.e., points in the diagonal of 
  the surface have zero cost. See examples.
  }
  \item{m.pts}{
  A matrix with 3-columns, with interpolation points specifying the utility, cost
  or benefit of the surface. The points sould be in the off-diagonal of the 
  surface, i.e., the user should provide points where y != y.pred.
  The first column must have the true value (y), the second column the corresponding
  prediction (y.pred) and the third column sets the utility cost or benefit of that point 
  (y, y.pred). The user should define as many points as possible. The minimum number of 
  required points are two. More specifically, the user must always set the surface 
  values of at least the points (minds, maxds) and (maxds, minds). See minds and 
  maxds description.
  }
  \item{maxds}{
  The numeric upper bound of the target variable considered.
  }
  \item{minds}{
  The numeric lower bound of the target variable considered.
  }
  \item{eps}{
  Numeric value for the precision considered during the interpolation. Defaults to 0.1. 
  }
}
\details{
The optimization process carried out by this function uses a method for conditional density estimation proposed by Rau M.M et al.(2015). Code for conditional density estimation (available on github https://github.com/MarkusMichaelRau/OrdinalClassification) kindly contributed by M. M. Rau with changes made by P.Branco.
The optimization is achieved generalizing the method proposed by Elkan (2001) for classification tasks. In regression, this process involves determining, for each test case, the maximum integral (for utility or benefit surfaces, or the minimum if we have a cost surface) of the product of the conditional density function estimated and either the utility, the benefit or the cost surface.
The optimal prediction for a case q is given by:
\eqn{y^{*}(q)=argmax[z] \int pdf(y|q).U(y,z) dy},
where pdf(y|q) is the conditional densitiy estimation for case q, and U(y,z) is the
utility, benefit or cost surface evaluated on the true value y and predictied value z.
}

\value{
  The function returns a vector with the predictions for the test data optimized
  using the surface provided.
}
\references{
Rau, M.M., Seitz, S., Brimioulle, F., Frank, E., Friedrich, O., Gruen, D. and Hoyle, B., 2015. Accurate photometric redshift probability density estimation-method comparison and application. Monthly Notices of the Royal Astronomical Society, 452(4), pp.3710-3725.

Elkan, C., 2001, August. The foundations of cost-sensitive learning. In International joint conference on artificial intelligence (Vol. 17, No. 1, pp. 973-978). LAWRENCE ERLBAUM ASSOCIATES LTD.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{phi.control}, \link{UtilOptimClassif}, \link{UtilInterpol}}
}
\examples{
\dontrun{
#Example using a utility surface:
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

eval.util <- EvalRegressMetrics(test$medv, pred.res$optim, pred.res$utilRes,
                                 thr=0.8, control.parms = control.parms)

# train a normal model
model <- randomForest(medv~.,train)
normal.preds <- predict(model, test)

#obtain the utility of the new points (trues, preds)
NormalUtil <- UtilInterpol(test$medv, normal.preds, type="util", 
                           control.parms = control.parms,
                           minds, maxds, m.pts, method = "bilinear")
#check the performance
eval.normal <- EvalRegressMetrics(test$medv, normal.preds, NormalUtil,
                                  thr=0.8, control.parms = control.parms)

#check both results
eval.util
eval.normal


#check visually both predictions and the surface used
UtilInterpol(test$medv, normal.preds, type = "util", control.parms = control.parms,
                           minds, maxds, m.pts, method = "bilinear", visual=TRUE)

points(test$medv, normal.preds, col="green")
points(test$medv, pred.res$optim, col="blue")

# another example now using points interpolation with splines
if (requireNamespace("DMwR2", quietly = TRUE)){

data(algae, package ="DMwR2")
ds <- data.frame(algae[complete.cases(algae[,1:12]), 1:12])
tgt <- which(colnames(ds) == "a1")
sp <- sample(1:nrow(ds), as.integer(0.7*nrow(ds)))
train <- ds[sp,]
test <- ds[-sp,]
  
control.parms <- phi.control(ds[,tgt], method="extremes", extr.type="both")

# the boundaries of the domain considered
minds <- min(train[,tgt])
maxds <- max(train[,tgt])

# build m.pts to include at least (minds, maxds) and (maxds, minds) points
m.pts <- matrix(c(minds, maxds, -1, maxds, minds, -1),
                byrow=TRUE, ncol=3)

pred.res <- UtilOptimRegress(a1~., train, test, type = "util", strat = "interpol",
                             strat.parms=list(method = "splines"),
                             control.parms = control.parms,
                             m.pts = m.pts, minds = minds, maxds = maxds)

# check the predictions
plot(test$a1, pred.res$optim)
# assess the performance
eval.util <- EvalRegressMetrics(test$a1, pred.res$optim, pred.res$utilRes,
                                thr=0.8, control.parms = control.parms)
#
# train a normal model
model <- randomForest(a1~.,train)
normal.preds <- predict(model, test)

#obtain the utility of the new points (trues, preds)
NormalUtil <- UtilInterpol(test$medv, normal.preds, type = "util", 
                           control.parms = control.parms,
                           minds, maxds, m.pts, method="splines")
#check the performance
eval.normal <- EvalRegressMetrics(test$medv, normal.preds, NormalUtil,
                                  thr=0.8, control.parms = control.parms)

eval.util
eval.normal

# observe the utility surface with the normal preds
UtilInterpol(test$a1, normal.preds, type="util", control.parms = control.parms,
             minds, maxds, m.pts, method="splines", visual=TRUE) 
# add the optim preds
points(test$a1, pred.res$optim, col="green")
}

# Example using a cost surface:
data(Boston, package = "MASS")

tgt <- which(colnames(Boston) == "medv")
sp <- sample(1:nrow(Boston), as.integer(0.7*nrow(Boston)))
train <- Boston[sp,]
test <- Boston[-sp,]

# if using interpolation methods for COST surface, the control.parms can be set to NULL
# the boundaries of the domain considered
minds <- min(train[,tgt])
maxds <- max(train[,tgt])

# build m.pts to include at least (minds, maxds) and (maxds, minds) points
m.pts <- matrix(c(minds, maxds, 5, maxds, minds, 20),
                byrow=TRUE, ncol=3)

pred.res <- UtilOptimRegress(medv~., train, test, type = "cost", strat = "interpol",
                             strat.parms = list(method = "bilinear"),
                             control.parms = NULL,
                             m.pts = m.pts, minds = minds, maxds = maxds)

# check the predictions
plot(test$medv, pred.res$optim)

# assess the performance
eval.util <- EvalRegressMetrics(test$medv, pred.res$optim, pred.res$utilRes,
                                type="cost", maxC = 20)
#
# train a normal model
model <- randomForest(medv~.,train)
normal.preds <- predict(model, test)

#obtain the utility of the new points (trues, preds)
NormalUtil <- UtilInterpol(test$medv, normal.preds, type="cost", control.parms = NULL,
                           minds, maxds, m.pts, method="bilinear")
#check the performance
eval.normal <- EvalRegressMetrics(test$medv, normal.preds, NormalUtil,
                                  type="cost", maxC = 20)

eval.normal
eval.util

# check visually the surface and the predictions
UtilInterpol(test$medv, normal.preds, type="cost", control.parms = NULL,
                           minds, maxds, m.pts, method="bilinear",
                           visual=TRUE)

points(test$medv, pred.res$optim, col="blue")

}
}
\keyword{utility optimization}

