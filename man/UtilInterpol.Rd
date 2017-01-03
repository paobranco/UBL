\name{UtilInterpol}
\alias{UtilInterpol}

\title{
Utility surface obtained through methods for spatial interpolation of points. 
}
\description{
This function uses spatial interpolation methods for obtaining the utility surface. 
It depends on a set of points provided by the user and on a method selected for 
interpolation. The available interpolation methods are: bilinear, splines, idw and krige. Check the details section for more on these methods.
}
\usage{
UtilInterpol(trues, preds, type = c("utility", "cost", "benefit"), control.parms,
            minds, maxds, m.pts, method = c("bilinear", "splines", "idw", "krige"),
            visual = FALSE, eps = 0.1, full.output = FALSE)
}

\arguments{
  \item{trues}{
  A vector of true target variable values. Can be NULL. See details section.
  }
  \item{preds}{
  A vector with corresponding predicted values for the trues provided. Can be NULL.
  See details section.
  }
  \item{type}{
  A character specifying the type of surface that is being interpolated. 
  It can be set to either "utility", "cost" or "benefit".
  When set to "cost" we assume that the diagonal of the surface (where y=y.pred) is zero.
  Therefore, in this case, the user doesn't need to set the control.parms parameter.
  }
  \item{control.parms}{ 
  These parameters are necessary for utility and benefit surfaces. control.parms 
  can be obtained with a call to function \link{phi.control}. This provides a list 
  with the parameters used for defining the relevance function \link{phi}.
  The points provided through these parameters are used for interpolating the 
  utility surface because the relevance function matches the diagonal of the
  utility, i.e., the relevance function \link{phi} corresponds to the utility of
  accurate predictions (y = y.pred). Alternatively, the user may build the control.parms list.
  When the user selects a cost surface, control.parms can simply be NULL. In this case,
  we assume that the surface diagonal is zero. If control.parms are not NULL, then
  specified points are used. See examples section.
  }
  \item{minds}{
  The lower bound of the target variable considered for interpolation. A new minds value may be necessary when trues and/or preds provided have lower values than minds. This is handled by extrapolation and a warning is issued (see details).
  }
  \item{maxds}{
  The upper bound of the target variable considered for interpolation. A new maxds value may be necessary when trues and/or preds provided have values higher than maxds. This is handled by extrapolation and a warning is issued (see details).
  }
  \item{m.pts}{
  A 3-column matrix with interpolating points for the off-diagonal cases
  (i.e., y != y.pred), provided by the user. The first column
  has the y value, the second column the y.pred value and the
  third column has the corresponding utility value. At least, the off diagonal
  domain boundary points (i.e., points (minds, maxds, util) and 
  (maxds, minds, util) ) must be provided in this matrix. Moreover, the points
  provided through this parameter must be in [minds, maxds] range.
  }
  \item{method}{
  A character indicating which interpolation method should be used. Can be one of: "bilinear", "splines", "idw" or "krige". See details section for a description of the available methods.
  }
  \item{visual}{
  Logical. If TRUE a plot of the utility surface isometrics obtained and
  the points provided is displayed. If FALSE (the default) no image is plotted.
  }
  \item{eps}{
  Numeric value for the precision considered during the interpolation. Defaults to 0.1. 
  Only relevant if a plot is displayed, or when full.output is set to TRUE. See details section.
  }
  \item{full.output}{Logical. If FALSE (the default) only the results from points provided through
  parameters trues and preds are returned. If TRUE a matrix with the utility
  of all points in domain (considering the eps provided) are returned. See details section.
  }
}
\details{
  \describe{
    \item{\code{method} parameter:}{The parameter \code{method} allows the user to select from a set of interpolation methods. The available methods are as follows:
    
    - \code{bilinear}: local fitting of a polynomial surface of degree 1 obtained through loess function of stats package.
    
    - \code{splines}: multilevel B-splines interpolation method obtained through MBA R package.

    - \code{idw}: inverse distance weighted interpolation obtained through R package gstat.
    
    - \code{krige}: automatic kriging obtained using automap R package. 
    
}
  \item{extrapolation:}{ when trues or preds provided are outside the range [minds, maxds] the function   performs an extrapolation of the domain. To achieve this, four new points are added that extend the initial target variable domain ([minds, maxds]). This extrapolation is performed as follows:

    - first: determine inc.fac, the distance necessary to increase (the largest value needed increase the axes to include all trues and preds provided);
    
    - second: define the new target variable domain ([minds - inc.fac, maxds + inc.fac]);
    
    - third: add two new diagonal points evaluating the relevance function on these new points
    (i.e. add (minds-inc.fac, minds-nc.fac, phi(minds-inc.fac, minds.inc.fac)) and
    (maxds+inc.fac, maxds+inc.fac, phi(maxds+inc.fac, maxds+inc.fac)));

    - fourth: add two new off-diagonal points using the new min and max values of
    the domain and the utility provided by the user for the two mandatory 
    points (minds, maxds) and (maxds, minds).
    
  In order to avoid this extrapolation, the user must ensure that the values provided in trues and preds vectors are inside the [minds, maxds] range provided.
}
  \item{\code{full.output} parameter:}{This parameter is used to select which 
  utility values are returned. There are two options for this parameter:
  
  - FALSE: This means that the user is only interested in obtaining the utility
  surface values of some points (y, y.pred). In this case, the y and y.pred should be
  provided through parameters trues and preds and the function returns a vector
  with the utility for the corresponding points.
  
  - TRUE: The user is interested in obtaining the utility surface values on a 
  grid of equally spaced values of the target variable domain. In this case, there
  is no need for specifying parameters trues and preds, because the goal is not 
  to observe the utility of these points. Parameters trues and preds can be set 
  to NULL in this case. The function returns a lXl matrix with the utility of
  all points in a grid defined as follows. The l equally spaced points are a 
  sequence that starts at minds-0.01, ends at maxds+0.01 and are incremented by
  eps value.
  
}
}
}
\value{
  The function returns a vector with utility of the points provided through the vectors trues and preds.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{phi.control}, \link{UtilOptimRegress}}
}
\examples{
\dontrun{
# examples with a utility surface
data(Boston, package = "MASS")

tgt <- which(colnames(Boston) == "medv")
sp <- sample(1:nrow(Boston), as.integer(0.7*nrow(Boston)))
train <- Boston[sp,]
test <- Boston[-sp,]

control.parms <- phi.control(Boston[,tgt], method="extremes", extr.type="both")
# the boundaries of the domain considered
minds <- min(Boston[,tgt])-5
maxds <- max(Boston[,tgt])+5

# build m.pts to include at least the utility of the
# points (minds, maxds) and (maxds, minds)
m.pts <- matrix(c(minds, maxds, -1, maxds, minds, 0),
                byrow=TRUE, ncol=3)

trues <- test[,tgt]
library(randomForest)
model <- randomForest(medv~., train)
preds <- predict(model, test)

resLIN <- UtilInterpol(trues, preds, type="util", control.parms, minds, maxds, m.pts,
                         method = "bilinear", visual=TRUE)
resIDW <- UtilInterpol(trues, preds, type="util", control.parms, minds, maxds, m.pts,
                        method = "idw", visual=TRUE)
resSPL <- UtilInterpol(trues, preds, type="util", control.parms, minds, maxds, m.pts,
                        method = "spl", visual=TRUE)
resKRIGE <- UtilInterpol(trues, preds, type="util", control.parms, minds, maxds, m.pts,
                          method = "krige", visual=TRUE)

# examples with a cost surface
data(Boston, package = "MASS")

tgt <- which(colnames(Boston) == "medv")
sp <- sample(1:nrow(Boston), as.integer(0.7*nrow(Boston)))
train <- Boston[sp,]
test <- Boston[-sp,]

# the boundaries of the domain considered
minds <- min(Boston[,tgt])-5
maxds <- max(Boston[,tgt])+5

# build m.pts to include at least the utility of the
# points (minds, maxds) and (maxds, minds)
m.pts <- matrix(c(minds, maxds, 5, maxds, minds, 20),
                byrow=TRUE, ncol=3)

trues <- test[,tgt]

# train a model and predict on test set
library(randomForest)
model <- randomForest(medv~., train)
preds <- predict(model, test)

costLIN <- UtilInterpol(trues, preds, type="cost", control.parms=NULL, minds, maxds, m.pts,
                         method = "bilinear", visual=TRUE )

costSPL <- UtilInterpol(trues, preds, type="cost", control.parms=NULL, minds, maxds, m.pts,
                        method = "spl", visual=TRUE)

costKRIGE <- UtilInterpol(trues, preds, type="cost", control.parms=NULL, minds, maxds, m.pts,
                          method = "krige", visual=TRUE)

costIDW <- UtilInterpol(trues, preds, type="cost", control.parms=NULL, minds, maxds, m.pts,
                        method = "idw", visual=TRUE)


# if the user has a cost matrix and wants to specify the control.parms:
my.pts <- matrix(c(0, 0, 0, 10, 0, 0, 20, 0, 0, 45, 0, 0), byrow=TRUE, ncol=3)
control.parms <- phi.control(trues, method="range", control.pts = my.pts)

costLIN <- UtilInterpol(trues, preds, type="cost", control.parms=control.parms,
                        minds, maxds, m.pts, method = "bilinear", visual=TRUE )


# first trues and preds
trues[1:5]
preds[1:5]
trues[1:5]-preds[1:5]

# first cost results on these predictions for cost surface costIDW
costIDW[1:5]
# a summary of these prediction costs:
summary(costIDW)

#example with a benefit surface

# define control.parms either by defining a list with 3 named elements 
# or by calling phi.control function with method range and passing 
# the selected control.pts
control.parms <- list(method="range", npts=5,
                      control.pts=c(0,1,0,10,5,0.5,20,10,0.5,30,30,0,50,30,0))
m.pts <- matrix(c(minds, maxds, 0, maxds, minds, 0),
                byrow=TRUE, ncol=3)
                
benLIN <- UtilInterpol(trues, preds, type="ben", control.parms, minds, maxds, m.pts,
                       method = "bilinear", visual=TRUE)
benIDW <- UtilInterpol(trues, preds, type="ben", control.parms, minds, maxds, m.pts,
                       method = "idw", visual=TRUE)
benSPL <- UtilInterpol(trues, preds, type="ben", control.parms, minds, maxds, m.pts,
                       method = "spl", visual=TRUE)
benKRIGE <- UtilInterpol(trues, preds, type="ben", control.parms, minds, maxds, m.pts,
                         method = "krige", visual=TRUE)
}
}
\keyword{utility surface}

