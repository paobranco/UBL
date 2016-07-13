\name{ENNClassif}
\alias{ENNClassif}

\title{
Edited Nearest Neighbor for multiclass imbalanced problems
}
\description{
This function handles imbalanced classification problems using the Edited Nearest Neighbor (ENN) algorithm. It removes examples whose class label differs from the class of at least half of its k nearest neighbors. All the existing classes can be under-sampled with this technique. Alternatively a subset of classes to under-sample can be provided by the user.
}
\usage{
ENNClassif(form, dat, k = 3, dist = "Euclidean", p = 2, Cl = "all")
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original (imbalanced) data set.
  }
  \item{k}{
    A number indicating the number of nearest neighbors to use.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors. See the details. Defaults to "Euclidean".
  }
  \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen. Only necessary to define if a "p-norm" is chosen in the \code{dist} argument. See details.
  }
  \item{Cl}{
   A character vector indicating which classes should be under-sampled. Defaults to "all" meaning that all classes are candidates for having examples removed. The user may define a subset of the existing classes in which this technique will be applied. 
  }

}
\details{
  \describe{
    \item{\code{dist} parameter:}{The parameter \code{dist} allows the user to define the distance metric to be used in the neighbors computation. Although the default is the Euclidean distance, other metrics are available. This allows the computation of distances in data sets with, for instance, both nominal and numeric features. The options available for the distance functions are as follows: 
    
    - for data with only numeric features: "Manhattan", "Euclidean", "Canberra", "Chebyshev", "p-norm";
    
    - for data with only nominal features: "Overlap";
    
    - for dealing with both nominal and numeric features: "HEOM", "HVDM".
    
    When the "p-norm" is selected for the \code{dist} parameter, it is also necessary to define the value of parameter \code{p}. The value of parameter \code{p} sets which "p-norm" will be used. For instance, if \code{p} is set to 1, the "1-norm" (or Manhattan distance) is used, and if \code{p} is set to 2, the "2-norm" (or Euclidean distance) is applied.
    For more details regarding the distance functions implemented in UBL package please see the package vignettes.
    }
    \item{ENN algorithm:}{The ENN algorithm uses a cleaning method to perform under-sampling. For each example with class label in Cl the k nearest neighbors are computed using a selected distance metric. The example is removed from the data set if it is misclassified by at least half of it's k nearest neighbors. Usually this algorithm uses k=3.
}
}
}
\value{
  The function returns a list containing a data frame with
  the new data set resulting from the application of the ENN
  algorithm, and the indexes of the examples removed. 
}
\references{
D. Wilson. (1972). \emph{Asymptotic properties of nearest neighbor rules using edited data}. Systems, Man and Cybernetics, IEEE Transactions on, 408-421.
}

\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\seealso{
\code{\link{NCLClassif}}
}

\examples{

# generate an small imbalanced data set
  ir<- iris[-c(95:130), ]
# use ENN technique with different metrics, number of neighbours and classes
  ir1norm <- ENNClassif(Species~., ir, k = 5, dist = "p-norm", 
                        p = 1, Cl = "all")
  irEucl <- ENNClassif(Species~., ir) # defaults to Euclidean distance
  irCheby <- ENNClassif(Species~., ir, k = 7, dist = "Chebyshev",
                       Cl = c("virginica", "setosa"))
  irHVDM <- ENNClassif(Species~., ir, k = 3, dist = "HVDM")
# checking the impact
  summary(ir$Species)
  summary(ir1norm[[1]]$Species)
  summary(irEucl[[1]]$Species)
  summary(irCheby[[1]]$Species)
  summary(irHVDM[[1]]$Species)
# check the removed indexes of the ir1norm data set
  ir1norm[[2]]
}

\keyword{pre-processing classification}

