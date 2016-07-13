\name{NCLClassif}
\alias{NCLClassif}

\title{
Neighborhood Cleaning Rule (NCL) algorithm for multiclass imbalanced problems
}
\description{
This function handles imbalanced classification problems using the Neighborhood Cleaning Rule (NCL) method. 
}
\usage{
NCLClassif(form, dat, k = 3, dist = "Euclidean", p = 2, Cl = "smaller")
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original imbalanced data set.
  }
  \item{k}{
    A number indicating the number of nearest neighbors to use.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors.  See the details. Defaults to "Euclidean".
  }
  \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen. Only necessary to define if a "p-norm" is chosen in the \code{dist} argument. See details.
  }
  \item{Cl}{
   A character vector indicating which classes should be under-sampled. Defaults to "smaller" meaning that all "smaller"" classes are the most important and therefore only examples from the remaining classes should be removed. The user may define a subset of the existing classes in which this technique will be applied.
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
    \item{NCL algorithm:}{The NCL algorithm includes two phases. In the first phase the ENN algorithm is used to under-sample the examples whose class label is not in Cl. Then, a second step is performed which aims at further clean the neighborhood of the examples in Cl. To achieve this, the k nearest neighbors of examples in Cl are scanned. An example is removed if all the previous neighbors have a class label which is not in Cl, and if the example belongs to a class which is larger than half of the smaller class in Cl. In either steps the examples with class labels in Cl are always maintained.
}
}
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the NCL
  algorithm. 
}
\references{J. Laurikkala. (2001). \emph{Improving identification of difficult small classes by balancing class distribution}. Artificial Intelligence in Medicine, pages 63-66.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\seealso{
\code{\link{ENNClassif}}
}
\examples{
# generate a small imbalanced data set
ir <- iris[-c(90:135), ]
# apply NCL method with different metrics, number of neighbors and classes
ir.M1 <- NCLClassif(Species~., ir, k = 3, dist = "Manhattan", Cl = "smaller")
ir.Def <- NCLClassif(Species~., ir)
ir.Ch <- NCLClassif(Species~., ir, k = 7, dist = "Chebyshev", Cl = "virginica")
ir.Eu <- NCLClassif(Species~., ir, k = 5, Cl = c("versicolor", "virginica"))
# check the results
summary(ir$Species)
summary(ir.M1$Species)
summary(ir.Def$Species)
summary(ir.Ch$Species)
summary(ir.Eu$Species)
}

\keyword{pre-processing classification}

