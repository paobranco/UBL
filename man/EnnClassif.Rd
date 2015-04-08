\name{EnnClassif}
\alias{EnnClassif}

\title{
Edited Nearest Neighbour for imbalanced classification problems
}
\description{
This function handles imbalanced classification problems using the Edited Nearest Neighbour (ENN) algorithm. It removes examples whose class label differs from the class of at least half of its k nearest neighbours. All the existing classes can be undersampled with this technique. Alternatively a subset of classes to undersample can be provided by the user.
}
\usage{
EnnClassif(form, data, k=3, dist="Euclidean", p=2, use.at="all", Cl="all")
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{data}{
    A data frame containing the original (imbalanced) data set.
  }
  \item{k}{
    A number indicating the number of nearest neighbours to use.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbours. 
  }
  \item{p}{
    A number indicating the value of p if the "p-norm" distance is choosen.
  }
  \item{use.at}{
    A character string indicating which attribute types should be used. Defaults to "all" meaning that all the attributes should be used in the distance computation. Alternatively, this parameter can be set to "numeric" or "nominal". 
  }
  \item{Cl}{
   A character vector indicating which classes should be undersampled. Defaults to "all" meaning that all classes are candidates for having examples removed. The user may define a subset of the existing classes in which this technique will be applied. 
  }

}
\details{
The ENN algorithm uses a cleaning method to perform undersampling. For each example with class label in Cl the k nearest neighbours are computed using a selected distance metric. The example is removed from the data set if it is misclassified by at least half of it's k nearest neighbours. Usually this algorithm uses k=3.
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
  ir<- iris[-c(95:130),]
# use ENN technique with different metrics, number of neighbours and classes
  ir1norm <- EnnClassif(Species~., ir, k=5, dist="p-norm", p=1, Cl="all")
  irEucl <- EnnClassif(Species~., ir)
  irCheby <- EnnClassif(Species~., ir, k=7, dist="Chebyshev", Cl=c("virginica", "setosa"))
  irHVDM <- EnnClassif(Species~., ir, k=3, dist="HVDM")
  summary(ir1norm[[1]])
  summary(irEucl[[1]])
  summary(irCheby[[1]])
  summary(irHVDM[[1]])
}

\keyword{models}

