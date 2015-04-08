\name{NCLClassif}
\alias{NCLClassif}

\title{
Neighbourhood Cleaning Rule (NCL) algorithm for imbalanced classification problems
}
\description{
This function handles imbalanced classification problems using the Neighbourhood Cleaning Rule (NCL) method. 
}
\usage{
NCLClassif(form, data, k=3, dist="Euclidean", p=2, use.at="all", Cl="smaller")
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{data}{
    A data frame containing the original imbalanced data set.
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
   A character vector indicating which classes should be undersampled. Defaults to "smaller" meaning that all "smaller"" classes are the most important and therefore only examples from the remaining classes should be removed. The user may define a subset of the existing classes in which this technique will be applied.
  }
}
\details{NCL algorithm includes two phases. In the first phase the ENN algorithm is used to undersample the examples whose class label is not in Cl. Then, a second step is performed which aims at further clean the neighbourhood of the examples in Cl. To achive this, the k nearest neighbours of examples in Cl are scanned. An example is removed if all the previous neighbours have a class label which is not in Cl, and if the example belongs to a class which is larger than half of the smaller class in Cl. In either steps the examples with class labels in Cl are always maintained.
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
\code{\link{EnnClassif}}
}
\examples{
# generate an small imbalanced data set
ir<- iris[-c(95:130),]
# apply NCL method with different metrics, number of neighbours and classes
ir.ManAuto <- NCLClassif(Species~., ir, k=3, dist="p-norm", p=1, Cl="smaller")
ir.ManAuto<- NCLClassif(Species~., ir, k=5, dist="p-norm", p=1, Cl="smaller")
ir.EuclSmal <- NCLClassif(Species~., ir)
ir.Cheby <- NCLClassif(Species~., ir, k=7, dist="Chebyshev", Cl="virginica")
ir.ChebyEuc <- NCLClassif(Species~., ir, k=3, dist="Euclidean", Cl=c("setosa", "virginica"))
summary(ir.M1$Species)
summary(ir.M2$Species)
summary(ir.Def$Species)
summary(ir.Ch$Species)
summary(ir.Eu$Species)
}

\keyword{models}

