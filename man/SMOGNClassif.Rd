\name{SMOGNClassif}
\alias{SMOGNClassif}

\title{
SMOGN algorithm for imbalanced classification problems
}
\description{
This function handles imbalanced classification problems using the SMOGN
method. Namely, it can generate a new data set containing synthetic examples
that addresses the problem of imbalanced domains. The new examples are obtained
either using SMOTE method or the introduction of Gaussian Noise
depending on the distance between the two original cases used.
If they are too further apart Gaussian Noise is used, if they are close
then it is safe to use SMOTE method.
}
\usage{
SMOGNClassif(form, dat, C.perc = "balance",
                         k = 5, repl = FALSE, dist = "Euclidean",
                         p = 2, pert=0.01)
}
\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{C.perc}{
      A named list containing the percentage(s) of under- or/and 
        over-sampling to apply to each class.
        The over-sampling percentage is a number above 1 while the under-sampling percentage should be a number below 1. If the number 1 is provided for a given class then that class remains unchanged. Alternatively it may be "balance" (the default) or "extreme", cases where the sampling percentages are automatically estimated either to balance the examples between the minority and majority classes or to invert the distribution of examples across the existing classes transforming the majority classes into minority and vice-versa.
  }
  \item{k}{
    A number indicating the number of nearest neighbors that are used to
    generate the new examples of the minority class(es).
  }
  \item{repl}{
    A boolean value controlling the possibility of having repetition of examples when performing under-sampling by selecting among the majority class(es) examples.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors. See the details. Defaults to "Euclidean".
  }
    \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen. Only necessary to define if a "p-norm" is chosen in the \code{dist} argument. See details.
  }
    \item{pert}{
    A number indicating the level of perturbation to introduce when generating synthetic examples. Assuming as center the base example, this parameter defines the radius (based on the standard deviation) where the new example is generated.
  }

}
\details{
  \describe{
    \item{\code{dist} parameter:}{The parameter \code{dist} allows the user to define the distance metric to be used in the neighbors computation. Although the default is the Euclidean distance, other metrics are available. This allows the computation of distances in data sets with, for instance, both nominal and numeric features. The options available for the distance functions are as follows: 
    
    - for data with only numeric features: "Manhattan", "Euclidean", "Canberra", "Chebyshev", "p-norm";
    
    - for data with only nominal features: "Overlap";
    
    - for dealing with both nominal and numeric features: "HEOM".
    
    When the "p-norm" is selected for the \code{dist} parameter, it is also necessary to define the value of parameter \code{p}. The value of parameter \code{p} sets which "p-norm" will be used. For instance, if \code{p} is set to 1, the "1-norm" (or Manhattan distance) is used, and if \code{p} is set to 2, the "2-norm" (or Euclidean distance) is applied.
    For more details regarding the distance functions implemented in UBL package please see the package vignettes.
    }
}
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the SMOGN
  algorithm. 
}
\references{
Paula Branco, Luis Torgo, and Rita Paula Ribeiro. \emph{SMOGN: a pre-processing approach for imbalanced regression.} First International Workshop on Learning with Imbalanced Domains: Theory and Applications, 36-50(2017).
Torgo, Luis and Ribeiro, Rita P and Pfahringer, Bernhard and Branco, Paula (2013). \emph{SMOTE for Regression}. Progress in Artificial Intelligence, Springer,378-389.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{SmoteClassif}, \link{GaussNoiseClassif}}
}

\examples{

  ir <- iris[-c(95:130), ]
  smogn1 <- SMOGNClassif(Species~., ir)
  smogn2 <- SMOGNClassif(Species~., ir,  C.perc=list(setosa=0.5,versicolor=2.5))

  # checking visually the results 
  plot(sort(ir$Sepal.Width))
  plot(sort(smogn1$Sepal.Width))
  
  # using a relevance function provided by the user
  rel <- matrix(0, ncol = 3, nrow = 0)
  rel <- rbind(rel, c(2, 1, 0))
  rel <- rbind(rel, c(3, 0, 0))
  rel <- rbind(rel, c(4, 1, 0))

  smognRel <- SmoteRegress(Sepal.Width~., ir, rel = rel, dist = "HEOM",
                        C.perc = list(4, 0.5, 4))

  plot(sort(smognRel$Sepal.Width))
}

\keyword{pre-processing classification}
