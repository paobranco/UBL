\name{SMOGNRegress}
\alias{SMOGNRegress}

\title{
SMOGN algorithm for imbalanced regression problems
}
\description{
This function handles imbalanced regression problems using the SMOGN
method. Namely, it can generate a new data set containing synthetic examples
that addresses the problem of imbalanced domains. The new examples are obtained
either using SmoteR method or the introduction of Gaussian Noise
depending on the distance between the two original cases used.
If they are too further apart Gaussian Noise is used, if they are close
then it is safe to use SmoteR method.
}
\usage{
SMOGNRegress(form, dat, rel = "auto", thr.rel = 0.5,
                        C.perc = "balance", k = 5, repl = FALSE,
                        dist = "Euclidean", p = 2, pert=0.01)

}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined (the default) or may be provided by the user through a matrix.
  }
  \item{thr.rel}{
    A number indicating the relevance threshold above which a case is considered as belonging to the rare "class".
  }
    \item{C.perc}{
      A list containing the percentage(s) of under- or/and 
        over-sampling to apply to each "class" (bump) obtained with the threshold. The percentages should be provided in ascending order of target variable value. The percentages are applied in this order to the "classes" (bumps) obtained through the threshold.
        The over-sampling percentage, a number above 1, means that the examples in that bump are increased by this percentage. The under-sampling percentage, a number below 1, means that the cases in the corresponding bump are under-sampled by this percentage. If the number 1 is provided then those examples are not changed. Alternatively it may be "balance" (the default) or "extreme",
        cases where the sampling percentages are automatically estimated.
  
  }
  \item{k}{
    A number indicating the number of nearest neighbors to consider as the pool from where
              the new generated examples are generated.
  }
  \item{repl}{
    A boolean value controlling the possibility of having repetition of examples when performing under-sampling by selecting among the "normal" examples.
  }
    \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors. See the details. Defaults to "Euclidean".
  }
    \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen. Only necessary to define if a "p-norm" is chosen in the \code{dist} argument. see details.
  }
    \item{pert}{
    A number indicating the level of perturbation to introduce when generating synthetic examples through Gaussian Noise. Assuming as center the base example, this parameter defines the radius (based on the standard deviation) where the new example is generated.
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
\code{\link{SmoteRegress}, \link{GaussNoiseRegress}}
}

\examples{

  ir <- iris[-c(95:130), ]
  # using automatic relevance
  smogn1 <- SmoteRegress(Sepal.Width~., ir, dist = "HEOM",
                                C.perc=list(0.5,2.5))
  smogn2 <- SmoteRegress(Sepal.Width~., ir, dist = "HEOM",
                                C.perc = list(0.2, 4), thr.rel = 0.8)
  smogn3.iris <- SmoteRegress(Sepal.Width~., ir, dist = "HEOM",
                                C.perc = "balance")

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

\keyword{pre-processing regression}
