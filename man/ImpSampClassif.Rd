\name{WERCSClassif}
\alias{WERCSClassif}

\title{
WEighted Relevance-based Combination Strategy (WERCS) algorithm for imbalanced classification problems
}
\description{
This function handles imbalanced classification problems using the importance/relevance provided to re-sample the data set. The relevance is used to introduce replicas of the most important examples and to remove the least important examples. This function combines random over-sampling with random under-sampling which are applied in the problem classes according to the corresponding relevance.
}
\usage{
WERCSClassif(form, dat, C.perc = "balance")
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{C.perc}{
      A list containing the percentage(s) of random under- or 
        over-sampling to apply to each class. The over-sampling percentage is a number above 1 while the under-sampling percentage should be a number below 1. If the number 1 is provided for a given class then that class remains unchanged. Alternatively it may be "balance" (the default) or "extreme",
        cases where the sampling percentages are automatically estimated. 
  }
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the importance sampling strategy. 
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{RandUnderClassif}, \link{RandOverClassif}}
}

\examples{
  data(iris)
  # generating an artificially imbalanced data set
  ir <- iris[-c(51:70,111:150), ]
  IS.ext <-WERCSClassif(Species~., ir, C.perc = "extreme")
  IS.bal <-WERCSClassif(Species~., ir, C.perc = "balance")
  myIS <-WERCSClassif(Species~., ir, C.perc = list(setosa = 0.2,
                                                    versicolor = 2,
                                                    virginica = 6))
  # check the results
  table(ir$Species)
  table(IS.ext$Species)
  table(IS.bal$Species)
  table(myIS$Species)
}

\keyword{pre-processing classification}
