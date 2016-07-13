\name{ImpSampRegress}
\alias{ImpSampRegress}

\title{
Importance Sampling algorithm for imbalanced regression problems
}
\description{
This function handles imbalanced regression problems using the relevance function provided to re-sample the data set. The relevance function is used to introduce replicas of the most important examples and to remove the least important examples.
}
\usage{
ImpSampRegress(form, dat, rel = "auto", thr.rel = NA, 
               C.perc = "balance", O = 0.5, U = 0.5)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined (the default) or may be provided by the user through a matrix with interpolating points.
  }
  \item{thr.rel}{
    The default is NA which means that no threshold is used when performing the over/under-sampling. In this case, the over-sampling is performed by assigning a higher probability for selecting an example to the examples with higher relevance. On the other hand, the under-sampling is performed by removing the examples with less relevance. The user may chose a number between 0 and 1 indicating the relevance threshold above which a case is considered as belonging to the rare "class".  
  }
    \item{C.perc}{
      A list containing the percentage(s) of under- or/and 
        over-sampling to apply to each "class" obtained with the threshold. This parameter is only used when a relevance threshold (thr.rel) is set. Otherwise it is ignored. The \code{C.perc} values should be provided in ascending order of target variable values. The over-sampling percentage(s) must be numbers higher than 1 and represent the increase applied to the examples of the bump. The under-sampling percentage(s) should be numbers below 1 and represent the decrease applyed in the corresponding bump. If the value of 1 is provided for a given bump, then the examples in that bump will remain unchanged. Alternatively, this parameter may be set to "balance" (the default) or "extreme", cases where the sampling percentages are automatically estimated. 
  }
  \item{O}{
  A number expressing the importance given to over-sampling when the 
       thr.rel parameter is NA. When O increases the number of examples to
         include during the over-sampling step also increases. Default to 0.5.
  }
  \item{U}{
  A number expressing the importance given to under-sampling when the 
         thr.rel parameter is NA. When U increases, the number of examples selected
         during the under-sampling step also increases. Defaults to 0.5.
  }
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the importance sampling strategy. 
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{RandUnderRegress}, \link{RandOverRegress}}
}

\examples{
  library(DMwR)
  data(algae)
  clean.algae <- algae[complete.cases(algae), ]
  # defining a threshold on the relevance
  IS.ext <-ImpSampRegress(a7~., clean.algae, rel = "auto", 
                          thr.rel = 0.7, C.perc = "extreme")
  IS.bal <-ImpSampRegress(a7~., clean.algae, rel = "auto", thr.rel = 0.7,
                          C.perc = "balance")
  myIS <-ImpSampRegress(a7~., clean.algae, rel = "auto", thr.rel = 0.7,
                        C.perc = list(0.2, 6))
  # neither threshold nor C.perc defined
  IS.auto <- ImpSampRegress(a7~., clean.algae, rel = "auto")

}

\keyword{pre-processing regression}
