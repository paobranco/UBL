\name{ImpSampRegress}
\alias{ImpSampRegress}

\title{
Importance Sampling algorithm for imbalanced regression problems
}
\description{
This function handles imbalanced regression problems using the relevance function provided to resample the data set. The relevance function is used to introduce replicas of the most important examples and to remove the least important examples.
}
\usage{
ImpSampRegress(form, data, rel="auto", thr.rel=NA, C.perc="balance")
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{data}{
    A data frame containing the original (unbalanced) data set
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined with uba package (the default) or may be provided by the user through a matrix.
  }
  \item{thr.rel}{
    The default is NA which means that no threshold is used when performing the over/undersampling. In this case, the oversampling is performed by assigning a higher probability f selecting an example to the examples with higher relevance. On the other hand, the undersampling is performed by removing the examples with less relevance. The user may chose a number between 0 and 1 indicating the relevance threshold above which a case is considered as belonging to the rare "class".  
  }
    \item{C.perc}{
      A list containing the percentage(s) of under- or/and 
        over-sampling to apply to each "class" obtained with the threshold.
        The over-sampling percentage means that the examples above the threshold
        are increased by this percentage. The undersampling percentage means 
        that the normal cases (cases below the threshold) are undersampled by 
        this percentage. Alternatively it may be "balance" (the default) or "extreme",
        cases where the sampling percentages are automatically estimated. This parameter is only used when a relevance threshold (thr.rel) is set. Otherwise it is ignored.
  }
}
\details{

}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the importance sampling strategy. 
}
\references{
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{randUnderRegress}, \link{randOverRegress}}
}

\examples{
  library(DMwR)
  data(algae)
  clean.algae <- algae[complete.cases(algae),]
  IS.ext <-ImpSampRegress(a7~., clean.algae, rel="auto", thr.rel=0.7, C.perc="extreme")
  IS.bal <-ImpSampRegress(a7~., clean.algae, rel="auto", thr.rel=0.7, C.perc="balance")
  myIS <-ImpSampRegress(a7~., clean.algae, rel="auto", thr.rel=0.7, C.perc=list(0.2,6))
  # neither threshold nor C.perc defined
  IS.auto <- ImpSampRegress(a7~., clean.algae, rel="auto")

}

\keyword{models}
