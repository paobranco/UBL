\name{randOverRegress}
\alias{randOverRegress}

\title{
Random oversampling for imbalanced regression problems
}
\description{
This function performs a random oversampling strategy for imbalanced regression problems. 
Basically a percentage of cases of the "class(es)"
(bumps above a relevance threshold defined) selected by the user are randomly over-sampled. 
Alternatively, it can either balance all the existing "classes" 
(the default) or it can "smoothly invert" the frequency
of the examples in each class.
}
\usage{
randOverRegress(form, data, rel="auto", thr.rel=0.5, C.perc, repl=TRUE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{data}{
    A data frame containing the original imbalanced data set.
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined with uba package (the default) or may be provided by the user through a matrix.
  }
  \item{thr.rel}{
    A number indicating the relevance threshold above which a case is considered as belonging to the rare "class".
  }
  \item{C.perc}{
      A list containing the over-sampling percentage/s to apply to all/each
        "class" (bump) obtained with the relevance threshold. Replicas of the examples 
        are added randomly in each "class". Moreover, different percentages may 
        be provided for each "class". Alternatively, it may be "balance" or "extreme",
        cases where the over-sampling percentages are automatically estimated.
  }
  \item{repl}{
  A boolean value controlling the possibility of having repetition of examples when choosing the examples to repeat in the over-sampled data set. 
Defaults to TRUE because this is a necessary condition if the selected percentage is greater than 2. This parameter is only important when the oversampling percentage is between 1 and 2. In this case, it controls if all the new examples selected from a given "class" can be repeated or not.
  }
}
\details{
This function performs a random oversampling strategy for dealing with imbalanced regression problems. The new examples included in the new data set are randomly selected replicas of the examples already present in the original data set. 
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the random oversampling strategy.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{randUnderRegress}}
}
\examples{
  library(DMwR)
  data(algae)
  clean.algae <- algae[complete.cases(algae),]
  C.perc=list(4) 
  alg.myover <- randOverRegress(a7~., clean.algae, C.perc=C.perc)
  alg.Bal <- randOverRegress(a7~., clean.algae, C.perc="balance")
  alg.Ext <- randOverRegress(a7~., clean.algae, C.perc="extreme")

}

\keyword{pre-processing regression}
