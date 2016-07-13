\name{RandUnderRegress}
\alias{RandUnderRegress}

\title{
Random under-sampling for imbalanced regression problems
}
\description{
This function performs a random under-sampling strategy for imbalanced regression problems. Essentially, a percentage of cases of the "class(es)" (bumps below a relevance threshold defined) 
selected by the user are randomly removed. Alternatively, the strategy can be applied to either balance all the 
existing "classes"" or to "smoothly invert" the frequency
of the examples in each "class".

}
\usage{
RandUnderRegress(form, dat,  rel = "auto", thr.rel = 0.5, 
                 C.perc = "balance", repl = FALSE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original imbalanced data set.
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined (the default) or may be provided by the user through a matrix with interpolating points. 
  }
  \item{thr.rel}{
    A number indicating the relevance threshold below which a case is considered as belonging to the normal "class".
  }
  \item{C.perc}{
      A list containing the under-sampling percentage/s to apply to all/each
        "class" (bump) obtained with the relevance threshold. Examples are randomly removed from the "class(es)". If only one percentage is provided this value is reused in all the "classes" that have values below the relevance threshold. A different percentage can be provided to each "class". In this case, the percentages should be provided in ascending order of target variable value. The under-sampling percentage(s), should be a number below 1, meaning that the normal cases (cases below the threshold) are under-sampled by the corresponding percentage. If the number 1 is provided then those examples are not changed. 
        Alternatively, \code{C.perc} parameter may be set to "balance" or "extreme",
        cases where the under-sampling percentages are automatically estimated to either balance or invert the frequencies of the examples in the "classes" (bumps).
  }
  \item{repl}{
  A boolean value controlling the possibility of having repetition of examples in the under-sampled data set. Defaults to FALSE.
  }
}
\details{
This function performs a random under-sampling strategy for dealing with imbalanced regression problems. The examples removed are randomly selected among the examples belonging to the normal "class(es)" (bump of relevance below the threshold defined). The user can chose one or more bumps to be under-sampled.

}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the random under-sampling strategy.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  

\seealso{
\code{\link{RandOverRegress}}
}
\examples{

data(morley)

C.perc = list(0.5)
myUnd <- RandUnderRegress(Speed~., morley, C.perc=C.perc)
Bal <- RandUnderRegress(Speed~., morley, C.perc= "balance")
Ext <- RandUnderRegress(Speed~., morley, C.perc= "extreme")

}

\keyword{pre-processing regression}
