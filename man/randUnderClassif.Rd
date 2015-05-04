\name{randUnderClassif}
\alias{randUnderClassif}

\title{
Random undersampling for imbalanced classification problems
}
\description{
This function performs a random undersampling strategy for imbalanced multiclass problems. Essentially, a percentage of cases of the class(es) 
selected by the user are randomly removed. Alternatively, the strategy can be applied to either balance all the 
existing classes or to "smoothly invert" the frequency
of the examples in each class.

}
\usage{
randUnderClassif(form, data, C.perc, repl=FALSE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{data}{
    A data frame containing the original imbalanced data set.
  }
  \item{C.perc}{
A named list containing each class name and the corresponding under-sampling percentage, between 0 and 1, where 1 means that no under-sampling is to be aplied in the corresponding class. The user may indicate only the classes where he wants to apply random undersampling. Alternatively, this parameter can be set to "balance" or "extreme", cases where the under-sampling percentages are automatically estimated. The "balance" option tries to balance all the existing classes while the "extreme" option inverts the classes original frequencies.
  }
  \item{repl}{
  A boolean value controling the possibility of having repetition of examples in the undersampled data set. Defaults to FALSE.
  }
}
\details{
This function performs a random undersampling strategy for dealing with imbalanced multiclass problems. The examples removed are randomly selected among the examples belonging to each class containing the normal cases. The user can chose one or more classes to be undersampled. 

}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the random undersampling strategy.
}
\references{


}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  

\seealso{
\code{\link{randOverClassif}}
}
\examples{
  library(DMwR)
  data(algae)
  C.perc=list(autumn=1, summer=0.9, winter=0.4) # classes autumn and spring remain unchanged
  myunder.algae <- randUnderClassif(season~., algae, C.perc)
  undBalan.algae <- randUnderClassif(season~., algae, "balance")
  undInvert.algae <- randUnderClassif(season~., algae, "extreme")

  library(MASS)
  data(cats)
  myunder.cats <- randUnderClassif(Sex~., cats, list(M=0.8))
  undBalan.cats <- randUnderClassif(Sex~., cats,"balance")
  undInvert.cats <- randUnderClassif(Sex~., cats, "extreme")


}

\keyword{models}

