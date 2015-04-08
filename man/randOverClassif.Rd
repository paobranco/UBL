\name{randOverClassif}
\alias{randOverClassif}

\title{
Random oversampling for imbalanced classification problems
}
\description{
This function performs a random oversampling strategy for imbalanced multiclass problems. Essentially, a percentage of cases of the class(es) 
selected by the user are randomly oversampled by the introduction of replicas of examples. Alternatively, the strategy can be applied to either balance all the existing classes or to "smoothly invert" the frequency of the examples in each class.
}
\usage{
randOverClassif(form, data, C.perc, repl=TRUE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{data}{
    A data frame containing the original imbalanced data set.
  }
  \item{C.perc}{
A named list containing each class name and the corresponding oversampling percentage, greater than or equal to 1, where 1 means that no oversampling is to be aplied in the corresponding class. The user may indicate only the classes where he wants to apply random oversampling. Alternatively, this parameter can be set to "balance" or "extreme", cases where the oversampling percentages are automatically estimated. The "balance" option tries to balance all the existing classes while the "extreme" option inverts the classes original frequencies.
  }
  \item{repl}{
  A boolean value controling the possibility of having repetition of examples when chosing the examples to repeat in the oversampled data set. Defaults to TRUE because this is a necessary condition if the selected percentage is greater than 2. This parameter is only important when the oversampling percentage is between 1 and 2. In this case, it controls if all the new examples selected from a given class can be repeated or not.
  }
}
\details{
This function performs a random oversampling strategy for dealing with imbalanced multiclass problems. The new examples included in the new data set are replicas of the examples already present in the original data set. 
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the random oversampling strategy.
}
\references{

}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{randUnderClassif}}
}
\examples{
  library(DMwR)
  data(algae)
  # classes spring and winter remain unchanged
  C.perc=list(autumn=2, summer=1.5, spring=1) 
  myover.algae <- randOverClassif(season~., algae, C.perc)
  oveBalan.algae <- randOverClassif(season~., algae, "balance")
  oveInvert.algae <- randOverClassif(season~., algae, "extreme")
  
  library(MASS)
  data(cats)
  myover.cats <- randOverClassif(Sex~., cats, list(M=1.5))
  oveBalan.cats <- randOverClassif(Sex~., cats,"balance")
  oveInvert.cats <- randOverClassif(Sex~., cats, "extreme")
}

\keyword{models}

