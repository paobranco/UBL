\name{RandOverClassif}
\alias{RandOverClassif}

\title{
Random over-sampling for imbalanced classification problems
}
\description{
This function performs a random over-sampling strategy for imbalanced multiclass problems. Essentially, a percentage of cases of the class(es) 
selected by the user are randomly over-sampled by the introduction of replicas of examples. Alternatively, the strategy can be applied to either balance all the existing classes or to "smoothly invert" the frequency of the examples in each class.
}
\usage{
RandOverClassif(form, dat, C.perc = "balance", repl = TRUE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original imbalanced data set.
  }
  \item{C.perc}{
A named list containing each class name and the corresponding over-sampling percentage, greater than or equal to 1, where 1 means that no over-sampling is to be applied in the corresponding class. The user may indicate only the classes where he wants to apply random over-sampling. For instance, a percenatge of 2 means that, in the changed data set, the number of examples of that class are doubled. Alternatively, this parameter can be set to "balance" (the default) or "extreme", cases where the over-sampling percentages are automatically estimated. The "balance" option tries to balance all the existing classes while the "extreme" option inverts the classes original frequencies.
  }
  \item{repl}{
  A boolean value controlling the possibility of having repetition of examples when choosing the examples to repeat in the over-sampled data set. Defaults to TRUE because this is a necessary condition if the selected percentage is greater than 2. This parameter is only important when the over-sampling percentage is between 1 and 2. In this case, it controls if all the new examples selected from a given class can be repeated or not.
  }
}
\details{
This function performs a random over-sampling strategy for dealing with imbalanced multiclass problems. The new examples included in the new data set are replicas of the examples already present in the original data set. 
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the random over-sampling strategy.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{RandUnderClassif}}
}
\examples{
  if (requireNamespace("DMwR2", quietly = TRUE)) {
  data(algae, package ="DMwR2")
  clean.algae <- data.frame(algae[complete.cases(algae), ])
  # classes spring and winter remain unchanged
  C.perc = list(autumn = 2, summer = 1.5, spring = 1) 
  myover.algae <- RandOverClassif(season~., clean.algae, C.perc)
  oveBalan.algae <- RandOverClassif(season~., clean.algae, "balance")
  oveInvert.algae <- RandOverClassif(season~., clean.algae, "extreme")
  } else {
  library(MASS)
  data(cats)
  myover.cats <- RandOverClassif(Sex~., cats, list(M = 1.5))
  oveBalan.cats <- RandOverClassif(Sex~., cats, "balance")
  oveInvert.cats <- RandOverClassif(Sex~., cats, "extreme")
  
  # learn a model and check results with original and over-sampled data
  library(rpart)
  idx <- sample(1:nrow(cats), as.integer(0.7 * nrow(cats)))
  tr <- cats[idx, ]
  ts <- cats[-idx, ]
  
  ctO <- rpart(Sex ~ ., tr)
  predsO <- predict(ctO, ts, type = "class")
  new.cats <- RandOverClassif(Sex~., tr, "balance")
  ct1 <- rpart(Sex ~ ., new.cats)
  preds1 <- predict(ct1, ts, type = "class")
  table(predsO, ts$Sex)   
  table(preds1, ts$Sex)
  }
}

\keyword{pre-processing classification}

