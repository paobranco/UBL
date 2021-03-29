\name{RandUnderClassif}
\alias{RandUnderClassif}

\title{
Random under-sampling for imbalanced classification problems
}
\description{
This function performs a random under-sampling strategy for imbalanced multiclass problems. Essentially, a percentage of cases of the class(es) selected by the user are randomly removed. Alternatively, the strategy can be applied to either balance all the existing classes or to "smoothly invert" the frequency of the examples in each class.

}
\usage{
RandUnderClassif(form, dat, C.perc = "balance", repl = FALSE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original imbalanced data set.
  }
  \item{C.perc}{
A named list containing each class name and the corresponding under-sampling percentage, between 0 and 1, where 1 means that no under-sampling is to be applied in the corresponding class. The user may indicate only the classes where he wants to apply random under-sampling. For instance, a percentage of 0.2 means that, in the changed data set, the class is reduced to 20\% of its original size. Alternatively, this parameter can be set to "balance" (the defualt) or "extreme", cases where the under-sampling percentages are automatically estimated. The "balance" option tries to balance all the existing classes while the "extreme" option inverts the classes original frequencies.
  }
  \item{repl}{
  A boolean value controlling the possibility of having repetition of examples in the under-sampled data set. Defaults to FALSE.
  }
}
\details{
This function performs a random under-sampling strategy for dealing with imbalanced multiclass problems. The examples removed are randomly selected among the examples belonging to each class containing the normal cases. The user can chose one or more classes to be under-sampled. 

}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the random under-sampling strategy.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  

\seealso{
\code{\link{RandOverClassif}}
}
\examples{
  if (requireNamespace("DMwR2", quietly = TRUE)) {

  data(algae, package ="DMwR2")
  clean.algae <- data.frame(algae[complete.cases(algae), ])
  C.perc = list(autumn = 1, summer = 0.9, winter = 0.4) 
  # classes autumn and spring remain unchanged
  
  myunder.algae <- RandUnderClassif(season~., clean.algae, C.perc)
  undBalan.algae <- RandUnderClassif(season~., clean.algae, "balance")
  undInvert.algae <- RandUnderClassif(season~., clean.algae, "extreme")
} else {
  library(MASS)
  data(cats)
  myunder.cats <- RandUnderClassif(Sex~., cats, list(M = 0.8))
  undBalan.cats <- RandUnderClassif(Sex~., cats, "balance")
  undInvert.cats <- RandUnderClassif(Sex~., cats, "extreme")


  # learn a model and check results with original and under-sampled data
  library(rpart)
  idx <- sample(1:nrow(cats), as.integer(0.7*nrow(cats)))
  tr <- cats[idx, ]
  ts <- cats[-idx, ]
  
  idx <- sample(1:nrow(cats), as.integer(0.7*nrow(cats)))
  tr <- cats[idx, ]
  ts <- cats[-idx, ]
  ctO <- rpart(Sex ~ ., tr)
  predsO <- predict(ctO, ts, type = "class")
  new.cats <- RandUnderClassif(Sex~., tr, "balance")
  ct1 <- rpart(Sex ~ ., new.cats)
  preds1 <- predict(ct1, ts, type = "class")
   
  table(predsO, ts$Sex)  
#  predsO  F  M
#      F  9  3
#      M  7 25


  table(preds1, ts$Sex)   
# preds1  F  M
#      F 13  4
#      M  3 24
}
}

\keyword{pre-processing classification}

