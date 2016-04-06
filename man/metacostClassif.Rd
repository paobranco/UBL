\name{metacostClassif}
\alias{metacostClassif}

\title{
METACOST algorithm for cost-sensitive classification problems
}
\description{
This function obtains the predicted classes that optimize the utility
given a set of probabilities of each class for each test cases, and a
cost-benefit matrix
}
\usage{
metacostClassif(preds, cb.matrix)
}

\arguments{
  \item{preds}{
    A matrix or data.frame of probabilistic predictions of a classifier
  for each test case, i.e. a nTestCases x nClasses matrix
  }
  \item{cb.matrix}{
    A square matrix with dimensions nClasses x nClasses where entry
  X_i,j corresponds to the cost/benefit of predicting a test case as
  belonging to class j when it is of class i. The diagonal of this
  matrix (correct predictions) should contain positive numbers
  (benefits), whilst numbers outside of the matrix should contain
  negative numbers (costs of misclassifications).
  }

}
\details{
This function applies the well-know METACOST (Domingos, 1999) algorithm
to obtain the predictions of a probabilistic classifier that maximize
the utility according to a provided cost-benefit matrix.
}
\value{
  The function returns a factor of class labels.
}
\references{
    Domingos,P. (1999), \emph{MetaCost: a general method for making
    classifiers cost-sensitive}. Proceedings of
    KDD'1999. \url{http://homes.cs.washington.edu/~pedrod/papers/kdd99.pdf} 

}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\examples{
data(iris)

## First lets create the cost-benefit matrix
cb <- matrix(c(30,-100,-100,-100,60,-200,-100,-200,60),byrow=TRUE,ncol=3)
colnames(cb) <- paste("p",levels(iris$Species),sep=".")
rownames(cb) <- paste("t",levels(iris$Species),sep=".")

## This leads to the following cost-benefit matrix
##              p.setosa p.versicolor p.virginica
## t.setosa           30         -100        -100
## t.versicolor     -100           60        -200
## t.virginica      -100         -200          60

\dontrun{
## Now learning a model
library(rpart)
idx <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
tr <- iris[idx,]
ts <- iris[-idx,]

ct <- rpart(Species ~ ., tr)
prob.preds <- predict(ct,ts)
noprob.preds <- predict(ct,ts,type="class")

metacost.preds <- metacostClassif(prob.preds,cb)

table(noprob.preds,metacost.preds)
table(noprob.preds,ts$Species)
table(metacost.preds,ts$Species)
}
}

\keyword{post-processing classification}

