\name{smote}
\alias{smote}

\title{
SMOTE algorithm for unbalanced classification problems
}
\description{
This function handles unbalanced classification problems using the SMOTE
method. Namely, it can generate a new "SMOTEd" data set that addresses
the class unbalance problem. Alternatively, it can also run a
classification algorithm on this new data set and return the resulting
model. 
}
\usage{
smote(form, data, perc.over = 2, k = 5, perc.under = 2)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{data}{
    A data frame containing the original (unbalanced) data set
  }
  \item{perc.over}{
    A number that drives the decision of how many extra cases from the
    minority class are generated (known as over-sampling).
  }
  \item{k}{
    A number indicating the number of nearest neighbours that are used to
    generate the new examples of the minority class.
  }
  \item{perc.under}{
    A number that drives the decision of how many extra cases from the
    majority classes are selected for each case generated from the
    minority class (known as under-sampling)
  }
}
\details{
  Unbalanced classification problems cause problems to many learning
  algorithms. These problems are characterized by  the uneven proportion
  of cases that are available for each class of the problem.
  
  SMOTE (Chawla et. al. 2002) is a well-known algorithm to fight this
  problem. The general idea of this method is to artificially generate
  new examples of the minority class using the nearest neighbors of
  these cases. Furthermore, the majority class examples are also
  under-sampled, leading to a more balanced dataset. 

The parameters \code{perc.over} and \code{perc.under} control the amount
of over-sampling of the minority class and under-sampling of the
majority classes, respectively. \code{perc.over} will tipically be a
number above 1. With this type of values, for each case in the orginal
data set belonging to the minority class, \code{perc.over} new
examples of that class will be created. If \code{perc.over} is a value
below 1 than a single case will be generated for a randomly selected
proportion (given by \code{perc.over}) of the cases belonging to the
minority class on the original data set. The parameter \code{perc.under}
controls the proportion of cases of the majority class that will be
randomly selected for the final "balanced" data set. This proportion is
calculated with respect to the number of newly generated minority class
cases. For instance, if 200 new examples were generated for the minority
class, a value of \code{perc.under} of 1 will randomly select exactly 200 cases
belonging to the majority classes from the original data set to belong
to the final data set. Values above 1 will select more examples from
the majority classes. 

The parameter \code{k} controls the way the new examples are
created. For each currently existing minority class example X new
examples will be created (this is controlled by the parameter
\code{perc.over} as mentioned above). These examples will be generated
by using the information from the \code{k} nearest neighbours of each
example of the minority class. The parameter \code{k} controls how many
of these neighbours are used.

}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the SMOTE
  algorithm. 
}
\references{
  Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer, W. P. (2002).
\emph{Smote: Synthetic minority over-sampling technique}. Journal of Artificial
Intelligence Research, 16:321-357.

  Torgo, L. (2010) \emph{Data Mining using R: learning with case studies},
  CRC Press (ISBN: 9781439810187).
  \url{http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR} 

  Torgo, L. (2014) \emph{An Infra-Structure for Performance
    Estimation and Experimental Comparison of Predictive Models in R}. arXiv:1412.0436 [cs.MS]
  \url{http://arxiv.org/abs/1412.0436}
}
\author{ Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
\examples{
## A small example with a data set created artificially from the IRIS
## data 
data(iris)
data <- iris[, c(1, 2, 5)]
data$Species <- factor(ifelse(data$Species == "setosa","rare","common")) 
## checking the class distribution of this artificial data set
table(data$Species)

## now using SMOTE to create a more "balanced problem"
newData <- smote(Species ~ ., data, perc.over = 6,perc.under=1)
table(newData$Species)

## Checking visually the created data
\dontrun{
par(mfrow = c(1, 2))
plot(data[, 1], data[, 2], pch = 19 + as.integer(data[, 3]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[,3]),
     main = "SMOTE'd Data")
}

}

\keyword{models}

