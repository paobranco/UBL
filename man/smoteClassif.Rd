\name{smoteClassif}
\alias{smoteClassif}

\title{
SMOTE algorithm for unbalanced classification problems
}
\description{
This function handles unbalanced classification problems using the SMOTE
method. Namely, it can generate a new "SMOTEd" data set that addresses
the class unbalance problem. 
}
\usage{
smoteClassif(form, data, C.perc, k=5, repl=FALSE, dist="Euclidean", p=2)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{data}{
    A data frame containing the original (unbalanced) data set
  }
  \item{C.perc}{
      A named list containing the percentage(s) of under- or/and 
        over-sampling to apply to each class.
        The oversampling percentage means that the examples above the threshold
        are increased by this percentage. The undersampling percentage means 
        that the normal cases are undersampled by this percentage. Alternatively it may be "balance" (the default) or "extreme", cases where the sampling percentages are automatically estimated either to balance the examples between the minority and majority classes or to invert the distribution of examples across the existing classes transforming the majority classes into minority and vice-versa.
  }
  \item{k}{
    A number indicating the number of nearest neighbours that are used to
    generate the new examples of the minority class(es).
  }
  \item{repl}{
    A boolean value controling the possibility of having repetition of examples when performing undersampling by selecting among the majority class(es) examples.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbours.
  }
    \item{p}{
    A number indicating the value of p if the "p-norm" distance is choosen.
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

The parameter \code{C.perc} controls the amount
of over-sampling and under-sampling applied and can be automatically estimated either to balance or invert the distribution of examples across the different classes. 
The parameter \code{k} controls the number of neighbours used to generate new synthetic examples.
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
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  \seealso{
\code{\link{randUnderClassif}, \link{randOverClassif}}
}

\examples{
## A small example with a data set created artificially from the IRIS
## data 
data(iris)
data <- iris[, c(1, 2, 5)]
data$Species <- factor(ifelse(data$Species == "setosa","rare","common")) 
## checking the class distribution of this artificial data set
table(data$Species)

## now using SMOTE to create a more "balanced problem"
newData <- smoteClassif(Species ~ ., data, C.perc = list(common=1,rare=6))
table(newData$Species)

## Checking visually the created data
\dontrun{
par(mfrow = c(1, 2))
plot(data[, 1], data[, 2], pch = 19 + as.integer(data[, 3]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[,3]),
     main = "SMOTE'd Data")
}

# automaticaly balancing the data maintaining the total number of examples
datBal <- smoteClassif(Species ~ ., data, C.perc = "balance")
table(datBal$Species)

# automaticaly inverting the original distribution of examples 
datExt <- smoteClassif(Species ~ ., data, C.perc = "extreme")
table(datExt$Species)


 library(DMwR)
 data(algae)
 clean.algae <- algae[complete.cases(algae),]
 C.perc=list(autumn=2, summer=1.5, winter=0.9) # class spring remains unchanged
 mysmote.algae <- smoteClassif(season~., clean.algae, C.perc)
 smoteBalan.algae <- smoteClassif(season~., clean.algae, "balance")
 smoteExtre.algae <- smoteClassif(season~., clean.algae, "extreme")

  library(MASS)
  data(cats)
  mysmote.cats <- smoteClassif(Sex~., cats, list(M=0.8, F=1.8))
  smoteBalan.cats <- smoteClassif(Sex~., cats,"balance")
  smoteExtre.cats <- smoteClassif(Sex~., cats, "extreme")

}

\keyword{models}

