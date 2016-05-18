\name{SmoteClassif}
\alias{SmoteClassif}

\title{
SMOTE algorithm for unbalanced classification problems
}
\description{
This function handles unbalanced classification problems using the SMOTE
method. Namely, it can generate a new "SMOTEd" data set that addresses
the class unbalance problem. 
}
\usage{
SmoteClassif(form, dat, C.perc = "balance", k = 5, repl = FALSE,
             dist = "Euclidean", p = 2)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{C.perc}{
      A named list containing the percentage(s) of under- or/and 
        over-sampling to apply to each class.
        The over-sampling percentage means that the examples above the threshold
        are increased by this percentage. The under-sampling percentage means 
        that the normal cases are under-sampled by this percentage. Alternatively it may be "balance" (the default) or "extreme", cases where the sampling percentages are automatically estimated either to balance the examples between the minority and majority classes or to invert the distribution of examples across the existing classes transforming the majority classes into minority and vice-versa.
  }
  \item{k}{
    A number indicating the number of nearest neighbors that are used to
    generate the new examples of the minority class(es).
  }
  \item{repl}{
    A boolean value controlling the possibility of having repetition of examples when performing under-sampling by selecting among the majority class(es) examples.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors.
  }
    \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen.
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
The parameter \code{k} controls the number of neighbors used to generate new synthetic examples.
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
\code{\link{RandUnderClassif}, \link{RandOverClassif}}
}

\examples{
## A small example with a data set created artificially from the IRIS
## data 
data(iris)
dat <- iris[, c(1, 2, 5)]
dat$Species <- factor(ifelse(dat$Species == "setosa", "rare", "common")) 
## checking the class distribution of this artificial data set
table(dat$Species)

## now using SMOTE to create a more "balanced problem"
newData <- SmoteClassif(Species ~ ., dat, C.perc = list(common = 1,rare = 6))
table(newData$Species)

## Checking visually the created data
par(mfrow = c(1, 2))
plot(dat[, 1], dat[, 2], pch = 19 + as.integer(dat[, 3]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[, 3]),
     main = "SMOTE'd Data")


# automatically balancing the data maintaining the total number of examples
datBal <- SmoteClassif(Species ~ ., dat, C.perc = "balance")
table(datBal$Species)

# automatically inverting the original distribution of examples 
datExt <- SmoteClassif(Species ~ ., dat, C.perc = "extreme")
table(datExt$Species)


 library(DMwR)
 data(algae)
 clean.algae <- algae[complete.cases(algae),]
 C.perc = list(autumn = 2, summer = 1.5, winter = 0.9) 
 # class spring remains unchanged
 # In this case it is necessary to define a distance function that 
 # is able to deal with both nominal and numeric features 
 mysmote.algae <- SmoteClassif(season~., clean.algae, C.perc, dist = "HEOM")
 # the distance function may be HVDM 
 smoteBalan.algae <- SmoteClassif(season~., clean.algae, "balance",
                                  dist = "HVDM")
 smoteExtre.algae <- SmoteClassif(season~., clean.algae, "extreme",
                                  dist = "HVDM")

  library(MASS)
  data(cats)
  mysmote.cats <- SmoteClassif(Sex~., cats, list(M = 0.8, F = 1.8))
  smoteBalan.cats <- SmoteClassif(Sex~., cats, "balance")
  smoteExtre.cats <- SmoteClassif(Sex~., cats, "extreme")

}

\keyword{pre-processing classification}

