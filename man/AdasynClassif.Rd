\name{AdasynClassif}
\alias{AdasynClassif}

\title{
ADASYN algorithm for unbalanced classification problems, both binary and multi-class.
}
\description{
This function handles unbalanced classification problems using the ADASYN
algorithm. This algorithm generates synthetic cases using a SMOTE-like approache.
However, the examples of the class(es) where over-sampling is applied are weighted
according to their level of difficulty in learning. This means that more synthetic
data is generated for cases which are hrder to learn compared to the examples of
the same class that are easier to learn. This implementation provides a strategy
suitable for both binary and multi-class problems.
}
\usage{
AdasynClassif(form, dat, baseClass = NULL, beta = 1, dth = 0.95,
                          k = 5, dist = "Euclidean", p = 2)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{baseClass}{
  Character specifying the reference class, i.e., the class from which all other
  classes will be compared to. This can be selected by the user or estimated from the 
  classes distribution. If not defined (the default) the majority class is selected.
  }
  \item{beta}{
    Either a numeric value indicating the desired balance level after synthetic 
    examples generation, or a named list specifying the selected classes beta 
    value. A beta value of 1 (the default) corresponds to full balancing the 
    classes. See examples. 
  }
  \item{dth}{A threshold for the maximum tolerated degree of class imbalance ratio.
  Defaults to 0.95, meaning that the strategy is applied if the 
  imbalance ratio is more than 5\%. This means that the strategy is applied to a class A if, the ratio of this class frequency and the baseClass frequency is less than 95\% (|A|/|baseClass| < 0.95).
  }
  \item{k}{
    A number indicating the number of nearest neighbors that are used to
    generate the new examples of the minority class(es).
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining
    the k nearest neighbors. See the details. Defaults to "Euclidean".
  }
    \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen. Only 
    necessary to define if a "p-norm" is chosen in the \code{dist} argument. 
    See details.
  }
}

\details{
  \describe{
    \item{\code{dist} parameter:}{The parameter \code{dist} allows the user to define the distance metric to be used in the neighbors computation. Although the default is the Euclidean distance, other metrics are available. This allows the computation of distances in data sets with, for instance, both nominal and numeric features. The options available for the distance functions are as follows: 
    
    - for data with only numeric features: "Manhattan", "Euclidean", "Canberra", "Chebyshev", "p-norm";
    
    - for data with only nominal features: "Overlap";
    
    - for dealing with both nominal and numeric features: "HEOM", "HVDM".
    
    When the "p-norm" is selected for the \code{dist} parameter, it is also necessary to define the value of parameter \code{p}. The value of parameter \code{p} sets which "p-norm" will be used. For instance, if \code{p} is set to 1, the "1-norm" (or Manhattan distance) is used, and if \code{p} is set to 2, the "2-norm" (or Euclidean distance) is applied.
    For more details regarding the distance functions implemented in UBL package please see the package vignettes.
    }
  }
}

\value{
  The function returns a data frame with
  the new data set resulting from the application of ADASYN
  algorithm. 
}

\references{
He, H., Bai, Y., Garcia, E.A. and Li, S., 2008, June. 
\emph{ADASYN: Adaptive synthetic sampling approach for imbalanced learning.}
In 2008 IEEE International Joint Conference on Neural Networks 
(IEEE World Congress on Computational Intelligence) (pp. 1322-1328). IEEE.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  \seealso{
\code{\link{SmoteClassif}, \link{RandOverClassif}, \link{WERCSClassif}}
}

\examples{
# Example with an imbalanced multi-class problem
 data(iris)
 dat <- iris[-c(45:75), c(1, 2, 5)]
# checking the class distribution of this artificial data set
 table(dat$Species)
 newdata <- AdasynClassif(Species~., dat, beta=1)
 table(newdata$Species)
 beta <- list("setosa"=1, "versicolor"=0.5)
 newdata <- AdasynClassif(Species~., dat, baseClass="virginica", beta=beta)
 table(newdata$Species)

## Checking visually the created data
par(mfrow = c(1, 2))
plot(dat[, 1], dat[, 2], pch = 19 + as.integer(dat[, 3]),
     col = as.integer(dat[,3]), main = "Original Data",
     xlim=range(newdata[,1]), ylim=range(newdata[,2]))
plot(newdata[, 1], newdata[, 2], pch = 19 + as.integer(newdata[, 3]),
     col = as.integer(newdata[,3]), main = "New Data",
     xlim=range(newdata[,1]), ylim=range(newdata[,2]))

# A binary example
library(MASS)
data(cats)
table(cats$Sex)
Ada1cats <- AdasynClassif(Sex~., cats)
table(Ada1cats$Sex)
Ada2cats <- AdasynClassif(Sex~., cats, beta=5)
table(Ada2cats$Sex)

}

\keyword{pre-processing classification}

