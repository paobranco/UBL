\name{CNNClassif}
\alias{CNNClassif}

\title{
Condensed Nearest Neighbors strategy for multiclass imbalanced problems
}
\description{
This function applies the Condensed Nearest Neighbors (CNN) strategy for imbalanced multiclass problems. It constructs a subset of examples which are able to correctly classify the original data set using a one nearest neighbor rule.
}
\usage{
CNNClassif(form, dat, dist = "Euclidean", p = 2, Cl = "smaller")
}
\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original imbalanced data set.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors. Defaults to "Euclidean".
  }
  \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen.
  }
  \item{Cl}{
   A character vector indicating which are the most important classes. Defaults to "smaller" which means that the smaller classes are automatically determined. In this case, all the smaller classes are those with a frequency below #examples/#classes. With the selection of option "smaller" those classes are the ones considered important for the user.}
}
\details{
This function applies the Condensed Nearest Neighbors (CNN) strategy for dealing with imbalanced multiclass problems. The classes selected in \code{Cl} are considered the most important ones and all the others are under-sampled. The CNN under-sampling strategy starts with a set composed by all the examples from the important classes and one randomly selected example from the other classes. Then, examples from the other classes are added to the set forming a subset of examples which correctly classifies the original data set using a one nearest neighbor rule.
}
\value{
  The function returns a list with a data frame with
  the new data set resulting from the application of the CNN strategy, a character vector with the important classes, and another character vector with the unimportant classes.
}
\references{
Hart, P. E. (1968). \emph{The condensed nearest neighbor rule} IEEE Transactions on Information Theory, 14, 515-516
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{OSSClassif}, \link{TomekClassif}}
}
\examples{
  library(DMwR)
  data(algae)
  clean.algae <- algae[complete.cases(algae), ]
  myCNN <- CNNClassif(season~., clean.algae, 
                      Cl = c("summer", "spring", "winter"),
                      dist = "HEOM")
  CNN1 <- CNNClassif(season~., clean.algae, Cl = "smaller", dist = "HEOM")
  CNN2<- CNNClassif(season~., clean.algae, Cl = "summer",dist = "HVDM")
  summary(myCNN[[1]]$season)
  summary(CNN1[[1]]$season)
  summary(CNN2[[1]]$season)
  
  
  ir <- iris[-c(95:130), ]
  myCNN.iris <- CNNClassif(Species~., ir, Cl = c("setosa", "virginica"))
  CNN.iris1 <- CNNClassif(Species~., ir, Cl = "smaller")
  CNN.iris2 <- CNNClassif(Species~., ir, Cl = "versicolor")
  summary(myCNN.iris[[1]]$Species)
  summary(CNN.iris1[[1]]$Species)
  summary(CNN.iris2[[1]]$Species)
  
  library(MASS)
  data(cats)
  CNN.catsF <- CNNClassif(Sex~., cats, Cl = "F")
  CNN.cats <- CNNClassif(Sex~., cats, Cl = "smaller")
}

\keyword{pre-processing classification}

