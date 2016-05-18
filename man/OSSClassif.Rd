\name{OSSClassif}
\alias{OSSClassif}

\title{
One-sided selection strategy for handling multiclass imbalanced problems.
}
\description{
 This function performs an adapted one-sided selection strategy for multiclass imbalanced problems.
}
\usage{
OSSClassif(form, dat, dist = "Euclidean", p = 2, Cl = "smaller", start = "CNN")
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
   A character vector indicating which are the most important classes. Defaults to "smaller" which means that the smaller classes are automatically determined. In this case, all the smaller classes are those with a frequency below (nr.examples)/(nr.classes). With the selection of option "smaller" those classes are the ones considered important for the user.
   }
   \item{start}{
     A string which determines which strategy (CNN or Tomek links) should be performed first. The existing options are "CNN" and "Tomek". The first one, "CNN", which is the default, means that CNN strategy will be performed first and Tomek links are applied after. On the other hand, if \code{start} is set to "Tomek" then the reverse order is applied (first Tomek links and after CNN strategy).
   }
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the selected OSS strategy. 
}
\references{
Kubat, M. & Matwin, S. (1997). \emph{Addressing the Curse of Imbalanced Training Sets: One-Sided Selection} Proc. of the 14th Int. Conf. on Machine Learning, Morgan Kaufmann, 179-186.

Batista, G. E.; Prati, R. C. & Monard, M. C. (2004). \emph{A study of the behavior of several methods for balancing machine learning training data} ACM SIGKDD Explorations Newsletter, ACM, 6, 20-29

}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{TomekClassif}, \link{CNNClassif}}
}

\examples{
  \dontrun{
  library(DMwR)
  data(algae)
  clean.algae <- algae[complete.cases(algae), ]
  alg1 <- OSSClassif(season~., clean.algae, dist = "HVDM", 
                     Cl = c("spring", "summer"))
  alg2 <- OSSClassif(season~., clean.algae, dist = "HEOM", 
                     Cl = c("spring", "summer"), start = "Tomek")
  alg3 <- OSSClassif(season~., clean.algae, dist = "HVDM", start = "CNN")
  alg4 <- OSSClassif(season~., clean.algae, dist = "HVDM", start = "Tomek")
  alg5 <- OSSClassif(season~., clean.algae, dist = "HEOM", Cl = "winter")
  summary(alg1$season)
  summary(alg2$season)
  summary(alg3$season)
  summary(alg4$season)
  summary(alg5$season)
  }
}
\keyword{pre-processing classification}

