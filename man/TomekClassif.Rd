\name{TomekClassif}
\alias{TomekClassif}

\title{
Tomek links for imbalanced classification problems
}
\description{
This function uses Tomek links to perform under-sampling for handling imbalanced multiclass problems. Tomek links are broken by removing one or both examples forming the link. 
}
\usage{
TomekClassif(form, dat, dist = "Euclidean", p = 2, Cl = "all", rem = "both")
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original imbalanced data set.
  }
  \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors. See the details. Defaults to "Euclidean".
  }
  \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen. Only necessary to define if a "p-norm" is chosen in the \code{dist} argument. See details.
  }
  \item{Cl}{
   A character vector indicating which classes should be under-sampled. Defaults to "all" meaning that examples from all existing classes can be removed. The user may also specify a subset of classes for which tomek links should be removed.}
  \item{rem}{
  A character string indicating if both examples forming the Tomek link are to be removed, or if only the example from the larger class should be discarded. In the first case this parameter should be set to "both" and in the second case should be set to "maj".
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
    \item{Tomek method:}{This function performs an under-sampling strategy based on the notion of Tomek links for imbalanced multiclass problems. Two examples form a Tomek link if they are each other closest neighbors and they have different class labels.
    
    The under-sampling procedure can be performed in two different ways. When detected the Tomek links, the examples of both classes can be removed, or the Tomek link can be broken by removing only one of the examples (traditionally the one belonging to the majority class). This function also includes these two procedures. Moreover, it allows for the user to identify in which classes under-sampling should be applied. These two aspects are controlled by the \code{Cl} and \code{rem} parameters. The \code{Cl} parameter is used to express the classes that can be under-sampled and its default is  "all" (all existing classes are candidates for having examples removed). The parameter \code{rem} indicates if the Tomek link is broken by removing both examples ("both") or by removing only the example belonging to the more populated class between the two existing in the Tomek link.
    
    Note that the options for \code{Cl} and \code{rem} may "disagree". In those cases, the preference is given to the \code{Cl} options once the user choose that specific set of classes to under-sample and not the other ones (even if the defined classes are not the larger ones). This means that, when making a decision on how many and which examples will be removed the first criteria used will be the \code{Cl} definition .
    
    
    For a better clarification of the impact of the options selected for Cl and rem parameters we now provide some possible scenarios and the expected behavior:
    
    
    
    1) \code{Cl} is set to one class which is neither the more nor the less frequent, and \code{rem} is set to "maj". The expected behavior is the following:
    - if a Tomek link exists connecting the largest class and another class(not included in \code{Cl}): no example is removed;
    - if a Tomek link exists connecting the larger class and the class defined in \code{Cl}: the example from the \code{Cl} class is removed (because the user expressly indicates that only examples from class \code{Cl} should be removed);
    
    
    2) \code{Cl} includes two classes and \code{rem} is set to "both". This function will do the following:
    - if a Tomek link exists between an example with class in \code{Cl} and another example with class not in \code{Cl}, then, only the example with class in \code{Cl} is removed;
    - if the Tomek link exists between two examples with classes in \code{Cl}, then, both are removed.
    
    
    3) \code{Cl} includes two classes and \code{rem} is set to "maj". The behavior of this function is the following:
    -if a Tomek link exists connecting two classes included in \code{Cl}, then only the example belonging to the more populated class is removed;
    -if a Tomek link exists connecting an example from a class included in \code{Cl} and another example whose class is not in \code{Cl} and is the largest class, then, no example is removed.

}
}
}
\value{
  The function returns a list containing a data frame with
  the new data set resulting from the application of the Tomek link strategy defined, and the indexes of the examples removed.
}
\references{
Tomek, I. (1976). \emph{Two modifications of CNN} IEEE Trans. Syst. Man Cybern.,  769-772
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{OSSClassif}, \link{CNNClassif}}
}
\examples{
  library(DMwR)
  data(algae)
  clean.algae <- algae[complete.cases(algae), ]
  alg.HVDM1 <- TomekClassif(season~., clean.algae, dist = "HVDM", 
                            Cl = c("winter", "spring"), rem = "both")
  alg.HVDM2 <- TomekClassif(season~., clean.algae, dist = "HVDM", rem = "maj")
  
  # removes only examples from class summer which are the 
  # majority class in the link
  alg.EuM <- TomekClassif(season~., clean.algae, dist = "HEOM", 
                          Cl = "summer", rem = "maj")
  
  # removes only examples from class summer in every link they appear
  alg.EuB <- TomekClassif(season~., clean.algae, dist = "HEOM",
                          Cl = "summer", rem = "both")
                          
  summary(clean.algae$season)
  summary(alg.HVDM1[[1]]$season)
  summary(alg.HVDM2[[1]]$season)
  summary(alg.EuM[[1]]$season)
  summary(alg.EuB[[1]]$season)
  
  # check which were the indexes of the examples removed in alg.EuM
  alg.EuM[[2]]
}

\keyword{pre-processing classification}

