\name{neighbours}
\alias{neighbours}

\title{Computation of nearest neighbours using a selected distance function. 
}
\description{This function allows to obtain the nearest neighbours of each example in a data set using a distance function selected by the user.
}
\usage{
neighbours(tgt, dat, dist, p=2, k)
}

\arguments{
  \item{tgt}{
  The column number of the problem target variable.
  }
  \item{dat}{A data frame containing the problem data.
  }
  \item{dist}{A character string specifying the distance function to use in the nearest neighbours evaluation.
  }
  \item{p}{An optional parameter that is only required if the distance function selected in parameter \code{dist} is "p-norm".
  }
  \item{k}{The number of nearest neighbours to return for each example.  
  }
  }
  
\value{
The function returns a matrix with the indexes of the k nearest neighbours for each example in the data set. 
}

\details{Several distance function are implemented in UBL package. The goal of having such a diversity of distance functions is to provide the users more flexibility regarding the distance used and also to provide distance fucntions that are able to deal with nominal and numeric features. The options available for the distance functions are as follows: 
  \describe{
    \item{data with only numeric features:}{ "Manhattan", "Euclidean", "Canberra", "Chebyshev", "p-norm";}
    \item{data with only nominal features:}{ "Overlap";}
    \item{data with both nominal and numeric features:}{ "HEOM", "HVDM".}
    }
    
    When the "p-norm" is selected for the \code{dist} parameter, it is also necessary to define the value of parameter \code{p}. The value of parameter \code{p} sets which "p-norm" will be used. For instance, if \code{p} is set to 1, the "1-norm" (or Manhattan distance) is used, and if \code{p} is set to 2, the "2-norm" (or Euclidean distance) is applied.
    For more details regarding the distance functions implemented in UBL package please see the package vignettes.
    }
    
\references{Wilson, D.R. and Martinez, T.R. (1997). \emph{Improved heterogeneous distance functions.} Journal of artificial intelligence research, pp.1-34.

}

\seealso{
\code{\link{distances}}
}

\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} 
} 
  
\examples{
\dontrun{
data(ImbC)
# determine the 2 nearest neighbours of each example in ImbC data set
# using the "HVDM" distance function.
neig1 <- neighbours(3, ImbC, "HVDM", k=2)

# now using the "HEOM" distance function
neig2 <- neighbours(3, ImbC, "HEOM", k=2)

# check the differences
head(neig1)
head(neig2)
}
}

\keyword{neighbours evaluation}
