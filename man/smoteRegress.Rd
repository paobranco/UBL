\name{SmoteRegress}
\alias{SmoteRegress}

\title{
SMOTE algorithm for imbalanced regression problems
}
\description{
This function handles imbalanced regression problems using the SMOTE
method. Namely, it can generate a new "SMOTEd" data set that addresses
the problem of imbalanced domains. 
}
\usage{
SmoteRegress(form, dat, rel = "auto", thr.rel = 0.5, C.perc = "balance",
                         k = 5, repl = FALSE, dist = "Euclidean", p = 2)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined (the default) or may be provided by the user through a matrix.
  }
  \item{thr.rel}{
    A number indicating the relevance threshold above which a case is considered as belonging to the rare "class".
  }
    \item{C.perc}{
      A list containing the percentage(s) of under- or/and 
        over-sampling to apply to each "class" obtained with the threshold.
        The over-sampling percentage means that the examples above the threshold
        are increased by this percentage. The under-sampling percentage means 
        that the normal cases (cases below the threshold) are under-sampled by 
        this percentage. Alternatively it may be "balance" (the default) or "extreme",
        cases where the sampling percentages are automatically estimated.
  
  }
  \item{k}{
    A number indicating the number of nearest neighbors to consider as the pool from where
              the new generated examples are generated.
  }
  \item{repl}{
    A boolean value controlling the possibility of having repetition of examples when performing under-sampling by selecting among the "normal" examples.
  }
    \item{dist}{
    A character string indicating which distance metric to use when determining the k nearest neighbors.
  }
    \item{p}{
    A number indicating the value of p if the "p-norm" distance is chosen. Only necessary if a "p-norm" is choosen in the \code{dist} argument.
  }
}
\details{
  Imbalanced domains cause problems to many learning
  algorithms. These problems are characterized by the uneven proportion
  of cases that are available for certain ranges of the target variable which are the most important to the user.
  
  SMOTE (Chawla et. al. 2002) is a well-known algorithm for classification tasks to fight this
  problem. The general idea of this method is to artificially generate
  new examples of the minority class using the nearest neighbors of
  these cases. Furthermore, the majority class examples are also
  under-sampled, leading to a more balanced data set. smoteR is a variant of SMOTE algorithm proposed by Torgo et al. (2013) to address the problem of imbalanced domains in regression tasks. This function uses the parameters \code{rel} and \code{thr.rel}, a relevance function and a relevance threshold for distinguishing between the normal and rare cases.
  

The parameter \code{C.perc} controls the amount
of over-sampling and under-sampling applied and can be automatically estimated either to balance or invert the distribution of examples across the different bumps. 
The parameter \code{k} controls the number of neighbors used to generate new synthetic examples.
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the smoteR
  algorithm. 
}
\references{
  Chawla, N. V., Bowyer, K. W., Hall, L. O., and Kegelmeyer, W. P. (2002).
\emph{Smote: Synthetic minority over-sampling technique}. Journal of Artificial
Intelligence Research, 16:321-357.

  Torgo, Luis and Ribeiro, Rita P and Pfahringer, Bernhard and Branco, Paula (2013). \emph{SMOTE for Regression}. Progress in Artificial Intelligence, Springer,378-389.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{RandUnderRegress}, \link{RandOverRegress}}
}

\examples{

  ir <- iris[-c(95:130), ]
  mysmote1.iris <- SmoteRegress(Sepal.Width~., ir, dist = "HEOM",
                                C.perc=list(0.5,2.5))
  mysmote2.iris <- SmoteRegress(Sepal.Width~., ir, dist = "HEOM",
                                C.perc = list(0.2, 4), thr.rel = 0.8)
  smoteBalan.iris <- SmoteRegress(Sepal.Width~., ir, dist = "HEOM",
                                C.perc = "balance")
  smoteExtre.iris <- SmoteRegress(Sepal.Width~., ir, dist = "HEOM",
                                C.perc = "extreme")
  
  # checking visually the results 
  plot(sort(ir$Sepal.Width))
  plot(sort(smoteExtre.iris$Sepal.Width))
  
  # using a relevance function provided by the user
  rel <- matrix(0, ncol = 3, nrow = 0)
  rel <- rbind(rel, c(2, 1, 0))
  rel <- rbind(rel, c(3, 0, 0))
  rel <- rbind(rel, c(4, 1, 0))

  sP.ir <- SmoteRegress(Sepal.Width~., ir, rel = rel, dist = "HEOM",
                        C.perc = list(4, 0.5, 4))

}

\keyword{pre-processing regression}
