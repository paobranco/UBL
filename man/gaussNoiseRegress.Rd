\name{GaussNoiseRegress}
\alias{GaussNoiseRegress}

\title{
Introduction of Gaussian Noise for the generation of synthetic examples to handle imbalanced regression problems
}
\description{
 This strategy performs both over-sampling and under-sampling. The under-sampling is randomly performed on the examples below the relevance threshold defined by the user. Regarding the over-sampling method, this is based on the generation of new synthetic examples with the introduction of a small perturbation on existing examples through Gaussian noise. A new example from a rare "class"" is obtained by perturbing all the features and the target variable a percentage of its standard deviation (evaluated on the rare examples). The value of nominal features of the new example is randomly selected according to the frequency of the values existing in the rare cases of the bump in consideration.
}
\usage{
GaussNoiseRegress(form, dat, rel = "auto", thr.rel = 0.5, C.perc = "balance", 
                  pert = 0.1, repl = FALSE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined (the default) or may be provided by the user through a matrix with interpolating points.
  }
  \item{thr.rel}{
    A number indicating the relevance threshold above which a case is considered as belonging to the rare "class".
  }
    \item{C.perc}{
      A list containing the percentage(s) of under- or/and  over-sampling to apply to each "class" (bump) obtained with the threshold. The \code{C.perc} values should be provided in ascending order of target variable values. The over-sampling percentage(s) should be numbers above 1 and represent the increase that is applied to the examples of the bump. The under-sampling percentage(s) should be numbers below 1 and represent the decrease applied to the cases in the corresponding bump. If the value of 1 is provided for a given bump, then the examples in that bump will remain unchanged. Alternatively it may be "balance" (the default) or "extreme", cases where the sampling percentages are automatically estimated.
  }
  \item{pert}{
    A number indicating the level of perturbation to introduce when generating synthetic examples. Assuming as center the base example, this parameter defines the radius (based on the standard deviation) where the new example is generated.
  }
  \item{repl}{
    A boolean value controlling the possibility of having repetition of examples when performing under-sampling by selecting among the "normal" examples.
  }
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of random under-sampling and over-sampling through the generation of synthetic examples using Gaussian noise. 
}
\references{
Sauchi Stephen Lee. (1999) \emph{Regularization in skewed binary classification.} Computational Statistics Vol.14, Issue 2, 277-292.

Sauchi Stephen Lee. (2000) \emph{Noisy replication in skewed binary classification.} Computaional stistics and data analysis Vol.34, Issue 2, 165-191.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{SmoteRegress}}
}

\examples{
  if (requireNamespace("DMwR2", quietly = TRUE)) {
  data(algae, package ="DMwR2")
  clean.algae <- data.frame(algae[complete.cases(algae), ])
  C.perc = list(0.5, 3) 
  mygn.alg <- GaussNoiseRegress(a7~., clean.algae, C.perc = C.perc)
  gnB.alg <- GaussNoiseRegress(a7~., clean.algae, C.perc = "balance", 
                               pert = 0.1)
  gnE.alg <- GaussNoiseRegress(a7~., clean.algae, C.perc = "extreme")
  
  plot(density(clean.algae$a7))
  lines(density(gnE.alg$a7), col = 2)
  lines(density(gnB.alg$a7), col = 3)
  lines(density(mygn.alg$a7), col = 4)


} else {
  ir <- iris[-c(95:130), ]
  mygn1.iris <- GaussNoiseRegress(Sepal.Width~., ir, C.perc = list(0.5, 2.5))
  mygn2.iris <- GaussNoiseRegress(Sepal.Width~., ir, C.perc = list(0.2, 4),
                                  thr.rel = 0.8)
  gnB.iris <- GaussNoiseRegress(Sepal.Width~., ir, C.perc = "balance")
  gnE.iris <- GaussNoiseRegress(Sepal.Width~., ir, C.perc = "extreme")
  
  # defining a relevance function
  rel <- matrix(0, ncol = 3, nrow = 0)
  rel <- rbind(rel, c(2, 1, 0))
  rel <- rbind(rel, c(3, 0, 0))
  rel <- rbind(rel, c(4, 1, 0))

  gn.rel <- GaussNoiseRegress(Sepal.Width~., ir, rel = rel,
                              C.perc = list(5, 0.2, 5))
  plot(density(ir$Sepal.Width), ylim = c(0,1))
  lines(density(gnB.iris$Sepal.Width), col = 3)
  lines(density(gnE.iris$Sepal.Width, bw = 0.3), col = 4)
  # check the impact of a different relevance threshold
  lines(density(gn.rel$Sepal.Width), col = 2)
  }
}

\keyword{pre-processing regression}
