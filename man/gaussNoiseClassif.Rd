\name{GaussNoiseClassif}
\alias{GaussNoiseClassif}

\title{
Introduction of Gaussian Noise for the generation of synthetic examples to handle imbalanced multiclass problems.
}
\description{
This strategy performs both over-sampling and under-sampling. The under-sampling is randomly performed on the examples of the classes defined by the user through the \code{C.perc} parameter. Regarding the over-sampling method, this is based on the generation of new synthetic examples with the introduction of a small perturbation on existing examples through Gaussian noise. A new example from a minority class is obtained by perturbing each feature a percentage of its standard deviation (evaluated on the minority class examples). For nominal features, the new example randomly selects a label according to the frequency of examples belonging to the minority class. The \code{C.perc} parameter is also used to express which percentage of over-sampling should be applied and to which classes.
}
\usage{
GaussNoiseClassif(form, dat, C.perc = "balance", pert = 0.1, repl = FALSE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{dat}{
    A data frame containing the original (unbalanced) data set
  }
  \item{C.perc}{
      A named list containing the percentage(s) of under- or/and  over-sampling to apply to each class.         The over-sampling percentage is a number above 1 while the under-sampling percentage should be a number below 1. If the number 1 is provided for a given class then that class remains unchanged. Alternatively it may be "balance" (the default) or "extreme", cases where the sampling percentages are automatically estimated either to balance the examples between the minority and majority classes or to invert the distribution of examples across the existing classes transforming the majority classes into minority and vice-versa.
  }
  \item{pert}{
    A number indicating the level of perturbation to introduce when generating synthetic examples. Assuming as center the base example, this parameter defines the radius (based on the standard deviation) where the new example is generated.
  }
  \item{repl}{
    A boolean value controlling the possibility of having repetition of examples when performing under-sampling by selecting among the majority class(es) examples.
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
\code{\link{SmoteClassif}}
}

\examples{
  if (requireNamespace("DMwR2", quietly = TRUE)) {
data(algae, package ="DMwR2")
clean.algae <- data.frame(algae[complete.cases(algae), ])
# autumn and summer are the most important classes and winter
# is the least important
C.perc = list(autumn = 3, summer = 1.5, winter = 0.2)
gn <- GaussNoiseClassif(season~., clean.algae, C.perc)
table(algae$season)
table(gn$season)
} else {
# another example
data(iris)
dat <- iris[, c(1, 2, 5)]
dat$Species <- factor(ifelse(dat$Species == "setosa", "rare", "common")) 
## checking the class distribution of this artificial data set
table(dat$Species)
## now using gaussian noise to create a more "balanced problem"
new.gn <- GaussNoiseClassif(Species ~ ., dat)
table(new.gn$Species)
## Checking visually the created data
 par(mfrow = c(1, 2))
 plot(dat[, 1], dat[, 2], pch = as.integer(dat[, 3]), 
      col = as.integer(dat[, 3]), main = "Original Data")
 plot(new.gn[, 1], new.gn[, 2], pch = as.integer(new.gn[, 3]),
      col = as.integer(new.gn[, 3]), main = "Data with Gaussian Noise")
      }

}

\keyword{pre-processing classification}

