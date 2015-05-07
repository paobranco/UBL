\name{gaussNoiseClassif}
\alias{gaussNoiseClassif}

\title{
Introduction of Gaussian Noise for the generation of synthetic examples to handle imbalanced multiclass problems.
}
\description{
This strategy performs both oversampling and undersampling. The undersampling is randomly performed on the examples below the relevance threshold defined by the user. Regarding the oversampling method, this is based on the generation of new synthetic examples with the introduction of a small perturbation on existing examples through gaussian noise. A new example from a minority class is obtained by perturbing each feature a percentage of its standard deviation (evaluated on the minority class examples). For nominal features, the new example randomly selects a value according to the frequency of
examples belonging to the minority class.
}
\usage{
gaussNoiseClassif(form, data, C.perc="balance", pert=0.1, repl=FALSE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem
  }
  \item{data}{
    A data frame containing the original (unbalanced) data set
  }
  \item{C.perc}{
      A named list containing the percentage(s) of under- or/and  over-sampling to apply to each class. The oversampling percentage means that the examples above the threshold are increased by this percentage. The undersampling percentage means that the normal cases are undersampled by this percentage. Alternatively it may be "balance" (the default) or "extreme", cases where the sampling percentages are automatically estimated either to balance the examples between the minority and majority classes or to invert the distribution of examples across the existing classes transforming the majority classes into minority and vice-versa.
  }
  \item{pert}{
    A number indicating the level of perturbation to introduce when generating synthetic examples. Assuming as center the base example, this parameter defines the radius (based on the standard deviation) where the new example is generated.
  }
  \item{repl}{
    A boolean value controling the possibility of having repetition of examples when performing undersampling by selecting among the majority class(es) examples.
  }
}
\details{

}
\value{
  The function returns a data frame with
  the new data set resulting from the application of random undersampling and oversampling through the generation of synthetic examples using Gaussian noise. 
}
\references{
Sauchi Stephen Lee. (1999)  \emph{Regularization in skewed binary classification.}
Computational Statistics Vol.14, Issue 2, 277-292.
Sauchi Stephen Lee. 2000.  \emph{Noisy replication in skewed binary classification.}
Computational statistics & data analysis Vol. 34, Issue 2, 165–191.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  \seealso{
\code{\link{smoteClassif}}
}

\examples{
library(DMwR)
data(algae)
clean.algae <- algae[complete.cases(algae),]
C.perc=list(autumn=2, summer=1.5, winter=0.9)
gn <- gaussNoiseClassif(season~., clean.algae)
gn <- gaussNoiseClassif(season~., clean.algae, C.perc)

}

\keyword{models}

