\name{ImbR}
\alias{ImbR}
\title{Synthetic Regression Data Set
}
\description{
Simulated data set for imbalanced domain on regression. The rare cases corresponden to the higher extreme values and are described by a circle with white noise. The normal cases have a normal distribution with the same center of the circunference with elliptical contours.
}
\usage{
data(ImbR)
}
\format{
The data set has 2 continuous features (\code{X1} and \code{X2}) and a continuous target variable (denoted as \code{Tgt}). The rare examples, i.e, cases with higher values of the target variable  occur in 5\% of the data. Data set ImbR has 1000 examples.

ImbR data has been simulated as follows:
\describe{
\item{-}{lower \code{Tgt} values: \code{(X1, X2)}\eqn{\sim \mathbf{N}_{2} \left(\mathbf{10}_{2}, \mathbf{2.5}_{2}\right)}}  and \code{Tgt}\eqn{\sim \mathbf{\Gamma} \left( 0.5, 1 \right) +10}
\item{-}{higher \code{Tgt} values: \code{(X1, X2)}\eqn{\sim \left(\rho * cos(\theta) + 10, \rho * sin(\theta) + 10 \right)}, where \eqn{\rho \sim \mathbf{9}_{2}+\mathbf{N}_{2} \left(\mathbf{0}_{2}, \mathbf{I}_{2} \right)} and \eqn{\theta \sim \mathbf{U}_{2} \left( \mathbf{0}_{2}, 2\pi \mathbf{I}_{2} \right)} \code{Tgt}\eqn{\sim \mathbf{\Gamma} \left( 1,1 \right) + 20} }
}

}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} 
}
\examples{
data(ImbR)
summary(ImbR)

boxplot(ImbR$Tgt)
}

\keyword{datasets}
