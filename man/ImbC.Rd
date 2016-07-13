\name{ImbC}
\alias{ImbC}
\title{Synthetic Imbalanced Data Set for a Multi-class Task
}
\description{
Synthetic imbalanced data set for a multi-class task. The data set has a numeric feature ("X1"), a nominal feature ("X2") and a target class named "Class". The three classes of the problem ("normal", "rare1" and "rare2") are assigned according to the rules described below. These rules depend of the two features ("X1" and "X2").
}
\usage{
data(ImbC)
}
\format{
The data set has one continuous feature (\code{X1}) and one nominal feature (\code{X2}). The target class (denoted as \code{Class}) has three possible values ("normal" , "rare1" and "rare2"). Classes "rare1" and "rare2" are the minority classes. Examples of class "rare1" occur in 1\% of the data while those of class "rare2" occur in 13.1\% of the data. The remaining class, "normal", is the majority class and occurs in about 85.9\% of the data. Data set ImbC has 1000 examples distributed in classes "rare1", "rare2" and "normal" with 10, 131 and 859 examples respectively.

ImbC data has been simulated as follows:
\describe{
\item{-}{\code{X1}\eqn{\sim \mathbf{N} \left(0, 4\right)}}
\item{-}{\code{X2} labels "cat", "fish" and "dog" where randomly distributed with the restriction of having a frequency of 30\%, 30\% and 40\% respectively.}
\item{-}{To obtain the target variable \code{Class}, we have define the following sets:
\itemize{
\item \eqn{S_1=\{(X1, X2) : X1 > 9 \wedge (X2 \in \{"cat", "dog"\})\}}
\item \eqn{S_2=\{(X1, X2) : X1 > 7 \wedge X2 = "fish" \}}
\item \eqn{S_3=\{(X1, X2) :-1  <  X1 < 0.5\}}
\item \eqn{S_4=\{(X1, X2) : X1 < -7 \wedge X2 = "fish"\}}
}
}
\item{-}{ The following conditions define the target variable distribution of the ImbC synthetic data set:
\itemize{
\item Assign class label "rare1" to: a random sample of 90\% of set \eqn{S_1} and a random sample of 40\% of set \eqn{S_2}
\item Assign class label "rare2" to: a random sample of 80\% of set \eqn{S_3} and a random sample of 70\% of set \eqn{S_4}
\item Assign class label "normal" to the remaing examples.
}
}
}
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} 
}
\examples{
require(ggplot2)
data(ImbC)
summary(ImbC)
ggplot(data=ImbC, aes(x=X2, y=X1, color=Class))+geom_jitter()
}

\keyword{datasets}
