\name{BagModel-class}
\docType{class}
\alias{BagModel}
\alias{BagModel-class}
\alias{show,BagModel-method}

\title{ Class "BagModel"}
\description{
\strong{BagModel} is an S4 class that contains the information of a bagging ensemble model.
Besides the base learning algorithms--\code{baseModels} --
\strong{BagModel} class contains information regarding the
type of weak learners selected, the learned models and the aggregation method to apply in the 
test set for obtaining the final predictions.
}

\section{Objects from BagModel Class}{
  Objects can be created by calls to the constructor
  \code{BagModel(...)}. The constructor requires a formula and a training set,
  the selected model type, the base models learned and the aggregation method
  to use for obtaining the final predictions.
}

\section{Slots}{

\describe{

\item{\code{form}}{formula}

\item{\code{train}}{training set used to train the \code{baseModels}}

\item{\code{learner}}{the weak learners type used}

\item{\code{learner.pars}}{the parameters of the used learners}

\item{\code{baseModels}}{a list of base learners}

\item{\code{aggregation}}{the aggregation method
used to obtain the final predictions. For now, only the \code{average} method is available.}

\item{\code{rel}}{optional information regarding the relevance function definde for the problem. Should be provided as a matrix. 
}

\item{\code{thr.rel}}{Optional number setting the threshold on the relevance values.
}
\item{\code{quiet}}{logical specifying if development should be shown or not. Defaults to TRUE}
}}

\seealso{
\code{\link{BaggingRegress}}
}
