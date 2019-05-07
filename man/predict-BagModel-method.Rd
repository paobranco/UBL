\docType{methods}
\name{predict,BagModel-method}
\alias{predict,BagModel-method}
\title{Predicting on new data with a \strong{BagModel} model}
\usage{
\S4method{predict}{BagModel}(object, newdata)
}
\arguments{
\item{object}{A \strong{BagModel-class} object.}

\item{newdata}{New test data to predict using a \code{BagModel} object.}
}
\value{
predictions produced by a \code{BagModel} model.
}
\description{
This is a \code{predict} method for predicting new test points using a
\code{BagModel} class object - refering to an ensemble
of weak models whose type is selected by the user. 
}
\seealso{
\code{\link{BagModel-class}} for details about the bagging model.
}
