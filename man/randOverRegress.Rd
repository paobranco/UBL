\name{RandOverRegress}
\alias{RandOverRegress}

\title{
Random over-sampling for imbalanced regression problems
}
\description{
This function performs a random over-sampling strategy for imbalanced regression problems. 
Basically a percentage of cases of the "class(es)"
(bumps above a relevance threshold defined) selected by the user are randomly over-sampled. 
Alternatively, it can either balance all the existing "classes" 
(the default) or it can "smoothly invert" the frequency
of the examples in each class.
}
\usage{
RandOverRegress(form, dat, rel = "auto", thr.rel = 0.5, 
                C.perc = "balance", repl = TRUE)
}

\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{dat}{
    A data frame containing the original imbalanced data set.
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined (the default) or may be provided by the user through a matrix with the interpolating points.
  }
  \item{thr.rel}{
    A number indicating the relevance threshold above which a case is considered as belonging to the rare "class".
  }
  \item{C.perc}{
      A list containing the over-sampling percentage/s to apply to all/each
        "class" (bump) obtained with the relevance threshold. Replicas of the examples 
        are added randomly in each "class". Moreover, different percentages may 
        be provided for each "class". Alternatively, it may be "balance" or "extreme",
        cases where the over-sampling percentages are automatically estimated.
  }
  \item{repl}{
  A boolean value controlling the possibility of having repetition of examples when choosing the examples to repeat in the over-sampled data set. 
Defaults to TRUE because this is a necessary condition if the selected percentage is greater than 2. This parameter is only important when the over-sampling percentage is between 1 and 2. In this case, it controls if all the new examples selected from a given "class" can be repeated or not.
  }
}
\details{
This function performs a random over-sampling strategy for dealing with imbalanced regression problems. The new examples included in the new data set are randomly selected replicas of the examples already present in the original data set. 
}
\value{
  The function returns a data frame with
  the new data set resulting from the application of the random over-sampling strategy.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{RandUnderRegress}}
}
\examples{

data(morley)

C.perc = list(2, 4)
myover <- RandOverRegress(Speed~., morley, C.perc=C.perc)
Bal <- RandOverRegress(Speed~., morley, C.perc= "balance")
Ext <- RandOverRegress(Speed~., morley, C.perc= "extreme")

library(DMwR)
data(algae)
clean.algae <- algae[complete.cases(algae), ]
# all automatic
ROB <-RandOverRegress(a7~., clean.algae)
# user defined percentage for the only existing extreme (high)
myRO <-RandOverRegress(a7~., clean.algae, rel = "auto", thr.rel = 0.7,
                        C.perc = list(5))

# check the results
plot(clean.algae[,c(1,ncol(clean.algae))])
plot(ROB[,c(1,ncol(clean.algae))])
plot(myRO[,c(1,ncol(clean.algae))])

}

\keyword{pre-processing regression}
