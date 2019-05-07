\name{BaggingRegress}
\alias{BaggingRegress}

\title{
Standard Bagging ensemble for regression problems.
}
\description{
This function handles regression problems through ensemble learning. A given number of weak learners selected by the user are trained on bootstrap samples of the training data provided.
}
\usage{
BaggingRegress(form, train, nmodels, learner, learner.pars,
               aggregation = "Average", quiet=TRUE)
}
\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{train}{
    A data frame containing the training (imbalanced) data set.
  }
  \item{nmodels}{
    A numeric indicating the number of models to train. 
  }
  \item{learner}{
  The learning algorithm to be used as weak learner.
  }
  \item{learner.pars}{
  A named list with the parameters selected for the learner.
  }
  \item{aggregation}{charater specifying the method used for aggregating the results
  obtained by the individual learners.
  For now, the only method available is by averaging the models predictions.
  }
  \item{quiet}{logical specifying if development should be shown or not.Defaults to TRUE
  }
}

\value{
  The function returns an object of class BagModel. 
}

\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }


\keyword{ensemble bagging regression}

