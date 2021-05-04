\name{ReBaggRegress}
\alias{ReBaggRegress}

\title{
REBaggRegress: RE(sampled) BAG(ging), an ensemble method for dealing with imbalanced regression problems.
}
\description{
This function handles imbalanced regression problems by learneing a special purpose bagging ensemble.
A number of weak learners selected by the user are trained on resamples of the training data provided.
The resamples are built taking into consideration the imbalance of the problem. 
Currently, there are 4 different methods for building the resamples used.
}
\usage{
ReBaggRegress(form, train, rel="auto", thr.rel, learner, learner.pars,
       nmodels, samp.method = "variationSMT", aggregation="Average", quiet=TRUE)
}
\arguments{
  \item{form}{
    A formula describing the prediction problem.
  }
  \item{train}{
    A data frame containing the training (imbalanced) data set.
  }
  \item{rel}{
    The relevance function which can be automatically ("auto") determined (the default) or may be provided by the user through a matrix.
  }
  \item{thr.rel}{
    A number indicating the relevance threshold above which a case is considered as belonging to the rare "class".
  }
  \item{learner}{
  The learning algorithm to be used as weak learner.
  }
  \item{learner.pars}{
  A named list with the learner parameters.
  }
  \item{nmodels}{
    A numeric indicating the number of models to train. 
  }
  \item{samp.method}{
  A character specifying the method used for building the resamples
  of the training set provided. 
  Possible characters are: "balance", "variation", "balanceSMT", "variationSMT".
  The "balance" methods builds a number (nmodels) of samples that use all 
  the rare cases and the same nr of normal cases. The "variation" method
  build a number of baggs with all the rare cases and varying percentages of normal cases.
  The SMT sufix is used when the SmoteR strategy is used to generate the new examples.
  Defaults to "variationSMT".
  }
  \item{aggregation}{charater specifying the method used for aggregating the results
  obtained by the individual learners. 
  For now, the only method available is by averaging the models predictions.
  }
  \item{quiet}{logical specifying if development should be shown or not. Defaults to TRUE
  }
}

\value{
  The function returns an object of class BagModel. 
}
\references{
Branco, P. and Torgo, L. and Ribeiro, R.P. (2018) \emph{REBAGG: REsampled BAGGing for Imbalanced Regression} LIDTA2018: 2nd International Workshop on Learning with Imbalanced Domains: Theory and Applications (Co-located with ECML/PKDD 2018) Dublin, Ireland
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }

\keyword{ensemble bagging regression}

