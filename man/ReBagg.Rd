\name{ReBagg}
\alias{ReBagg}

\title{
REBagg: RE(sampled) BAG(ging), an ensemble method for dealing with imbalanced regression problems.
}
\description{
This function handles imbalanced regression problems by learneing a special purpose bagging ensemble.
A number of weak learners selected by the user are trained on resamples of the training data provided.
The resamples are built taking into consideration the imbalance of the problem. 
Currently, there are 4 different methods for building the resamples used.
}
\usage{
ReBagg(form, train, rel="auto", thr.rel, learner, learner.pars,
       nmodels = "auto", samp.method = "nbalance", aggregation="Average", quiet=TRUE)
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
    A numeric indicating the number of models to train. Defaults to "auto", which means that the minimum number of models are used to guarantee the use of the entire training set.
  }
  \item{samp.method}{
  character specifying the method used for building the resamples
  of the training set provided. 
  Possible characters are: "balance", "nbalance", "variation" and "nvariation".
  The "balance" methods builds a number(nmodels) of samples that use all 
  the rare cases and the same nr of normal cases. The "variation" method
  build a number of baggs with all the rare cases and varying percentages of normal cases.
  Both methods allow the prefix "n", which means that the samples are
  build to have the same size as the training set by allowing replicas of examples.
  In this case, after the baggs are determined their size is increased
  by using sampling with replacement. If the prefix n is not used, then
  the number examples in each bagg is set to a minimum which depends on the
  total number of relevant cases.  Defaults to "nbalance".
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

