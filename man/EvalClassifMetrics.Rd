\name{EvalClassifMetrics}
\alias{EvalClassifMetrics}

\title{Utility metrics for assessing the performance of utility-based classification tasks. 
}
\description{This function allows to evaluate utility-based metrics in classification problems which have defined a cost, benefit, or utility matrix.
}
\usage{
EvalClassifMetrics(trues, preds, mtr, type = "util", metrics = NULL, thr=0.5, beta = 1)
}

\arguments{
  \item{trues}{
 A vector with the true target variable values of the problem.
  }
  \item{preds}{
 A vector with the prediction values obtained for the vector of trues.
  }
  \item{mtr}{
  A matrix that can be either a cost, a benefit or a utility matrix. 
  The matrix must be always provided with the true class in the rows and 
  the predicted class in the columns.
  }
  \item{type}{
  A character specifying the type of matrix provided. Can be set to "cost", 
  "benefit" or "utility" (the default).
  }
  \item{metrics}{A character vector with the metrics names to be evaluated. If not specified (the default), all the metrics avaliable for the type of matrix provided are evaluated.
  }
  \item{thr}{A numeric value between 0 and 1 setting a threshold on the
  relevance values for determining which are the important classes to consider.
  This threshold is only necessary for the following metrics: precPhi, recPhi
  and FPhi. Moreover, these metrics are only available for problems based on 
  utility matrices. Defaults to 0.5.
  }
  \item{beta}{
  The numeric value of the beta parameter for F-score.
  }
}
\value{
The function returns a named list with the evaluated metrics results. 
}
\references{Ribeiro, R., 2011. Utility-based regression 
              (Doctoral dissertation, PhD thesis, 
              Dep. Computer Science, Faculty of Sciences - 
              University of Porto).
              
            Branco, P., 2014. Re-sampling Approaches for Regression Tasks under Imbalanced Domains 
            (Msc thesis, Dep. Computer Science, Faculty of Sciences - 
            University of Porto).
}

\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{phi.control}}
}

\examples{
# the synthetic data set provided with UBL package for classification
data(ImbC)
sp <- sample(1:nrow(ImbC), round(0.7*nrow(ImbC)))
train <- ImbC[sp, ]
test <- ImbC[-sp,]

# example with a utility matrix
# define a utility matrix (true class in rows and pred class in columns)
 matU <- matrix(c(0.2, -0.5, -0.3, -1, 1, -0.9, -0.9, -0.8, 0.9), byrow=TRUE, ncol=3)
# determine optimal preds (predictions that maximize utility)
library(e1071) # for the naiveBayes classifier
 resUtil <- UtilOptimClassif(Class~., train, test, mtr = matU, type="util",
                        learner = "naiveBayes", 
                        predictor.pars = list(type="raw", threshold = 0.01))
 
# learning a model without maximizing utility
 model <- naiveBayes(Class~., train)
 resNormal <- predict(model, test, type="class", threshold = 0.01)
#Check the difference in the total utility of the results
 EvalClassifMetrics(test$Class, resNormal, mtr=matU, type= "util")
 EvalClassifMetrics(test$Class, resUtil, mtr=matU, type= "util")
   
# example with a classification task that has a cost matrix associated
# define a cost matrix (true class in rows and pred class in columns)
 matC <- matrix(c(0, 0.5, 0.3, 1, 0, 0.9, 0.9, 0.8, 0), byrow=TRUE, ncol=3)
 resUtil <- UtilOptimClassif(Class~., train, test, mtr = matC, type="cost",
                            learner = "naiveBayes", 
                            predictor.pars = list(type="raw", threshold = 0.01))
 
 # learning a model without maximizing utility
 model <- naiveBayes(Class~., train)
 resNormal <- predict(model, test, type="class")
 #Check the difference in the total utility of the results
 EvalClassifMetrics(test$Class, resNormal, mtr=matC, type= "cost")
 EvalClassifMetrics(test$Class, resUtil, mtr=matC, type= "cost")
 
#example with a benefit matrix
# define a benefit matrix (true class in rows and pred class in columns)
 matB <- matrix(c(0.2, 0, 0, 0, 1, 0, 0, 0, 0.9), byrow=TRUE, ncol=3)
 
 resUtil <- UtilOptimClassif(Class~., train, test, mtr = matB, type="ben",
                            learner = "naiveBayes", 
                            predictor.pars = list(type="raw", threshold = 0.01))
 
# learning a model without maximizing utility
 model <- naiveBayes(Class~., train)
 resNormal <- predict(model, test, type="class", threshold = 0.01)
# Check the difference in the total utility of the results
 EvalClassifMetrics(test$Class, resNormal, mtr=matB, type= "ben")
 EvalClassifMetrics(test$Class, resUtil, mtr=matB, type= "ben")
 
 table(test$Class,resNormal)
 table(test$Class,resUtil)

}

\keyword{evaluation metrics}
