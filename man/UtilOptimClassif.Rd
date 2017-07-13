\name{UtilOptimClassif}
\alias{UtilOptimClassif}

\title{
Optimization of predictions utility, cost or benefit for classification problems.
}
\description{
This function determines the optimal predictions given a utility, cost or benefit matrix for the selected learning algorithm. The learning algorithm must provide probabilities for the problem classes. If the matrix provided is of type utility or benefit a maximization process is carried out. If the user provides a cost matrix, then a minimization process is applied.
}
\usage{
UtilOptimClassif(form, train, test, mtr, type = "util",
                 learner = NULL, learner.pars=NULL, predictor="predict",
                 predictor.pars=NULL)
}

\arguments{
  \item{form}{    
  A formula describing the prediction problem.
  }
  \item{train}{
  A data.frame with the training data.
  }
  \item{test}{
  A data.frame with the test data.
  }
  \item{mtr}{
  A matrix, specifying the utility, cost, or benefit values associated to accurate 
  predictions and misclassification errors. It can be either a cost matrix, 
  a benefit matrix or a utility matrix. The corresponding type should be set in 
  parameter type. The matrix must be always provided with the true class in 
  the rows and the predicted class in the columns.
  }
  \item{type}{
  The type of mtr provided. Can be set to: "utility"(default), "cost" or "benefit".
  }
  \item{learner}{
  Character specifying the learning algorithm to use. It is required for the selected learner to 
  provide class probabilities.
  }
  \item{learner.pars}{
  A named list containing the parameters of the learning algorithm.
  }
  \item{predictor}{
  Character specifying the predictor selected (defaults to "predict").
  }
  \item{predictor.pars}{
  A named list with the predictor selected parameters.
  }
}
\value{
  The function returns a vector with the predictions for the test data optimized
  using the matrix provided.
}
\references{
Elkan, C., 2001, August. The foundations of cost-sensitive learning. In International joint conference on artificial intelligence (Vol. 17, No. 1, pp. 973-978). LAWRENCE ERLBAUM ASSOCIATES LTD.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
\seealso{
\code{\link{UtilOptimRegress}, \link{EvalClassifMetrics}}
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

library(e1071) # for the naiveBayes classifier

resUtil <- UtilOptimClassif(Class~., train, test, mtr = matU, type="util",
                       learner = "naiveBayes", 
                       predictor.pars = list(type="raw", threshold = 0.01))

# learning a standard model without maximizing utility
model <- naiveBayes(Class~., train)
resNormal <- predict(model, test, type="class", threshold = 0.01)
# Check the difference in the total utility of the results
EvalClassifMetrics(test$Class, resNormal, mtr=matU, type= "util")
EvalClassifMetrics(test$Class, resUtil, mtr=matU, type= "util")


#example with a cost matrix
# define a cost matrix (true class in rows and pred class in columns)
matC <- matrix(c(0, 0.5, 0.3, 1, 0, 0.9, 0.9, 0.8, 0), byrow=TRUE, ncol=3)
resUtil <- UtilOptimClassif(Class~., train, test, mtr = matC, type="cost",
                           learner = "naiveBayes", 
                           predictor.pars = list(type="raw", threshold = 0.01))

# learning a standard model without minimizing the costs
model <- naiveBayes(Class~., train)
resNormal <- predict(model, test, type="class")
# Check the difference in the total utility of the results
EvalClassifMetrics(test$Class, resNormal, mtr=matC, type= "cost")
EvalClassifMetrics(test$Class, resUtil, mtr=matC, type= "cost")


#example with a benefit matrix
# define a benefit matrix (true class in rows and pred class in columns)
matB <- matrix(c(0.2, 0, 0, 0, 1, 0, 0, 0, 0.9), byrow=TRUE, ncol=3)

resUtil <- UtilOptimClassif(Class~., train, test, mtr = matB, type="ben",
                           learner = "naiveBayes", 
                           predictor.pars = list(type="raw", threshold = 0.01))

# learning a standard model without maximizing benefits
model <- naiveBayes(Class~., train)
resNormal <- predict(model, test, type="class", threshold = 0.01)
# Check the difference in the total utility of the results
EvalClassifMetrics(test$Class, resNormal, mtr=matB, type= "ben")
EvalClassifMetrics(test$Class, resUtil, mtr=matB, type= "ben")

table(test$Class,resNormal)
table(test$Class,resUtil)


}
\keyword{utility optimization, utility-based classification}

