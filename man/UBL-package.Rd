\name{UBL-package}
\alias{UBL-package}
\docType{package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
UBL: Utility-Based Learning
}
\description{
The package provides a diversity of pre-processing functions to deal with both classification (binary and multi-class) and regression problems that encompass non-uniform costs and/or benefits. These functions can be used to obtain a better predictive performance on this type of tasks. 
  The package also includes two synthetic data sets for regression and classification.
}
\details{
\tabular{ll}{
Name: \tab UBL\cr
Type: \tab Package\cr
Version: \tab 0.0.5\cr
Date: \tab 2016-07-01\cr
License: \tab GPL 2 GPL 3\cr
}

The package in focused on utility-based learning, i.e., classification and regression problems with non-uniform benefits and/or costs. The main goal of the implemented functions is to improve the predictive performance of the models obtained. The package provides pre-processing approaches that change the original data set biasing it towards the user preferences.

All the methods avaliable are suitable for classification (binary and multiclass) and regression tasks. Moreover, several distance functions are also implemented which allows the use of the methods in data sets with categorical and/or numeric features. 

We also provide two synthetic data sets for classification and regression.

}
\references{
Branco, P., Ribeiro, R.P. and Torgo, L. (2016) \emph{UBL: an R package for Utility-based Learning.} arXiv preprint arXiv:1604.08079.
}
\author{ Paula Branco \email{paobranco@gmail.com}, Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} 
  
  Maintainer:
Paula Branco}
  
\keyword{ package }

\examples{
\dontrun{
library(UBL)
# an example with the synthetic classification data set provided with the package
data(ImbC)

plot(ImbC$X1, ImbC$X2, col = ImbC$Class, xlab = "X1", ylab = "X2")

summary(ImbC)
# randomly generate a 30-70\% test and train partition 
i.train <- sample(1:nrow(ImbC), as.integer(0.7*nrow(ImbC)))
trainD <- ImbC[i.train,]
testD <- ImbC[-i.train,]

model <- rpart(Class~., trainD)
preds <- predict(model, testD, type = "class")
table(preds, testD$Class)

# apply random over-sampling approach to balance the data set:

newTrain <- RandOverClassif(Class~., trainD)

newModel <- rpart(Class~., newTrain)
newPreds <- predict(newModel, testD, type = "class")
table(newPreds, testD$Class)


# an example with the synthetic regression data set provided with the package
data(ImbR)

library(ggplot2)
ggplot(ImbR, aes(x = X1, y = X2)) + geom_point(data = ImbR, aes(colour=Tgt)) +
      scale_color_gradient(low = "red", high="blue")

boxplot(ImbR$Tgt)
#relevance function automatically obtained
phiF.args <- phi.control(ImbR$Tgt, method = "extremes", extr.type = "high")
y.phi <- phi(sort(ImbR$Tgt),control.parms = phiF.args)

plot(sort(ImbR$Tgt), y.phi, type = "l", xlab = "Tgt variable", ylab = "relevance value")

# set the train and test data
i.train <- sample(1:nrow(ImbR), as.integer(0.7*nrow(ImbR)))
trainD <- ImbR[i.train,]
testD <- ImbR[-i.train,]

# train a model on the original train data
library(DMwR)
model <- rpartXse(Tgt~., trainD, se = 0)

preds <- predict(model, testD)

plot(testD$Tgt, preds, xlim = c(0,55), ylim = c(0,55))
abline(a = 0, b = 1)

# obtain a new train using random under-sampling strategy
newTrain <- RandUnderRegress(Tgt~., trainD)
newModel <- rpartXse(Tgt~., newTrain, se = 0)
newPreds <- predict(newModel, testD)

# plot the predictions for the model obtained with 
# the original and the modified train data
plot(testD$Tgt, preds, xlim = c(0,55), ylim = c(0,55)) #black for original train
abline(a = 0, b = 1, lty=2, col="grey")
points(testD$Tgt, newPreds, col="blue", pch=2) #blue for changed train
abline(h=30, lty=2, col="grey")
abline(v=30, lty=2, col="grey")
}
}