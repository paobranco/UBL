\name{phi.control}
\alias{phi.control}

\title{Estimation of parameters used for obtaining the relevance function.
}
\description{This function allows to obtain the parameters of the relevance function (\link{phi}). The parameters can be obtained using one of the following methods: "extremes" or "range". If the selected method is "extremes", the distribution of the target variable values is used to assign more importance to the most extreme values according to the boxplot. If the selected method is "range", a matrix should be provided defining the important and unimportant values (see section details).
}
\usage{
phi.control(y, method="extremes", extr.type="both", coef=1.5, control.pts=NULL)
}

\arguments{
  \item{y}{The target variable values.
  }
  \item{method}{"extremes" (default) or "range".
  }
  \item{extr.type}{parameter needed for method "extremes" to specify which type of extremes are to be considered relevant: "high", "low" or "both"(default). 
  }
  \item{coef}{parameter needed for method "extremes" to specify how far the wiskers extend to the most extreme data point in the boxplot. The default is 1.5.
  }
  \item{control.pts}{parameter needed for method "range" to specify the interpolating points to the relevance function (phi). It should be a matrix with three columns. The first column represents the y value, the second column represents the corresponding relevance value (phi(y)) in [0,1], and the third optional column represents the corresponding relevance value derivative (phi'(y)).  
  }

}
\value{
  The function returns a list with the parameters needed for obtaining and evaluating the relevance function (phi).
}

\details{
The method "extremes" uses the target variable distribution to automatically determine the most relevant values. This method uses the boxplot to automatically derive a matrix with the interpolating points for the relevance function (phi). According to the \code{extr.type} parameter it assigns maximum relevance to: only the "high" extremes, only the "low" extremes or "both". In the latter case, it assigns maximum relevance to "both" types of extremes if they exist. If "both" is selected and only one type of extremes is present, then only the existing extremes are considered.

The method "range" uses the \code{control.pts} matrix provided by the user to define the interpolating points for the relevance function (phi). The values supplied in the third column (phi derivative) of the matrix are only indicative, meaning that they will be adjusted afterwards by the relevance function (phi) to create a smooth continuous function.
}
\references{
Ribeiro, R., 2011. Utility-based regression 
              (Doctoral dissertation, PhD thesis, 
              Dep. Computer Science, Faculty of Sciences - 
              University of Porto).
}
\author{ Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt}, Paula Branco \email{paobranco@gmail.com} and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{phi}}
}

\examples{

data(morley)
# the target variable
y <- morley$Speed

# the target variable has "low" and "high"extremes 
boxplot(y)

## using method "extremes" considering that 
## "both" extremes are important
phiF.argsB <- phi.control(y,method="extremes",extr.type="both")
y.phiB <- phi(y, control.parms=phiF.argsB)
plot(y, y.phiB)

## using method "extremes" considering that only the
## "high" extremes are relevant
phiF.argsH <- phi.control(y,method="extremes",extr.type="high")
y.phiH <- phi(y, control.parms=phiF.argsH)
plot(y, y.phiH)


## using method "range" to choose the important values:
rel <- matrix(0,ncol=3,nrow=0)
rel <- rbind(rel,c(700,0,0)) 
rel <- rbind(rel,c(800,1,0))
rel <- rbind(rel,c(900,0,0))
rel <- rbind(rel,c(1000,1,0))
rel
phiF.argsR <- phi.control(y,method="range",control.pts=rel)
y.phiR <- phi(y, control.parms=phiF.argsR)

plot(y, y.phiR)
}

\keyword{relevance function}
