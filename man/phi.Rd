\name{phi}
\alias{phi}

\title{Relevance function. 
}
\description{This function allows to obtain the relevance function values on a set of target variable values given the interpolating points.
}
\usage{
phi(y, control.parms)
}

\arguments{
  \item{y}{
  The target variable values of the problem.
  }
  \item{control.parms}{A named list supplied by the phi.control function with the parameters needed for obtaining the relevance values.
  }
}
\value{
The function returns the relevance values. 
}

\details{
 The phi function specifies the regions of interest in the target
 variable. It does so by performing a Monotone Cubic Spline
 Interpolation over a set of maximum and minimum relevance points. 
 The notion of relevance can be associated with rarity.
 Nonetheless, this notion may depend on the domain experts knowledge.
}
\references{Ribeiro, R., 2011. Utility-based regression 
              (Doctoral dissertation, PhD thesis, 
              Dep. Computer Science, Faculty of Sciences - 
              University of Porto).
            
            Fritsch, F.N. and Carlson, R.E., 1980. Monotone piecewise cubic interpolation.
            SIAM Journal on Numerical Analysis, 17(2), pp.238-246.
}
\author{ Rita Ribeiro
  \email{rpribeiro@dcc.fc.up.pt}, Paula Branco \email{paobranco@gmail.com}, and Luis Torgo \email{ltorgo@dcc.fc.up.pt} }
  
  
\seealso{
\code{\link{phi.control}}
}

\examples{

# example of a relevance function where the extremes are the important values.
data(morley)
# the target variable
y <- morley$Speed

phiF.args <- phi.control(y,method="extremes",extr.type="both")
y.phi <- phi(y, control.parms=phiF.args)
plot(y, y.phi)

}

\keyword{relevance function}
