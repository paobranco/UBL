# ===================================================
# This function prepares the data and calls fortran subroutine
# that computes the distances between examples
# P. Branco, June 2017
# ---------------------------------------------------

distances <- function(tgt, dat, dist, p=2)
  #  INPUTS:
  #   tgt is the column where the target variable is
  #   dat is the original data set
  #   dist is the distance measure to be used
  #   p is a parameter used when a p-norm is computed
  #   OUTPUTS:
  #   a matrix with the distances between each example
{
  # check if p has an admissible value(>=1) and change to distance integer code
  # p-norm : p provided
  # Manhattan : p=1
  # Euclidean : p=2
  # Chebyshev : p=0
  # Canberra : p=-1
  # HEOM : p=-2
  # HVDM : p=-3
  # DVDM : p=-4
  # IVDM : p=-5
  # WVDM : p=-6
  # MVDM : p=-7
  
  # just set a dummy value for the number of neighbours to evaluate, which is not
  # used in this function but is necessary for the fortran function.  
  
  k <- 1

  if(p<1) stop("The parameter p must be >=1!")
  # p >=-1 only numeric attributes handled
  # p =-2 only nominal attributes handled
  # p<= -3 nominal and numeric attributes handled
  p<- switch(dist,
             "Chebyshev"=0,
             "Manhattan"=1,
             "Euclidean"=2,
             "Canberra"=-1,
             "Overlap"=-2,
             "HEOM"=-3,
             "HVDM"=-4,
             # to be implemented
             #             "DVDM"=-5,
             #             "IVDM"=-6,
             #             "WVDM"=-7,
             #             "MVDM"=-8,
             "p-norm"=p,
             stop("Distance measure not available!"))
  
  
  if (class(dat[,tgt]) == "numeric" & p <= -4) stop("distance measure selected only available for classification tasks")
  
  nomatr <- c() 
  for (col in seq.int(dim(dat)[2])) {
    if (class(dat[,col]) %in% c('factor','character')) {
      nomatr <- c(nomatr, col)
    }
  }
  
  nomatr <- setdiff(nomatr, tgt)
  numatr <- setdiff(seq.int(dim(dat)[2]), c(nomatr,tgt))
  
  nomData <- t(sapply(subset(dat, select = nomatr), as.integer))
  numData <- t(subset(dat, select = numatr))
  
  # check if the measure can be applied to the data set features
  
  if (length(numatr) & p == -2) {
    stop("Can not compute Overlap metric with numeric attributes!")
  }
  if (length(nomatr) & p >= -1) {
    stop("Can not compute ", dist ," distance with nominal attributes!")
  }
  
  tgtData <- dat[, tgt]
  n <- length(tgtData)
  res <- matrix(0.0, nrow = k, ncol = n)
  if (class(tgtData) != "numeric") {tgtData <- as.integer(tgtData)}
  
  Cl <- length(unique(tgtData))
  nnom <- length(nomatr)
  nnum <- length(numatr)
  
  distm <- matrix(0.0, nrow = n, ncol = n)
  numD <- matrix(0.0, nrow = nnum, ncol = n)
  
  
  storage.mode(numData) <- "double"
  storage.mode(nomData) <- "integer"
  storage.mode(res) <- "integer"
  storage.mode(tgtData) <- "double"
  storage.mode(distm) <- "double"
  storage.mode(numD) <- "double"
  
  neig <- .Fortran("F_neighbours", 
                   tgtData = tgtData,  # tgt data
                   numData = numData, #numeric data
                   nomData = nomData, #nominal data
                   p = as.integer(p), # code for distance metric
                   k = as.integer(k), # nr of neighbours
                   n = as.integer(n), # nr of examples in the data
                   nnum = as.integer(nnum), # nr of numeric attributes
                   nnom = as.integer(nnom), # nr of nominal attributes
                   Cl = as.integer(Cl), # number of different classes in the target variable
                   distm = distm,
                   numD = numD,
                   res = res) # output
  neig <- neig$distm
  
  neig
}
