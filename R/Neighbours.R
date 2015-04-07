# ===================================================
# This function prepares the data to pass to fortran call 
# P. Branco, April 2015
# ---------------------------------------------------

neighbours <- function(tgt, data, dist, p=2, k, use.at="all")
#  INPUTS:
#   tgt is the column where the target variable is
#   data is the original data set
#   dist is the distance measure to be used
#   p is a parameter used when a p-norm is computed
#   k is the number of neighbours to consider
#   use.at if set to "all" (default) means that all the attribute types 
#          are to be used. It can be set to "nominal" or "numeric" meaning
#          that only the selected type of attributes wil be used.
#          If dist is HVDM use.at should be always "all".
#   OUTPUTS:
#   a matrix with the indexes of the k nearest neighbours for each example
{
  # check if p has an admissible value(>=1) and change to distance integer code
  # p-norm : p provided
  # Manhattan : p=1
  # Euclidean : p=2
  # Chebyshev : p=0
  # HVDM : p=-1
  
  
  
  if(p<1) stop("The parameter p must be >=1!")
  # check this! It is not exactly the intended behaviour
  p<- switch(dist, "Chebyshev"=0, "Manhattan"=1, "Euclidean"=2, "HVDM"=-1, "p-norm"=p, stop("Distance measure not available!"))
  
  # check use.at and change to integer code:
  # "numeric": 0
  # "nominal": 1
  # "all": 2
  
  if(use.at=="numeric")codeAt <- 0
  else if(use.at=="nominal") codeAt <- 1
  else codeAt <- 2
  
  # construct a flag for the type of problem: 0 classification 1 regression (0 false 1 true)
  ifelse (class(data[,tgt])=="numeric", flag <- 1, flag <- 0)
  
  # tgt is included in the nominal atributes if it is a classification problem
  # or in the numeric atributes if it is a regression problem (always in the last column)
  nomatr <- c() 
  for(col in seq.int(dim(data)[2])){
    if(class(data[,col]) %in% c('factor','character')){
      nomatr <- c(nomatr, col)
    }
  }
  
  numatr <- setdiff(seq.int(dim(data)[2]), nomatr)
  
  nomData <- t(sapply(subset(data, select=setdiff(nomatr, tgt)), as.integer))
  numData <- t(subset(data, select=setdiff(numatr, tgt)))
  #ifelse(flag, numData <- cbind(num.data, data[,tgt]), nomData <- cbind(nomData, data[,tgt]))
  
  #nomData <- t(sapply(nomData, as.integer))
  #numData <- t(numData)
  tgtData <- data[,tgt]
  n <- length(tgtData)
  res <- matrix(0,nrow=k, ncol=n)
  if(class(tgtData)!="numeric"){tgtData <- as.integer(tgtData)}
  
  Cl <- length(unique(tgtData))
  nnom <- length(nomatr)
  nnum <- length(numatr)

  
  
  dist <- matrix(0,nrow=n, ncol=n)
  numD <- matrix(0,nrow=nnum, ncol=n)
  
  
                 
  storage.mode(numData) <- "double"
  storage.mode(nomData) <- "integer"
  storage.mode(res) <- "integer"
  storage.mode(tgtData) <- "double"
  storage.mode(dist) <- "double"
  storage.mode(numD) <- "double"
  
  neig <- .Fortran("neighbours", 
                   flag=as.integer(flag), # flag for classification/regression task
                   codeAt=as.integer(codeAt),
                   tgtData=tgtData,  # tgt data
                   numData=numData, #numeric data
                   nomData=nomData, #nominal data
                   p=as.integer(p), # code for distance metric
                   k=as.integer(k), # nr of neighbours
                   n=as.integer(n), # nr of examples in the data
                   nnum=as.integer(nnum), # nr of numeric attributes
                   nnom=as.integer(nnom), # nr of nominal attributes
                   Cl=as.integer(Cl), # number of different classes in the target variable
                   dist=dist,
                   numD=numD,
                   res=res) # output
  neig <- t(neig$res)
#  rownames(neig) <- rownames(data)
#  neig <- cbind(seq(1:nrow(neig)),neig)
  neig
}
