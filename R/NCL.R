# ===================================================
# Neighbourhood Cleaning Rule (NCL) is an undersampling strategy 
# based in ENN which in it's original formulation attempts to clean
# the neighbourhood of the class of interest (Laurikalla, 2001). NCLClassif is 
# suitable for multiclass problems.
# Examples:
#
# ir<- iris[-c(95:130),]
# ir.M1 <- NCLClassif(Species~., ir, k=3, dist="p-norm", p=1, Cl="smaller")
# ir.M2<- NCLClassif(Species~., ir, k=5, dist="p-norm", p=1, Cl="smaller")
# ir.Def <- NCLClassif(Species~., ir)
# ir.Ch <- NCLClassif(Species~., ir, k=7, dist="Chebyshev", Cl="virginica")
# ir.Eu <- NCLClassif(Species~., ir, k=3, dist="Euclidean", Cl=c("versicolor", "virginica"))
# summary(ir.M1$Species)
# summary(ir.M2$Species)
# summary(ir.Def$Species)
# summary(ir.Ch$Species)
# summary(ir.Eu$Species)
#
# P. Branco, April 2015
# ---------------------------------------------------
NCLClassif <- function(form, data, k=3, dist="Euclidean", p=2, use.at="all", Cl="smaller")
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # k is the number of neighbours considered (should be odd to avoid ties)
  # dist represents the distance function to be used for the kNN 
  #       computation
  # p a parameter used when the dist is set to "p-norm" which represents the used p.
  # use.at indicates which attributes should be used in the distance computation (defaults to 
  #       "all")
  # Cl is a vector indicating the names of the most important classes for the user. 
  #       Alternatively, a string can be provided which can be set to "smaller".
  #       The default is "smaller" meaning that the smaller classes (classes with #< (#original data)/(#classes))
  #       are the most important.

{
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  nrCl <-length(as.integer(unique(data[,tgt])))
  classes <- levels(data[,tgt])

  # When Cl is "all" or "smaller" the Cl and otherCl classes must be determined.
  # Alternatively the user may chose the Cl class(es) and then only otherCl is determined.
  
  if(Cl[1]=="smaller"){
    Cl <-names(which(table(data[,tgt])<nrow(data)/nrCl))
    otherCl <- setdiff(classes, Cl)
  }else{

    otherCl <-levels(data[,tgt])[which(!levels(data[,tgt])%in%Cl)]
  }
  
  if(!length(Cl)){
    res <- EnnClassif(form, data, k, dist, p, use.at)
    warning("Only ENN applied because no minority class was detected!")
    return(res)
  }


  # first remove noisy data with ENN on every classes except the important ones (Cl)
  # ENN can not be applied when only one class is in the less important group
  if(length(otherCl)>1){
  A1 <- EnnClassif(form, data[which(data[,tgt] %in% otherCl),], k, dist, p, use.at)
  } else{
    A1 <- list(data[which(data[,tgt] %in% otherCl),],c())
  }
  
  
  originalDat.Cl <- subset(data, data[,tgt] %in% Cl)
  #newDat <- rbind(originalDat.Cl, A1[[1]])
  #data <- newDat
  
  # now clean the neighbourhood of the important class(es)
  neig <-neighbours(tgt, data, dist, p, k, use.at)
  
  # construct the classes in otherCl that can be further under-sampled
  minVal <- min(sapply(Cl, function(x)length(which(data[,tgt]==x)))) 
  To.rem <- names(which(sapply(otherCl, function(x) length(which(data[,tgt]==x)))>0.5*minVal))

  rm.idx <- c()
  for (class in Cl){
    ind <- which(data[,tgt]==class)
    for (i in ind){
      # remove the neighbours of index i if all the k neighbours have a != tgt class and the neighbours 
      # belong to a class with at least 1/2 of the smaller important class
      if(sum(data[neig[i,],tgt]!=class)==k){
        for(indNeig in neig[i,]){
          if(data[indNeig, tgt] %in% To.rem) rm.idx <- c(rm.idx, indNeig)
        }
      }
    } 
  }

  rm.idx <- unique(c(rm.idx, as.integer(rownames(A1[[1]][A1[[2]],]))))
  if(length(rm.idx)){
    res <- data[-rm.idx,]
  }else{
    res <-data
  }

  res
}



