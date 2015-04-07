# ===================================================
# Edited Nearest Neighbours (ENN) rule is an undersampling strategy 
# based in a cleaning technique which was developed by Wilson & Martinez, 1972.
# EnnClassif is suitable for multiclass problems.
# Basically it removes cases (possibily from all classes) 
# which do not agree with the majority of the k neighbours.
# The Cl parameter allows for the user to specify which particular 
# classes should be under-sampled.
# Examples:
#   ir<- iris[-c(95:130),]
#   ir1norm <- EnnClassif(Species~., ir, k=5, dist="p-norm", p=1, Cl="all")
#   irManhat <- EnnClassif(Species~., ir, k=5, dist="Manhattan", Cl="all") 
#   irEucl <- EnnClassif(Species~., ir)
#   irCheby <- EnnClassif(Species~., ir, k=7, dist="Chebyshev", Cl=c("virginica", "setosa"))
#   irChebyAll <- EnnClassif(Species~., ir, k=7, dist="Chebyshev")
#   irHVDM <- EnnClassif(Species~., ir, k=3, dist="HVDM")
# 
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   alg.HVDMWinter <- EnnClassif(season~., clean.algae, k=3, dist="HVDM", Cl="winter")
#   alg.HVDM <- EnnClassif(season~., clean.algae, k=1, dist="HVDM")
#   alg.3norm <- EnnClassif(season~., clean.algae, k=3, dist="p-norm", p=2, use.at="numeric")
#   alg.Eucl <- EnnClassif(season~., clean.algae, k=5, use.at="numeric", Cl=c("winter", "summer"))
#   summary(clean.algae$season)
#   summary(alg.HVDMWinter$season)
#   summary(alg.HVDM$season)
#   summary(alg.Eucl$season)
#   summary(alg.3norm$season)
# P. Branco, Mar 2015
# ---------------------------------------------------
EnnClassif <- function(form, data, k=3, dist="Euclidean", p=2, use.at="all", Cl="all")
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # k is the number of neighbours considered (should be odd to avoid ties)
  # dist represents the distance function to be used for the kNN 
  #       computation
  # p a parameter used when the dist is set to "p-norm" which represents the used p.
  # use.at which attributes should be used for determining the neighbours. Defaults to "all",
  #       but it can be set to "numeric" or "nominal" meaning that only that type of attributes
  #       should e used in the neighbours computation.
  # Cl is a vector with the names of classes that should be under-sampled. 
  #       The default is "all" meaning that examples from all the
  #       existing classes can be removed
  # OUTPUTS:
  # a cleaned dataframe
  
{
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  nom.at <- which(sapply(data[,-tgt], is.numeric)==FALSE)
  if (length(nom.at) & (dist=="Euclidean" | dist=="p-norm" | dist=="Chebyshev") & use.at!="numeric"){
    stop("Can not compute the distance with nominal attributes!")
  }
  
  if(!length(unique(data[,tgt]))){
    stop("The data set contains only one class!")
  }
  
#  for(r in 1:rep){
    # prepare .Fortran
    neig <- neighbours(tgt, data, dist, p, k, use.at="all")
    rm.idx <- c()
    
    if(Cl[1]=="all"){
      for(i in 1:nrow(data)){
        if(sum(data[neig[i,],tgt]!=data[i,tgt])>k/2){
          rm.idx <- c(rm.idx, i)
        }
      }
    }else if(length(Cl)){
      for(i in Cl){ 
        #Cl.neig <- neig[which(data[,tgt]==i),]
        for(j in which(data[,tgt]==i)){
          if(sum(data[neig[j,],tgt]!=i)>k/2){
            rm.idx <- c(rm.idx, j)
          }
        }
      }
    }
    
    if(length(rm.idx)){
    res <- data[-rm.idx,]
    data <- res
    }else{
      warning("There are no examples to remove!")
      res <-data
    }
#  }
  res
}
