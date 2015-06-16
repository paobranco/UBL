# ===================================================
# Edited Nearest Neighbours (ENN) rule is an undersampling strategy 
# based in a cleaning technique which was developed by Wilson & Martinez, 1972.
# EnnClassif is suitable for multiclass problems.
# Basically it removes cases (possibily from all classes) 
# which do not agree with the majority of the k neighbours.
# The Cl parameter allows for the user to specify which particular 
# classes should be under-sampled.
# Examples:
# 
#   ir<- iris[-c(95:130),]
#   ir1norm <- ENNClassif(Species~., ir, k=5, dist="p-norm", p=1, Cl="all")
#   irManhat <- ENNClassif(Species~., ir, k=5, dist="Manhattan", Cl="all") 
#   irEucl <- ENNClassif(Species~., ir)
#   irCheby <- ENNClassif(Species~., ir, k=7, dist="Chebyshev", Cl=c("virginica", "setosa"))
#   irChebyAll <- ENNClassif(Species~., ir, k=7, dist="Chebyshev")
#   irHVDM <- ENNClassif(Species~., ir, k=3, dist="HVDM")
# 
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   alg.HVDMWinter <- ENNClassif(season~., clean.algae, k=3, dist="HVDM", Cl="winter")
#   alg.HVDM <- ENNClassif(season~., clean.algae, k=1, dist="HVDM")
# # the data set constains nominal and numeric attributes, therefore if the Euclidean distance 
# # is used (the default) and error will occur
#   ENNClassif(season~., clean.algae, k=3)
#   alg.HEOM <- ENNClassif(season~., clean.algae, k=5, dist="HEOM", Cl=c("winter", "summer"))
#   summary(clean.algae$season)
#   summary(alg.HVDMWinter[[1]]$season)
#   summary(alg.HVDM[[1]]$season)
#   summary(alg.HEOM[[1]]$season)

# P. Branco, Mar 2015
# ---------------------------------------------------
ENNClassif <- function(form, data, k=3, dist="Euclidean", p=2, Cl="all")
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # k is the number of neighbours considered (should be odd to avoid ties)
  # dist represents the distance function to be used for the kNN 
  #       computation
  # p a parameter used when the dist is set to "p-norm" which represents the used p.
  # Cl is a vector with the names of classes that should be under-sampled. 
  #       The default is "all" meaning that examples from all the
  #       existing classes can be removed
  # OUTPUTS:
  # a list with the cleaned dataframe and the indexes of the examples removed 
  
{
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  nom.at <- which(sapply(data[,-tgt], is.numeric)==FALSE)
#   if (length(nom.at) & (dist=="Euclidean" | dist=="p-norm" | dist=="Chebyshev") & use.at!="numeric"){
#     stop("Can not compute the distance with nominal attributes!")
#   }
  
  if(!length(unique(data[,tgt]))){
    stop("The data set contains only one class!")
  }
  

    neig <- neighbours(tgt, data, dist, p, k)
    rm.idx <- c()
    
    if(Cl[1]=="all"){
      for(i in 1:nrow(data)){
        if(sum(data[neig[i,],tgt]!=data[i,tgt])>k/2){
          rm.idx <- c(rm.idx, i)
        }
      }
    }else if(length(Cl)){
      for(i in Cl){ 
        for(j in which(data[,tgt]==i)){
          if(sum(data[neig[j,],tgt]!=i)>k/2){
            rm.idx <- c(rm.idx, j)
          }
        }
      }
    }
    
    if(length(rm.idx)){
    res <- data[-rm.idx,]
    }else{
      warning("There are no examples to remove!")
      res <-data
    }

  # to ensure that all the classes have at least one example in the final data set  
  if(length(unique(data[,tgt]))>length(unique(res[,tgt]))){
    Cdif <- setdiff( unique(data[,tgt]),unique(res[,tgt]))
    ad.idx <- sapply(Cdif, function(x)sample(which(data[,tgt] %in% x),1))
    res <- rbind(res, data[ad.idx,])
    rm.idx <- setdiff(rm.idx,ad.idx)
  }

  return(list(res, rm.idx))
}
