# ===================================================
# Tomek Links are can be used as an undersampling strategy. Two examples form a 
# Tomek link if their class label is different and they are each other nearest 
# neighbour. After find the existing Tomek links two different undersampling 
# strategies can be applied: to remove only the example of the larger class, 
# or to discard both examples. TomekClassif is suitable for multiclass problems.
# Examples:
# 
#   ir<- iris[-c(95:130),]
#   ir1norm <- TomekClassif(Species~., ir, dist="p-norm", p=1, Cl="all")
#   irMan <- TomekClassif(Species~., ir, dist="Manhattan", Cl="all", rem="maj") 
#   irEuc <- TomekClassif(Species~., ir)
#   irCheb <- TomekClassif(Species~., ir, dist="Chebyshev", Cl=c("virginica", "setosa"))
#   irChebAll <- TomekClassif(Species~., ir, dist="Chebyshev")
#   irChebMaj <- TomekClassif(Species~., ir, dist="Chebyshev", rem="maj")
#   irHVDM <- TomekClassif(Species~., ir, dist="HVDM")
#   summary(ir1norm[[1]]$Species)
#   summary(irMan[[1]]$Species)
#   summary(irEuc[[1]]$Species)
#   summary(irCheb[[1]]$Species)
#   summary(irChebAll[[1]]$Species)
#   summary(irChebMaj[[1]]$Species)
#   summary(irHVDM[[1]]$Species)
# 
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   alg.HVDM1 <- TomekClassif(season~., clean.algae, dist="HVDM", Cl=c("winter", "spring"), rem="both")
#   alg.HVDM2 <- TomekClassif(season~., clean.algae, dist="HVDM", rem="maj")
#   # removes only examples from class summer which are the majority class in the link
#   alg.EuM <- TomekClassif(season~., clean.algae, Cl="summer", rem="maj") 
#   # removes only examples from class summer in every link they appear
#   alg.EuB <- TomekClassif(season~., clean.algae, Cl="summer", rem="both") 
#   summary(clean.algae$season)
#   summary(alg.HVDM1[[1]]$season)
#   summary(alg.HVDM2[[1]]$season)
#   summary(alg.EuM[[1]]$season)
#   summary(alg.EuB[[1]]$season)
#   P. Branco, April 2015
# ---------------------------------------------------
TomekClassif <- function(form, data, dist="Euclidean", p=2, use.at="all", Cl="all", rem="both")
  
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # dist represents the distance function to be used for the kNN 
  #       computation
  # p a parameter used when the dist is set to "p-norm" which represents the used p.
  # use.at which attributes should be used for determining the neighbours. Defaults to "all",
  #       but it can be set to "numeric" or "nominal" meaning that only that type of attributes
  #       should e used in the neighbours computation.
  # Cl is a vector with the names of the classes that should be under-sampled. Defaults to "all" 
  #       meaning that all classes are considered for undersampling.
  # rem represents the under-sampling technique applied. When set to "both", both 
  #       examples will be remove; when set to "maj" only the link that belongs to the majority 
  #       class (or the class(es) provided in Cl) is removed. When "both" is selected and only 
  #       one example in the Tomek Link belongs to a class selected to be undersampled, then  
  #       only that example is removed.
  #
  # OUTPUTS:
  # a list with the cleaned dataframe and the indexes of the examples removed 
  
{
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))

  ClCode <-as.integer(as.factor(levels(data[,tgt])))
  ClChar <- levels(data[,tgt])
  Clcount <- as.vector(table(data[,tgt]))
  
  if(Cl[[1]]== "all"){
  rm.code <- ClCode
  
  } else{
   rm.code <- as.integer(factor(Cl, levels=ClChar)) 
  }

 
    # prepare .Fortran
    neig <- neighbours(tgt, data, dist, p, k=1, use.at=use.at)
    TL.idx <- matrix(nrow=0,ncol=4)
  for(i in 1:nrow(neig)){if(neig[neig[i,],]==i & !(neig[i,] %in% TL.idx[,1]) & data[i,tgt] !=data[neig[i,],tgt]) TL.idx <- rbind(TL.idx, c(i, neig[i,], data[i,tgt], data[neig[i,], tgt]))}
  
  rm.idx <- c()
  
  for(i in 1:nrow(TL.idx)){
    tr <- which(TL.idx[i,3:4] %in% rm.code)
    if(rem =="both" & length(tr)){
      rm.idx <- c(rm.idx, TL.idx[i,tr])
    }else if(length(tr)){ # rem=="maj"
      # only one of the examples is candidate for removal but we must check if it belongs to the larger class
      if(length(tr)==1){
        if (which.max(Clcount[TL.idx[i,3:4]])==tr) rm.idx <- c(rm.idx, TL.idx[i, tr])
      } else{# two candidate examples but only one can be removed: chech which one belongs to the larger class
        rm.idx <- c(rm.idx, TL.idx[i,which.max(Clcount[TL.idx[i,3:4]])]) 
      }
    }
  }
    
  
  
    if(length(rm.idx)){
    res <- data[-rm.idx,]
    }else{
      warning("There are no examples to remove!")
      res <-data
    }

  return(list(res, rm.idx))
}
