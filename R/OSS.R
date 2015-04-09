# ===================================================
# One-sided selection strategy for multiclass imbalanced problems.
#
# Examples:
#   ir<- iris[-c(95:130),]
#   ir1 <- OSSClassif(Species~., ir, dist="HVDM")
#   ir2 <- OSSClassif(Species~., ir, dist="p-norm", p=3, Cl="virginica")
#   ir3 <- OSSClassif(Species~., ir)
#   summary(ir1$Species)
#   summary(ir2$Species)
#   summary(ir3$Species)
# 
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   alg1 <- OSSClassif(season~., clean.algae, Cl=c("spring", "summer"))
#   alg2 <- OSSClassif(season~., clean.algae, dist="Euclidean", Cl="smaller")
#   alg3 <- OSSClassif(season~., clean.algae, dist="p-norm", p=1, use.at="numeric", Cl="winter")
#   summary(alg1$season)
#   summary(alg2$season)
#   summary(alg3$season)
#
# P. Branco, April 2015
# ---------------------------------------------------
OSSClassif <- function(form, data, dist="Euclidean", p=2, use.at="all", Cl="smaller") 
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # dist represents the distance function to be used for the kNN computation
  # p a parameter used when the dist is set to "p-norm" which represents the used p.
  # use.at which attributes should be used for determining the neighbours. Defaults to "all",
  #       but it can be set to "numeric" or "nominal" meaning that only that type of attributes
  #       should e used in the neighbours computation.
  # Cl is a vector with the names of the more important classes. Defaults to "smaller" which
  #       automatically decides which are the relevant classes. In this case, all the classes
  #       that have frequency below #examples/#classes are considered important.

{
  #  Obtain the reduced data set with CNN
  d1 <-CNNClassif(form, data, dist=dist, p=p, use.at=use.at, Cl=Cl)
  
  d2 <- TomekClassif(form, d1[[1]], dist=dist, p=p, use.at=use.at, Cl=d1[[3]], rem="both")
  
  return(d2[[1]])  
  
}
