# ===================================================
# One-sided selection strategy for multiclass imbalanced problems.
#
# Examples:
#   ir<- iris[-c(95:130),]
#   ir1 <- OSSClassif(Species~., ir, dist="HVDM")
#   ir2 <- OSSClassif(Species~., ir, dist="p-norm", p=3, Cl="virginica")
#   ir3 <- OSSClassif(Species~., ir, start="Tomek")
#   ir4 <- OSSClassif(Species~., ir)
#   summary(ir1$Species)
#   summary(ir2$Species)
#   summary(ir3$Species)
#   summary(ir4$Species)
# 
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   alg1 <- OSSClassif(season~., clean.algae, dist="HVDM", Cl=c("spring", "summer"))
#   alg2 <- OSSClassif(season~., clean.algae, dist="HEOM", Cl=c("spring", "summer"), start="Tomek")
#   alg3 <- OSSClassif(season~., clean.algae, dist="HVDM", start="CNN")
#   alg4 <- OSSClassif(season~., clean.algae, dist="HVDM", start="Tomek")
#   alg5 <- OSSClassif(season~., clean.algae, dist="HEOM", Cl="winter")
#   summary(alg1$season)
#   summary(alg2$season)
#   summary(alg3$season)
#   summary(alg4$season)
#   summary(alg5$season)
#
# P. Branco, April 2015
# ---------------------------------------------------
OSSClassif <- function(form, data, dist="Euclidean", p=2, Cl="smaller", start="CNN") 
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # dist represents the distance function to be used for the kNN computation
  # p a parameter used when the dist is set to "p-norm" which represents the used p.
  # Cl is a vector with the names of the more important classes. Defaults to "smaller" which
  #       automatically decides which are the relevant classes. In this case, all the classes
  #       that have frequency below #examples/#classes are considered important.
  # start is a string which determines which strategy (CNN or Tomek links) should be performed 
  #         first. If set to "CNN" (the default) this strategy will be performed first and  
  #         Tomek links are applied after. If set to "Tomek" the reverse order is applied.

{
  if(start=="CNN"){
  #  Obtain the reduced data set with CNN
  d1 <-CNNClassif(form, data, dist=dist, p=p, Cl=Cl)
  
  d2 <- TomekClassif(form, d1[[1]], dist=dist, p=p, Cl=d1[[3]], rem="both")
  
  return(d2[[1]])  
  } else if(start=="Tomek"){
    # the column where the target variable is
    tgt <- which(names(data) == as.character(form[[2]]))
    
    classes <- levels(data[,tgt])
    nrCl <- length(classes)
    
    if(Cl[[1]]=="smaller"){ # define which is(are) the important class(es)
      Cl <-names(which(table(data[,tgt])<nrow(data)/nrCl))
    }
    otherCl <-setdiff(levels(data[,tgt]), Cl)   
    
    d1 <- TomekClassif(form, data, dist=dist, p=p, Cl=otherCl, rem="both")
    
    d2 <- CNNClassif(form, d1[[1]], dist=dist, p=p, Cl=Cl)
    return(d2[[1]])
    
  } else{
    stop("start parameter must be CNN or Tomek!")
  }
}
