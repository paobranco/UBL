## ===================================================
## Condensed Nearest Neighbours strategy for multiclass imbalanced problems.
# 
# Examples:
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   myCNN <- CNNClassif(season~., clean.algae, Cl=c("summer", "spring", "winter"))
#   CNN1 <- CNNClassif(season~., clean.algae, Cl="smaller")
#   CNN2<- CNNClassif(season~., clean.algae, Cl="summer")
#   summary(myCNN$season)
#   summary(CNN1$season)
#   summary(CNN2$season)
#   ir<- iris[-c(95:130),]
#   myCNN.iris <- CNNClassif(Species~., ir, Cl=c("setosa", "virginica"))
#   CNN.iris1 <- CNNClassif(Species~., ir, Cl="smaller")
#   CNN.iris2 <- CNNClassif(Species~., ir, Cl="versicolor")
# 
# 
#   library(MASS)
#   data(cats)
#   CNN.catsF <- CNNClassif(Sex~., cats, Cl="F")
#   CNN.cats <- CNNClassif(Sex~., cats, Cl="smaller")
# 
## P.Branco, April 2015
## ---------------------------------------------------
CNNClassif <- function(form, data, dist="Euclidean", p=2, use.at= "all", Cl="smaller")
  
  # INPUTS:
  # form a model formula
  # data the original training set (with the imbalanced distribution)
  # dist represents the distance function to be used for the kNN computation
  # p a parameter used when the dist is set to "p-norm" which represents the used p.
  # use.at which attributes should be used for determining the neighbours. Defaults to "all",
  #       but it can be set to "numeric" or "nominal" meaning that only that type of attributes
  #       should e used in the neighbours computation.
  # Cl is a vector with the names of the more important classes. Defaults to "smaller" which
  #       automatically decides which are the relevant classes. In this case, all the classes
  #       that have frequency below #examples/#classes are considered important.

{
  if(any(is.na(data))){
    stop("The data set provided contains NA values!")
  }
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  
  classes <- levels(data[,tgt])
  nrCl <- length(classes)
  
  if(Cl[[1]]=="smaller"){ # define which is(are) the important class(es)
    Cl <-names(which(table(data[,tgt])<nrow(data)/nrCl))
  }
  otherCl <-setdiff(levels(data[,tgt]), Cl)   
  
  # construct a set with all examples from important classes and one random example from the other classes
  C.I <-data[data[,tgt] %in% Cl,]
  for(i in 1:length(otherCl)){
    C.I <- rbind(C.I, data[sample(which(data[,tgt] %in% otherCl[i]),1),])
  }
    
  neig <- neighbours(tgt, data, dist, p, k=1, use.at=use.at)
  test <- which(!(rownames(data) %in% rownames(C.I)))
  ad.idx <- c()
  
  for (i in 1:length(test)){
    if(data[neig[test[i],], tgt]!=data[test[i],tgt])ad.idx <- c(ad.idx, test[i])
  }
  
  C.I <- rbind(C.I, data[ad.idx,])
  return(list(C.I, Cl, otherCl))
  
}
