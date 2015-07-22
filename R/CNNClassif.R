## ===================================================
## Condensed Nearest Neighbours strategy for multiclass imbalanced problems.
# 
# Examples:
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   myCNN <- CNNClassif(season~., clean.algae, Cl=c("summer", "spring", "winter"), dist="HEOM")
#   CNN1 <- CNNClassif(season~., clean.algae, Cl="smaller", dist="HEOM")
#   CNN2<- CNNClassif(season~., clean.algae, Cl="summer", dist="HEOM")
#   summary(myCNN[[1]]$season)
#   summary(CNN1[[1]]$season)
#   summary(CNN2[[1]]$season)
#   ir<- iris[-c(95:130),]
#   myCNN.iris <- CNNClassif(Species~., ir, Cl=c("setosa", "virginica"))
#   CNN.iris1 <- CNNClassif(Species~., ir, Cl="smaller")
#   CNN.iris2 <- CNNClassif(Species~., ir, Cl="versicolor")
#   summary(ir$Species)
#   summary(myCNN.iris[[1]]$Species)
#   summary(CNN.iris1[[1]]$Species)
#   summary(CNN.iris2[[1]]$Species)
# 
# 
#   library(MASS)
#   data(cats)
#   CNN.catsF <- CNNClassif(Sex~., cats, Cl="F")
#   CNN.cats <- CNNClassif(Sex~., cats, Cl="smaller")
# 
## P.Branco, April 2015
## ---------------------------------------------------
CNNClassif <- function(form, data, dist="Euclidean", p=2, Cl="smaller")
#   
#   INPUTS:
#   form a model formula
#   data the original training set (with the imbalanced distribution)
#   dist represents the distance function to be used for the kNN computation
#   p a parameter used when the dist is set to "p-norm" which represents the used p.
#   Cl is a vector with the names of the more important classes. Defaults to "smaller" which
#         automatically decides which are the relevant classes. In this case, all the classes
#         that have frequency below #examples/#classes are considered important.

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
  test <- which(!(rownames(data) %in% rownames(C.I)))
  ad.idx <- c()
  
  for (i in 1:length(test)){

    neig <- neighbours(tgt, rbind(C.I, data[test[i],]), dist, p, k=1)
    d.n <- c(rownames(C.I),rownames(data[test[i],]))
    if(data[d.n[neig[nrow(neig)]], tgt]!=data[test[i],tgt]){
      ad.idx <- c(ad.idx, test[i])
    }
  }
  
  C.I <- rbind(C.I, data[ad.idx,])
  return(list(C.I, Cl, otherCl))
  
}
