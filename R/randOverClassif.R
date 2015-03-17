# ===================================================
# Learning a model using a random oversampling strategy,
# which basically adds existing cases (copies) of the positive
# class examples until a certain percentage of the negative class
# examples is reached
# P. Branco, Mar 2015
# ---------------------------------------------------

RandOverClassif <- function(form, data, perc.over=1)
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # perc.over is the number of positive class cases that are randomly
  #                selected for each negative class case
  
{
    
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  minCl <- names(which.min(table(data[,tgt])))
  
  # get the cases of the minority class
  minExs <- which(data[,tgt] == minCl)
  majExs <- (1:NROW(data))[-minExs]
  # start by adding all the cases of the original cases
  newdata <- data
  
  # test if it is possible to under-sample with the required conditions
  if(perc.over*length(majExs)-length(minExs)<=0){
    stop("The conditions set can not be satisfied!")
  }
  
  # get the replicas of the minority class examples
  rep.min <- sample(minExs,
                    as.integer((perc.over)*length(majExs)-length(minExs)),
                    replace=TRUE)
  
  newdata <- rbind(newdata,data[rep.min,])
  newdata
}


  
}