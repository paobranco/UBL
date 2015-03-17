# ===================================================
# Learning a model using a random undersampling strategy,
# which basically removes cases of the negative
# class until a certain percentage of the positive class
# examples is reached
# P. Branco, Mar 2015
# ---------------------------------------------------
RandUnderClassif <- function(form, data, perc.under=1, repl=FALSE)
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # perc.under is the number of negative class cases that are randomly
  #                selected for each positive class case
  # repl is it allowed to perform sampling with replacement
  
{

  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  minCl <- names(which.min(table(data[,tgt])))
  
  # get the cases of the minority class
  minExs <- which(data[,tgt] == minCl)
  
  # start by adding all the cases of the minority class
  newdata <- data[minExs,]
  
  # test if it is possible to under-sample with the required conditions
  if(perc.under*length(minExs)>nrow(data[,-minExs])){
    stop("The conditions set can not be satisfied!")
  }
  
  # get the undersample of the "majority class" examples
  sel.maj <- sample((1:NROW(data))[-minExs],
                    as.integer((perc.under)*nrow(newdata)),
                    replace=repl)
  
  newdata <- rbind(newdata,data[sel.maj,])
  newdata
}

  