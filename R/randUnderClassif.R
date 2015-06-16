# ===================================================
# Random undersampling strategy for multiclass problems.
# Basically randomly removes a percentage of cases of the class(es) 
# selected by the user. Alternatively, it can either balance all the 
# existing classes or it can "smoothly invert" the frequency
# of the examples in each class.
# Examples:
#   ir<- iris[-c(95:130),]
#   myunder.iris <- randUnderClassif(Species~., ir, list(setosa=0.5, versicolor=0.8))
#   undBalan.iris <- randUnderClassif(Species~., ir, "balance")
#   undInvert.iris <- randUnderClassif(Species~., ir, "extreme")
# 
#   library(DMwR)
#   data(algae)
#   C.perc=list(autumn=1, summer=0.9, winter=0.4) # classes autumn and spring remain unchanged
#   myunder.algae <- randUnderClassif(season~., algae, C.perc)
#   undBalan.algae <- randUnderClassif(season~., algae, "balance")
#   undInvert.algae <- randUnderClassif(season~., algae, "extreme")
#   
#   library(MASS)
#   data(cats)
#   myunder.cats <- randUnderClassif(Sex~., cats, list(M=0.8))
#   undBalan.cats <- randUnderClassif(Sex~., cats,"balance")
#   undInvert.cats <- randUnderClassif(Sex~., cats, "extreme")
# 
# P. Branco, Mar 2015
# ---------------------------------------------------
randUnderClassif <- function(form, data, C.perc="balance", repl=FALSE)
  # INPUTS:
  # form a model formula
  # data the original training set (with the imbalanced distribution)
  # C.perc is a named list containing each class under-sampling percentage(between 0 and 1).
  #       The user may only provide the classes where he wants to apply under-sampling.
  #       Alternatively it may be "balance" (the default) or "extreme", cases where 
  #       the under-sampling percentages are automatically estimated
  # repl is it allowed or not to perform sampling with replacement
  
{
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  names <- sort(unique(data[,tgt]))
  li <-class.freq(data, tgt)
  
  if(is.list(C.perc)){ # the under-sampling percentages are provided by the user
    if(any(C.perc>1)){
      stop("percentages provided must be less than 1 to perform under-sampling!")
    }
    names.und <- names(which(C.perc<1))

    # include examples from classes unchanged
    newdata <- data[which(data[,tgt] %in% names[which(!(names %in% names.und))]),]
  
    for(i in 1:length(names.und)){ # under-sampling each class provided
      Exs <- which(data[,tgt]== names.und[i])
      sel <- sample(Exs,
                    as.integer(C.perc[[names.und[i]]]*length(Exs)),
                    replace=repl)
      newdata <- rbind(newdata,data[sel,])
    }
  }else if (C.perc=="balance"){ # the under-sampling percentages must be calculated

      minCl <- names(which(table(data[,tgt])==min(table(data[,tgt]))))
      
      if(length(minCl)==length(names)){
        stop("Classes are already balanced!")
      }
      
      # add the cases of the minority classes
      minExs <- which(data[,tgt] %in% minCl)
      newdata <- data[minExs,]
      names.und <- names[which(!(names %in% minCl))]
      
      # under-sample all the other classes
      for(i in 1:length(names.und)){ 
        Exs <- which(data[,tgt]== names.und[i])
        sel <- sample(Exs,
                      as.integer(li[[2]][as.numeric(match(minCl, names))[1]]),
                      replace=repl)
        newdata <- rbind(newdata,data[sel,])
      }      
    }else if (C.perc=="extreme"){ #"reverse" the classes frequencies (freq.min^2/freq. each class)
      
      minCl <- names(which(table(data[,tgt])==min(table(data[,tgt]))))
      
      if(length(minCl)==length(names)){
        stop("Classes are balanced. Unable to reverse the frequencies!")
      }
      
      # add the cases of the minority classes
      minExs <- which(data[,tgt] %in% minCl)
      newdata <- data[minExs,]
      names.und <- names[which(!(names %in% minCl))]
      
      # under-sample all the other classes reversing frequencies 
      for(i in 1:length(names.und)){ 
        Exs <- which(data[,tgt]== names.und[i])
        sel <- sample(Exs,
                      as.integer((li[[2]][as.numeric(match(minCl, names))[1]])^2/li[[2]][as.numeric(match(names.und[i], names))]),
                      replace=repl)
        newdata <- rbind(newdata,data[sel,])
      }      
      
    } else{
      stop("Please provide a list with classes to under-sample or 'balance' or 'extreme'.")
    }
  newdata
}

# ===================================================
# Auxiliar function which returns a list with the classes names and frequency of a data set
# P.Branco, Mar 2015
# ---------------------------------------------------

class.freq <- function(data, tgt){
  names <- sort(unique(data[,tgt]))
  li <- list(names, sapply(names, function(x)length(which(data[,tgt] == names[x]))))
  li
}





  