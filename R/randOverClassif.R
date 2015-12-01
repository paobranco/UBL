# ===================================================
# Random oversampling strategy for multiclass problems.
# Basically randomly copies a percentage of cases of the class(es) 
# selected by the user. Alternatively, it can either balance all the 
# existing classes or it can "smoothly invert" the frequency
# of the examples in each class
# Examples:
#   ir<- iris[-c(95:130),]
#   myover.iris <- randOverClassif(Species~., ir, list(versicolor=1.2, virginica=2.3))
#   oveBalan.iris <- randOverClassif(Species~., ir, "balance")
#   oveInvert.iris <- randOverClassif(Species~., ir, "extreme")
# 
#   library(DMwR)
#   data(algae)
#   C.perc=list(autumn=2, summer=1.5, spring=1) # classes spring and winter remain unchanged
#   myover.algae <- randOverClassif(season~., algae, C.perc)
#   oveBalan.algae <- randOverClassif(season~., algae, "balance")
#   oveInvert.algae <- randOverClassif(season~., algae, "extreme")
#   
#   library(MASS)
#   data(cats)
#   myover.cats <- randOverClassif(Sex~., cats, list(M=1.5))
#   oveBalan.cats <- randOverClassif(Sex~., cats,"balance")
#   oveInvert.cats <- randOverClassif(Sex~., cats, "extreme")
#
# P. Branco, Mar 2015
# ---------------------------------------------------
randOverClassif <- function(form, data, C.perc="balance", repl=TRUE)
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # C.perc is a named list containing each class over-sampling percentage(>=1).
  #       The user may only provide the classes where he wants to apply over-sampling.
  #       Alternatively it may be "balance" or "extreme", cases where 
  #       the over-sampling percentages are automatically estimated
  # repl is it allowed or not to perform sampling with replacement
  #       defaults to TRUE because if the over-sampling percentage is 
  #       >2 this is necessary.
  
{
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  names <- sort(unique(data[,tgt]))
  li <-class.freq(data, tgt)
  
  # include base examples (i.e., the original data set)
  newdata <- data
  
  if(is.list(C.perc)){ # the over-sampling percentages are provided by the user
    if(any(C.perc<1)){
      stop("percentages provided must be higher than 1 to perform over-sampling!")
    }
    names.ove <- names(which(C.perc>1))
    
    for(i in 1:length(names.ove)){ # over-sampling each class provided
      Exs <- which(data[,tgt]== names.ove[i])
      sel <- sample(Exs,
                    as.integer((C.perc[[names.ove[i]]]-1)*length(Exs)),
                    replace=repl)
      newdata <- rbind(newdata,data[sel,])
    }
  }else if (C.perc=="balance"){ # the over-sampling percentages must be calculated
    
    majCl <- names(which(table(data[,tgt])==max(table(data[,tgt]))))
    
    if(length(majCl)==length(names)){
      stop("Classes are already balanced!")
    }
    
    names.ove <- names[which(!(names %in% majCl))]
    
    # over-sample all the other classes
    for(i in 1:length(names.ove)){ 
      Exs <- which(data[,tgt]== names.ove[i])
      sel <- sample(Exs,
                    as.integer(li[[2]][as.numeric(match(majCl, names))[1]]-li[[2]][as.numeric(names.ove[i])]),
                    replace=repl)
      newdata <- rbind(newdata,data[sel,])
    }      
  }else if (C.perc=="extreme"){ #"reverse" the classes frequencies(fre.maj^2/freq.each class)
    
    majCl <- names(which(table(data[,tgt])==max(table(data[,tgt]))))
    
    if(length(majCl)==length(names)){
      stop("Classes are balanced. Unable to reverse the frequencies!")
    }
    

    names.ove <- names[which(!(names %in% majCl))]
    
    # oveer-sample all the other classes reversing frequencies
    for(i in 1:length(names.ove)){ 
      Exs <- which(data[,tgt]== names.ove[i])
      sel <- sample(Exs,
                    round((li[[2]][as.numeric(match(majCl, names))[1]])^2/li[[2]][as.numeric(match(names.ove[i], names))]-li[[2]][as.numeric(match(names.ove[i], names))],0),
                    replace=repl)
      newdata <- rbind(newdata,data[sel,])
    }      
    
  } else{
    stop("Please provide a list with classes to under-sample or 'balance' or 'extreme'.")
  }
  newdata
}
