## ===================================================
## Creating a new training sample for regression problems
# based on the relevance function to perform both over and undersampling
# 
# Examples:
# IS.ext <-ImpSampRegress(a7~., clean.algae, rel="auto", thr.rel=0.7, C.perc="extreme")
# IS.bal <-ImpSampRegress(a7~., clean.algae, rel="auto", thr.rel=0.7, C.perc="balance")
# myIS <-ImpSampRegress(a7~., clean.algae, rel="auto", thr.rel=0.7, C.perc=list(0.2,6))
# # neither threshold nor C.perc defined
# IS.auto <- ImpSampRegress(a7~., clean.algae, rel="auto")
# 
# P. Branco, May 2015
# ---------------------------------------------------
ImpSampRegress <- function(form, data, rel="auto", thr.rel=NA, C.perc="balance")
  
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # rel is the relevance determined automatically (default: "auto") with uba 
  #       package or provided by the user through a matrix.
  # thr.rel is the relevance threshold above which a case is considered
  #       as belonging to the rare "class". If thr.rel is NA then both under- and 
  #       over-sampling are applied sampling examples accordingly to the relevance 
  #       of the target variable (oversampling) or accordingly to 1-phi of the 
  #       target (udersampling)
  # C.perc is a list containing the percentage of under- or/and 
  #       over-sampling to apply to each "class" obtained with the threshold. To use 
  #       this list, a thr.rel must be provided otherwise this parameter is ignored.
  #       The over-sampling percentage means that the examples above the threshold
  #       are increased by this percentage. The undersampling percentage means 
  #       that the normal cases (cases below the threshold) are undersampled by 
  #       this percentage. Alternatively it may be "balance" or "extreme",
  #       cases where the sampling percentages are automatically estimated.
{
  require(uba, quietly=TRUE)
  require(DMwR, quietly=TRUE)
  
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  
  y <- resp(form,data)
  if (is.matrix(rel)){ 
    pc <- phi.control(y, method="range", control.pts=rel)
  }else if(rel=="auto"){
    pc <- phi.control(y, method="extremes")
  }  else{ # TODO: handle other relevance functions and not using the threshold!
    stop("future work!")
  }
    
  
  if(!is.na(thr.rel)){
    s.y <- sort(y)
    temp <- y.relev <- phi(s.y,pc)
    
    temp[which(y.relev>thr.rel)] <- -temp[which(y.relev>thr.rel)]
    bumps <- c()
    for(i in 1:(length(y)-1)){if(temp[i]*temp[i+1]<0) bumps <- c(bumps,i)}
    nbump <- length(bumps)+1 # number of different classes
    # collect the indexes in each "class"
    count <- 1
    obs.ind <- as.list(rep(NA, nbump))
    base <- s.y[1]
    last <- y.relev[1]
    for(i in 2:length(s.y)){
      if((last <= thr.rel & y.relev[i] <= thr.rel) | (last > thr.rel & y.relev[i] > thr.rel)){
        base <- c(base,s.y[i])
        last <- y.relev[i]
      } else{
        obs.ind[[count]] <- base
        last <- y.relev[i]
        base <- s.y[i]
        count <- count+1
      }
    }
    obs.ind[[count]] <- base
    
    newdata <- data.frame()
    
    if(is.list(C.perc)){
      if(length(C.perc)!= nbump) stop("The percentages provided must be the same length as the number of bumps!")
    }else if(C.perc == "balance"){ # estimate the percentages of over/under sampling
      B <- round(nrow(data)/nbump,0)
      C.perc <- B/sapply(obs.ind, length)        
    } else if(C.perc == "extreme"){
      B <- round(nrow(data)/nbump,0)
      rescale <- nbump*B/sum(B^2/sapply(obs.ind,length))
      obj <- round((B^2/sapply(obs.ind, length))*rescale,2)
      C.perc <- round(obj/sapply(obs.ind, length),1)
    }
    
    for(i in 1:nbump){
      if(C.perc[[i] ]== 1){
        newdata <- rbind(newdata, data[names(obs.ind[[i]]),])
      }else if(C.perc[[i]]>1){
        s <- sample(names(obs.ind[[i]]), as.integer(C.perc[[i]]*length(obs.ind[[i]])), 
                    replace=TRUE, prob=y.relev[which(s.y %in% obs.ind[[i]])])

        newdata <- rbind(newdata, data[s,])
        
      }else if(C.perc[[i]]<1){
        s <- sample(names(obs.ind[[i]]), as.integer(C.perc[[i]]*length(obs.ind[[i]])),
                    replace=TRUE, prob=1-y.relev[which(s.y %in% obs.ind[[i]])])

        newdata <- rbind(newdata, data[s,])
        
      }
    }
  }
  
  if(is.na(thr.rel)){
    
    y.relev <- phi(y,pc)
    
    zero <- which(y.relev == 0)
    s.ove <- sample(setdiff(1:nrow(data), zero), as.integer(0.5*nrow(data)), replace=TRUE, prob=y.relev[-zero])
    
    one <- which(y.relev == 1)
    s.und <- sample(setdiff(1:nrow(data),one), as.integer(0.5*nrow(data)), replace=TRUE, prob=1-y.relev[-one])
    
    newdata <- rbind(data[s.ove,], data[-s.und,])
  }

  newdata
}
