
## ===================================================
## Creating a SMOTE training sample for classification problems
# 
# Examples:
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   C.perc=list(autumn=2, summer=1.5, winter=0.9) # class spring remains unchanged
#   mysmote.algae <- smoteClassif(season~., clean.algae, C.perc)
#   smoteBalan.algae <- smoteClassif(season~., clean.algae, "balance")
#   smoteExtre.algae <- smoteClassif(season~., clean.algae, "extreme")
# 
#   ir<- iris[-c(95:130),]
#   mysmote.iris <- smoteClassif(Species~., ir, list(setosa=0.6, virginica=1.5))
#   smoteBalan.iris <- smoteClassif(Species~., ir, "balance")
#   smoteExtre.iris <- smoteClassif(Species~., ir, "extreme")
# 
# 
#   library(MASS)
#   data(cats)
#   mysmote.cats <- smoteClassif(Sex~., cats, list(M=0.8, F=1.8))
#   smoteBalan.cats <- smoteClassif(Sex~., cats,"balance")
#   smoteExtre.cats <- smoteClassif(Sex~., cats, "extreme")
#
## L. Torgo, Feb 2010, Nov 2014
## P.Branco, Mar 2015
## ---------------------------------------------------
smoteClassif <- function(form, data, C.perc, k=5, repl=FALSE)
  
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # C.perc  named list containing each class percentage of under- or 
  #       over-sampling to apply between 0 and 1. The user may provide
  #       only a subset of the existing classes where sampling is to
  #       be applied. Alternatively it may be "balance" or "extreme",
  #       cases where the sampling percentages are automatically estimated.
  # k is the number of neighbours to consider as the pool from where
  #       the new examples are generated
  # repl is it allowed to perform sampling with replacement (when under-sampling)

{
  if(any(is.na(data))){
    stop("The data set provided contains NA values!")
  }
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  names <- sort(unique(data[,tgt]))
  li <-class.freq(data, tgt)
  if (tgt < ncol(data)) {
    orig.order <- colnames(data)
    cols <- 1:ncol(data)
    cols[c(tgt,ncol(data))] <- cols[c(ncol(data),tgt)]
    data <-  data[,cols]
  }
  
  if(is.list(C.perc)){
    
    names.und <- names(which(C.perc<1))
    names.ove <- names(which(C.perc>1))
    names.same <- setdiff(names, union(names.und, names.ove))
    
    # include examples from classes unchanged
    newdata <- data[which(data[,ncol(data)] %in% names.same),]

    if(length(names.und)){  # perform under-sampling
      for(i in 1:length(names.und)){ 
        Exs <- which(data[,ncol(data)]== names.und[i])
        sel <- sample(Exs,
                      as.integer(C.perc[[names.und[i]]]*length(Exs)),
                      replace=repl)
        newdata <- rbind(newdata,data[sel,])
      }
    }
    
    if(length(names.ove)){ # perform over-sampling
      for(i in 1:length(names.ove)){
        newExs <- smote.exsClassif(data[which(data[,ncol(data)] == names.ove[i]),],
                                   ncol(data),
                                   C.perc[[names.ove[i]]],
                                   k)
        # add original rare examples and synthetic generated examples
        newdata <- rbind(newdata, newExs, data[which(data[,ncol(data)] == names.ove[i]),])
      }
      
    }
    if (tgt < ncol(data)) {
      newdata <- newdata[,cols]
      data <- data[,cols]
    }
    

  } else{
    
    if(C.perc=="balance"){  
      li[[3]]<- round(sum(li[[2]])/length(li[[2]]),0)-li[[2]]
    } else if(C.perc =="extreme"){
      med <- sum(li[[2]])/length(li[[2]])
      li[[3]] <- round(med^2/li[[2]]*sum(li[[2]])/sum(med^2/li[[2]]),0)-li[[2]]
    } else{
      stop("Please provide a list with classes to under/over-sample or 'balance' or 'extreme'.")
    }
    
    und <-which(li[[3]]<0) # classes to under-sample
    ove <- which(li[[3]]>0) #classes to over-sample
    same <- which(li[[3]]==0) # unchanged classes
    
    # include examples from classes unchanged
    newdata <- data[which(data[,ncol(data)] %in% li[[1]][same]),]
    
    if(length(und)){ #perform under-sampling
      for(i in 1:length(und)){ 
        Exs <- which(data[,ncol(data)]== li[[1]][und[i]])
        sel <- sample(Exs,
                      as.integer(li[[2]][und[i]]+li[[3]][und[i]]),
                      replace=repl)
        newdata <- rbind(newdata,data[sel,])
      }
    }
    
    if(length(ove)){ #perform over-sampling
     
      for(i in 1:length(ove)){
        newExs <- smote.exsClassif(data[which(data[,ncol(data)] == li[[1]][ove[i]]),],
                                   ncol(data),
                                   li[[3]][ove[i]]/li[[2]][ove[i]]+1,
                                   k)
        # add original rare examples and synthetic generated examples
        newdata <- rbind(newdata, newExs, data[which(data[,ncol(data)] == li[[1]][ove[i]]),])
      } 
    }
    
    if (tgt < ncol(data)) {
      newdata <- newdata[,cols]
      data <- data[,cols]
    }
  } 
  newdata
}


# ===================================================
# Obtain a set of smoted examples for a set of rare cases.
# L. Torgo, Feb 2010
# P.Branco, Mar 2015
# ---------------------------------------------------
smote.exsClassif <- function(data,tgt,N,k)
  # INPUTS:
  # data are the rare cases (the minority class cases)
  # tgt is the name of the target variable
  # N is the percentage of over-sampling to carry out;
  # and k is the number of nearest neighours to use for the generation
  # OUTPUTS:
  # The result of the function is a N*nrow(data) set of generated
  # examples with rare class on the target
{
  nomatr <- c()
  T <- matrix(nrow=dim(data)[1],ncol=dim(data)[2]-1)
  for(col in seq.int(dim(T)[2]))
    if (class(data[,col]) %in% c('factor','character')) {
      T[,col] <- as.integer(data[,col])
      nomatr <- c(nomatr,col)
    } else T[,col] <- data[,col]
  
  #if (N < 1) { # only a percentage of the T cases will be SMOTEd
  #  nT <- NROW(T)
  #  idx <- sample(1:nT,as.integer(N*nT))
  #  T <- T[idx,]
  #  N <- 1
  #}
  
  p <- dim(T)[2]
  nT <- dim(T)[1]
  
  ranges <- apply(T,2,max)-apply(T,2,min)
  
  nexs <-  as.integer(N-1) # nr of examples to generate for each rare case
  extra <- as.integer(nT*(N-1-nexs)) # the extra examples to generate
  idx <- sample(1:nT, extra)
  new <- matrix(nrow=nexs*nT+extra,ncol=p)    # the new cases
  if(nexs){
    for(i in 1:nT) {
    
      # the k NNs of case T[i,]
      xd <- scale(T,T[i,],ranges)
      for(a in nomatr) xd[,a] <- !xd[,a]==0
      dd <- drop(xd^2 %*% rep(1, ncol(xd)))
      kNNs <- order(dd)[2:(k+1)]
          
      for(n in 1:nexs) {
        # select randomly one of the k NNs
        neig <- sample(1:k,1)
      
        ex <- vector(length=ncol(T))
      
        # the attribute values of the generated case
        difs <- T[kNNs[neig],]-T[i,]
        new[(i-1)*nexs+n,] <- T[i,]+runif(1)*difs
        for(a in nomatr)
          new[(i-1)*nexs+n,a] <- c(T[kNNs[neig],a],T[i,a])[1+round(runif(1),0)]
      
      }
    }
  }
  if(extra){
    count<-1
    for (i in idx){
      
      # the k NNs of case T[i,]
      xd <- scale(T,T[i,],ranges)
      for(a in nomatr) xd[,a] <- xd[,a]==0
      dd <- drop(xd^2 %*% rep(1, ncol(xd)))
      kNNs <- order(dd)[2:(k+1)]
      
      # select randomly one of the k NNs
      neig <- sample(1:k,1)
      
      ex <- vector(length=ncol(T))
      
      # the attribute values of the generated case
      difs <- T[kNNs[neig],]-T[i,]
      new[nexs*nT+count,] <- T[i,]+runif(1)*difs
      for(a in nomatr)
        new[nexs*nT+count,a] <- c(T[kNNs[neig],a],T[i,a])[1+round(runif(1),0)]
      
      count <- count+1
    }
  }
  newCases <- data.frame(new)
  
  for(a in nomatr)
    newCases[,a] <- factor(newCases[,a],levels=1:nlevels(data[,a]),labels=levels(data[,a]))
  
  newCases[,tgt] <- factor(rep(data[1,tgt],nrow(newCases)),levels=levels(data[,tgt]))
  colnames(newCases) <- colnames(data)
  newCases
}


