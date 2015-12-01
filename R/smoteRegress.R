## ===================================================
## Creating a SMOTE training sample for regression problems
# 
# Examples:
#   library(DMwR)
#   data(algae)
#   clean.algae <- algae[complete.cases(algae),]
#   C.perc=list(0.1, 8) 
#   mysmote.alg <- smoteRegress(a7~., clean.algae, C.perc=C.perc)
#   smoteBal.alg <- smoteRegress(a7~., clean.algae, C.perc="balance")
#   smoteExt.alg <- smoteRegress(a7~., clean.algae, C.perc="extreme")
# 
#   ir<- iris[-c(95:130),]
#   mysmote.iris <- smoteRegress(Sepal.Width~., ir, dist="HEOM", C.perc=list(0.5,2.5))
#   mysmote.iris <- smoteRegress(Sepal.Width~., ir, dist="HEOM", C.perc=list(0.2,4), thr.rel=0.8)
#   smoteBalan.iris <- smoteRegress(Sepal.Width~., ir, dist="HEOM", C.perc="balance")
#   smoteExtre.iris <- smoteRegress(Sepal.Width~., ir, dist="HEOM", C.perc="extreme")
# 
#   rel <- matrix(0,ncol=3,nrow=0)
#   rel <- rbind(rel,c(2,1,0))
#   rel <- rbind(rel,c(3,0,0))
#   rel <- rbind(rel,c(4,1,0))
#
#   sP.ir <- smoteRegress(Sepal.Width~., ir, rel =rel, C.perc=list(4,0.5,4))
# 
# L. Torgo, Jun 2008
# P. Branco, Mar, Apr 2015
# ---------------------------------------------------
smoteRegress <- function(form, data, rel="auto", thr.rel=0.5, C.perc="balance",
                         k=5, repl=FALSE, dist="Euclidean", p=2)
  
  # INPUTS:
  # form a model formula
  # data the original training set (with the unbalanced distribution)
  # rel is the relevance determined automatically (default: "auto") with uba 
  #       package or provided by the user through a matrix. See examples.
  # thr.rel is the relevance threshold above which a case is considered
  #       as belonging to the rare "class"
  # C.perc is a list containing the percentage of under- or/and 
  #         over-sampling to apply to each "class" obtained with the threshold.
  #         The over-sampling percentage means that the examples above the threshold
  #         are increased by this percentage. The undersampling percentage means 
  #         that the normal cases (cases below the threshold) are undersampled by 
  #         this percentage. Alternatively it may be "balance" or "extreme",
  #         cases where the sampling percentages are automatically estimated.
  # k is the number of neighbours to consider as the pool from where
  #               the new generated examples are generated
  # repl is it allowed to perform sampling with replacement
  # dist is the distance measure to be used (defaults to "Euclidean")
  # p is a parameter used when a p-norm is computed
{
 
#  require(uba, quietly=TRUE)
  suppressWarnings(suppressPackageStartupMessages(library('uba')))
  
  if(any(is.na(data))){
    stop("The data set provided contains NA values!")
  }
  
  # the column where the target variable is
  tgt <- which(names(data) == as.character(form[[2]]))
  
  if (tgt < ncol(data)) {
    orig.order <- colnames(data)
    cols <- 1:ncol(data)
    cols[c(tgt,ncol(data))] <- cols[c(ncol(data),tgt)]
    data <-  data[,cols]
  }
  if(is.na(thr.rel)){
    stop("Future work!")
  }
  

  y <- resp(form,data)
  s.y <- sort(y)
  
  if (is.matrix(rel)){ 
    pc <- phi.control(y, method="range", control.pts=rel)
  }else if(is.list(rel)){ 
    pc <- rel
  }else if(rel=="auto"){
    pc <- phi.control(y, method="extremes")
  }else{# TODO: handle other relevance functions and not using the threshold!
    stop("future work!")
  }
  
  temp <- y.relev <- phi(s.y,pc)
  if(!length(which(temp<1)))stop("All the points have relevance 1. Please, redefine your relevance function!")
  if(!length(which(temp>0)))stop("All the points have relevance 0. Please, redefine your relevance function!")
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
  }else if(C.perc=="balance"){ # estimate the percentages of over/under sampling
    B <- round(nrow(data)/nbump,0)
    C.perc <- B/sapply(obs.ind, length)        
  } else if(C.perc == "extreme"){
    B <- round(nrow(data)/nbump,0)
    rescale <- nbump*B/sum(B^2/sapply(obs.ind,length))
    obj <- round((B^2/sapply(obs.ind, length))*rescale,2)
    C.perc <- round(obj/sapply(obs.ind, length),1)
  }
  
  for(i in 1:nbump){
    if(C.perc[[i]]==1){
      newdata <- rbind(newdata, data[names(obs.ind[[i]]),])
    }else if(C.perc[[i]]>1){
      newExs <- smote.exsRegress(data[names(obs.ind[[i]]),],
                                 ncol(data),
                                 C.perc[[i]],
                                 k,
                                 dist,
                                 p)
      # add original rare examples and synthetic generated examples
      newdata <- rbind(newdata, newExs, data[names(obs.ind[[i]]),])
      
    }else if(C.perc[[i]]<1){
      sel.maj <- sample(1:length(obs.ind[[i]]),
                        as.integer(C.perc[[i]]*length(obs.ind[[i]])),
                        replace=repl)
      newdata <- rbind(newdata, data[names(obs.ind[[i]][sel.maj]),])
      
    }
  }
  
  if (tgt < ncol(data)) {
    newdata <- newdata[,cols]
    data <- data[,cols]
  }
  
  newdata
}



# ===================================================
# Obtain a set of smoted examples for a set of rare cases.
#
# L. Torgo, Jun 2008
# P.Branco, Mar 2015
# ---------------------------------------------------
smote.exsRegress <- function(data, tgt, N, k, dist, p)
  # INPUTS:
  # data are the rare cases (the minority "class" cases)
  # tgt the column nr of the target variable
  # N is the percentage of over-sampling to carry out;
  # and k is the number of nearest neighours
  # dist is the distance function used for the neighours computation
  # p is an integer used when a "p-norm" distance is selected
  # OUTPUTS:
  # The result of the function is a (N-1)*nrow(data) set of generate
  # examples with rare values on the target
{
  
  nomatr <- c()
  T <- matrix(nrow=dim(data)[1],ncol=dim(data)[2])
  for(col in seq.int(dim(T)[2]))
    if (class(data[,col]) %in% c('factor','character')) {
      T[,col] <- as.integer(data[,col])
      nomatr <- c(nomatr,col)
    } else T[,col] <- data[,col]
  
  nC <- dim(T)[2]
  nT <- dim(T)[1]
  

  ranges <- rep(1,nC)
  if(length(nomatr)){
    for(x in (1:nC)[-c(nomatr)]) ranges[x] <- max(T[,x]) - min(T[,x])
  } else{
    for(x in (1:nC)) ranges[x] <- max(T[,x]) - min(T[,x])
  }

  kNNs <-neighbours(tgt, data, dist, p, k)
    
  nexs <-  as.integer(N-1) # nr of examples to generate for each rare case
  extra <- as.integer(nT*(N-1-nexs)) # the extra examples to generate
  idx <- sample(1:nT, extra)
  new <- matrix(nrow=nexs*nT+extra,ncol=nC)    # the new cases
 
  if(nexs){
    for(i in 1:nT) {
    
        
      for(n in 1:nexs) {
        # select randomly one of the k NNs
        neig <- sample(1:k,1)
      
        # the attribute values of the generated case
        difs <- T[kNNs[i,neig],-tgt]-T[i,-tgt]
        new[(i-1)*nexs+n,-tgt] <- T[i,-tgt]+runif(1)*difs
        for(a in nomatr) # nominal attributes are randomly selected among the existing values of seed and the selected neighbour 
          new[(i-1)*nexs+n,a] <- c(T[kNNs[i,neig],a],T[i,a])[1+round(runif(1),0)]
        
        # now the target value (weighted (by inverse distance) average)
        d1 <- d2 <- 0
        for(x in (1:nC)[-c(nomatr, tgt)]) {
          d1 <- abs(T[i,x] - new[(i-1)*nexs+n,x])/ranges[x]
          d2 <- abs(T[kNNs[i,neig],x] - new[(i-1)*nexs+n,x])/ranges[x]
        }
        if (length(nomatr)) {
          d1 <- d1 + sum(T[i,nomatr] != new[(i-1)*nexs+n,nomatr])
          d2 <- d2 + sum(T[kNNs[i,neig],nomatr] != new[(i-1)*nexs+n,nomatr])
        }
        # (d2+d1-d1 = d2 and d2+d1-d2 = d1) the more distant the less weight
        new[(i-1)*nexs+n,tgt] <- if (d1 == d2) (T[i,tgt]+T[kNNs[i,neig],tgt])/2 else (d2*T[i,tgt]+d1*T[kNNs[i,neig],tgt])/(d1+d2)
        
      }
    }
  }
  if(extra){
    count<-1
    for (i in idx){
    
      # select randomly one of the k NNs
      neig <- sample(1:k,1) 
      
      # the attribute values of the generated case
      difs <- T[kNNs[i,neig],-tgt]-T[i,-tgt]
      new[nexs*nT+count,-tgt] <- T[i,-tgt]+runif(1)*difs
      for(a in nomatr)
        new[nexs*nT+count,a] <- c(T[kNNs[i,neig],a],T[i,a])[1+round(runif(1),0)]
      
      
      # now the target value (weighted (by inverse distance) average)
      d1 <- d2 <- 0
      for(x in (1:nC)[-c(nomatr,tgt)]) {
        d1 <- abs(T[i,x] - new[nexs*nT+count,x])/ranges[x]
        d2 <- abs(T[kNNs[i,neig],x] - new[nexs*nT+count,x])/ranges[x]
      }
      if (length(nomatr)) {
        d1 <- d1 + sum(T[i,nomatr] != new[(i-1)*nexs+n,nomatr])
        d2 <- d2 + sum(T[kNNs[i,neig],nomatr] != new[(i-1)*nexs+n,nomatr])
      }
      # (d2+d1-d1 = d2 and d2+d1-d2 = d1) the more distant the less weight
      new[nexs*nT+count,tgt] <- if (d1 == d2) (T[i,tgt]+T[kNNs[i,neig],tgt])/2 else (d2*T[i,tgt]+d1*T[kNNs[i,neig],tgt])/(d1+d2)
    
      count <- count+1
    }
  }

  newCases <- data.frame(new)

  for(a in nomatr)
    newCases[,a] <- factor(newCases[,a],levels=1:nlevels(data[,a]),labels=levels(data[,a]))
  
  colnames(newCases) <- colnames(data)
  newCases
  
}



