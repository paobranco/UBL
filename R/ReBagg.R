
##########################################################################
## New Bagging Strategies for imbalanced regression : RE(sampled)BAGG(ing)
## P. Branco Dez 2017
##########################################################################

ReBagg <- function(form, train, rel="auto", thr.rel, learner, learner.pars,
                   nmodels = "auto", samp.method = "nbalance", aggregation="Average",
                   quiet=TRUE){
  # INPUTS:
  # form    a model formula
  # train:  the original training set (with the unbalanced distribution)
  # rel:     is the relevance determined automatically (default: "auto") 
  #           or provided by the user through a matrix. See examples.
  # thr.rel: is the relevance threshold above which a case is considered
  #           as belonging to the rare "class", i.e., a relevant case
  # learner: character with the learner function (the packages required must be
  #           previously loaded by the user)
  # learner.pars: named list containing the learner parameteres that are
  #           passed to the learner function
  # nmodels: either a character specifying "auto" which evaluates automatically the
  #           number of models to use, or an numeric specifying directly the number
  #           of models to train. The automatic method keeps the nmodels to minimum
  #           which depends on the total number of rare cases provided.
  # samp.method: character specifying the method used for building the resamples
  #           of the training set provided. 
  #           Possible characters are: "balance", "nbalance", "variation" and "nvariation".
  #           The "balance" methods builds a number(nmodels) of samples that use all 
  #           the rare cases and the same nr of normal cases. The "variation" method
  #           build a number of baggs with all the rare cases and varying percentages of normal cases.
  #           Both methods allow the prefix "n", which means that the samples are
  #           build to have the same size as the training set by allowing replicas of examples.
  #           In this case, after the baggs are determined their size is increased
  #           by using sampling with replacement. If the prefix n is not used, then
  #           the number examples in each bagg is set to a minimum which depends on the
  #           total number of relevant cases.  Defaults to "nbalance".
  # aggregation: charater specifying the method used for aggregating the results
  #           obtained by the individual learners. For now, only average is available
  
  #
  # OUTPUTS:
  # an object of class BagModel that contains all the information regarding the learned models 
  #
  
  
  if (any(is.na(train))) {
    stop("The data set provided contains NA values!")
  }
  
  # the column where the target variable is
  tgt <- which(names(train) == as.character(form[[2]]))
  if (tgt < ncol(train)) {
    orig.order <- colnames(train)
    cols <- 1:ncol(train)
    cols[c(tgt, ncol(train))] <- cols[c(ncol(train), tgt)]
    train <- train[, cols]
  }
  if (is.na(thr.rel)) {
    stop("Future work!")
  }
  
  
  y <- train[, ncol(train)]
  attr(y, "names") <- rownames(train)
  s.y <- sort(y)
  
  if (is.matrix(rel)) { 
    pc <- phi.control(y, method = "range", control.pts = rel)
  } else if (is.list(rel)) { 
    pc <- rel
    rel <- matrix(pc$control.pts, ncol=3, byrow=TRUE)
  } else if (rel == "auto") {
    pc <- phi.control(y, method = "extremes")
    rel <- matrix(pc$control.pts, ncol=3, byrow=TRUE)
  } else {# handle other relevance functions and not using the threshold!
    stop("future work!")
  }
  
  temp <- y.relev <- phi(s.y, pc)
  if (!length(which(temp < 1))) {
    stop("All the points have relevance 1. 
         Please, redefine your relevance function!")
  }
  if (!length(which(temp > 0))) {
    stop("All the points have relevance 0. 
         Please, redefine your relevance function!")
  }
  
  bumps <- c()
  for (i in 1:(length(y) - 1)) { 
    if ((temp[i] >= thr.rel && temp[i+1] < thr.rel) || 
        (temp[i] < thr.rel && temp[i+1] >= thr.rel)) {
      bumps <- c(bumps, i)
    }
  }
  nbump <- length(bumps) + 1 # number of different "classes"
  
  # collect the indexes in each "class"
  obs.ind <- as.list(rep(NA, nbump))
  last <- 1
  for (i in 1:length(bumps)) {
    obs.ind[[i]] <- s.y[last:bumps[i]]
    last <- bumps[i] + 1
  }
  obs.ind[[nbump]] <- s.y[last:length(s.y)]
  
  y.phi <- as.list(rep(NA, nbump))
  for(i in 1: nbump){
    y.phi[[i]] <- mean(phi(obs.ind[[i]], pc))
  }
  
  n <- nrow(train)
  
  if(nmodels == "auto"){
    # nmodels <- |D_N|%/%|D_R|
    nmodels <- sum(sapply(obs.ind[which(y.phi<=thr.rel)], length))%/%sum(sapply(obs.ind[which(y.phi>thr.rel)], length))
  
  }
  
  ind.rare <- match(unlist(sapply(obs.ind[which(y.phi>thr.rel)], names)), rownames(train))

  ind.normal <- match(unlist(sapply(obs.ind[which(y.phi<=thr.rel)], names)), rownames(train))
  
    
  if(samp.method == "balance"){
    # DS is a first list for each subsample containing only the rare cases
    DS <- lapply(seq_len(nmodels), function(o){ 
      train[ind.rare,]
    })
    
    
    # nr. normal cases in each subsample (except for the last one)
    N <- sum(sapply(obs.ind[which(y.phi<=thr.rel)], length))%/%nmodels
    # nr. rare cases in each subsample
    R <- sum(sapply(obs.ind[which(y.phi>thr.rel)], length))
    DS[1:(nmodels-1)] <- lapply(seq_len(nmodels-1), function(o) {
      rbind(DS[[o]], train[ind.normal[((o-1)*N+1):(o*N)],])
    })
    DS[[nmodels]] <-  rbind(DS[[nmodels]], train[((nmodels-1)*N+1):(length(ind.normal)),])
    
  } else if(samp.method == "nbalance"){
    
    # tgt nr of examples for both minority and majority "class": 
    tgtNr <- nrow(train)%/%2
    # DS is a first list for each subsample containing only the rare cases
    DS <- lapply(seq_len(nmodels), function(o){ 
      s <- sample(ind.rare, tgtNr-length(ind.rare), replace=TRUE)
      train[c(ind.rare, s),]
    })
    
    # nr. normal cases in each subsample (except for the last one)
    N <- sum(sapply(obs.ind[which(y.phi<=thr.rel)], length))%/%nmodels
    rem <- sum(sapply(obs.ind[which(y.phi<=thr.rel)], length))%%nmodels
    # nr. rare cases in each subsample
    R <- sum(sapply(obs.ind[which(y.phi>thr.rel)], length))
    DS[1:(nmodels-1)] <- lapply(seq_len(nmodels-1), function(o) {
      s <- sample(ind.normal, tgtNr-N,replace=TRUE)
      rbind(DS[[o]], train[ind.normal[((o-1)*N+1):(o*N)],], train[s,])
    })
    s <- sample(ind.normal, tgtNr-(N+rem),replace=TRUE)
    DS[[nmodels]] <-  rbind(DS[[nmodels]], train[((nmodels-1)*N+1):(length(ind.normal)),], train[s,])
    
  } else if (samp.method == "variation"){
    # DS is a first list for each subsample containing only the rare cases
    DS <- lapply(seq_len(nmodels), function(o){ 
      train[ind.rare,]
    })
    
    # nr. normal cases in each subsample (except for the last one)
    N <- sum(sapply(obs.ind[which(y.phi<=thr.rel)], length))%/%nmodels
    # nr. rare cases in each subsample
    R <- sum(sapply(obs.ind[which(y.phi>thr.rel)], length))
    NL <- NULL
    for(i in 1:nmodels){
      k=nmodels-i+1
      if(i<k){
    NL[i] <- N-round((N/(i+1)),0)
    NL[k] <- N+round((N/(i+1)),0)
      }
      if(i == k){
        NL[i] <- N
      }
    }
    steps <- c(0, cumsum(NL)[-nmodels], length(ind.normal))
    
    DS <- lapply(seq_len(nmodels), function(o) {
      rbind(DS[[o]], train[ind.normal[(steps[o]+1):(steps[o+1])],])
    })

  } else if (samp.method == "nvariation"){
    # DS is a first list for each subsample containing only the rare cases
    DS <- lapply(seq_len(nmodels), function(o){ 
      train[ind.rare,]
    })
    
    # nr. normal cases in each subsample (except for the last one)
    N <- sum(sapply(obs.ind[which(y.phi<=thr.rel)], length))%/%nmodels
    # nr. rare cases in each subsample
    R <- sum(sapply(obs.ind[which(y.phi>thr.rel)], length))
    # tgtNr was previously calculated. Is stored in variable n
    
    NL <- NULL
    for(i in 1:nmodels){
      k=nmodels-i+1
      if(i<k){
        NL[i] <- N-round((N/(i+1)),0)
        NL[k] <- N+round((N/(i+1)),0)
      }
      if(i == k){
        NL[i] <- N
      }
    }
    steps <- c(0, cumsum(NL)[-nmodels], length(ind.normal))
    
    DS <- lapply(seq_len(nmodels), function(o) {
      rbind(DS[[o]], train[ind.normal[(steps[o]+1):(steps[o+1])],])
    })
    
    # increase rare and normal cases to obtain subsets with size n
    BagT <- sapply(DS, nrow)
    
    TgtRinc <- sapply(BagT, function(o){
      round(R*n/o,0)-R
      }) 
    # increase each subset with copies of rare cases proportionally
    DS <- lapply(seq_len(nmodels), function(o) {
      s <- sample(ind.rare, TgtRinc[o], replace=TRUE)
      rbind(DS[[o]], train[s,])
    })
    
    # increase each subset with copies of normal cases proportionally
        
    TgtNinc <- sapply(seq_len(nmodels), function(o){
      round(NL[o]*n/BagT[o],0)-NL[o]
      })    

    DS <- lapply(seq_len(nmodels), function(o) {
      s <- sample(ind.normal, TgtNinc[o], replace=TRUE)
      rbind(DS[[o]], train[s,])
    })
  } 
  
  
  BL <- lapply(seq_len(nmodels), function(o){
    do.call(learner, c(list(form, DS[[o]]), learner.pars))
    })
  
  names(BL) <- paste0("M", seq_along(BL))

  if (tgt < ncol(train)) {
    train <- train[, cols]
  }
  
  BagModel(form, train, learner = learner, learner.pars = learner.pars, baseModels = BL,
           aggregation = aggregation, rel = rel, thr.rel = thr.rel, quiet=quiet)
  
  }