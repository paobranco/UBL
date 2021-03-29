##########################################################################
## New Bagging Strategies for imbalanced regression : RE(sampled)BAGG(ing)
## P. Branco Dez 2017
##########################################################################


ReBaggRegress <- function(form, train, rel="auto", thr.rel, learner, learner.pars,
                   nmodels , samp.method= "variationSMT", aggregation="Average",
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
  # nmodels: a numeric specifying the number
  #           of models to train. 
  # samp.method: character specifying the method used for building the resamples
  #           of the training set provided. 
  #           Possible characters are: "balance", "variation", "balanceSMT", "variationSMT", .
  #           The "balance" methods builds a number(nmodels) of samples that use all 
  #           the rare cases and the same nr of normal cases. The "variation" method
  #           build a number of baggs with all the rare cases and varying percentages of normal cases.
  #           Defaults to "variationSMT".
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
  
  ind.rare <- match(unlist(sapply(obs.ind[which(y.phi>thr.rel)], names)), rownames(train))
  
  ind.normal <- match(unlist(sapply(obs.ind[which(y.phi<=thr.rel)], names)), rownames(train))
  
  #shuffle rare and normal indexes
  ind.rare <- sample(ind.rare)
  ind.normal <- sample(ind.normal)
  LR <- length(ind.rare)
  LN <- length(ind.normal)
  
  if (samp.method == "balance"){
    DS <- lapply(seq_len(nmodels), function(o){ 
      s1 <- sample(ind.rare, round(n/2,0), replace = TRUE)
      s2 <- sample(ind.normal, round(n/2,0), replace = TRUE)
      train[c(s1, s2),]
    })
    
  } else if (samp.method == "variation"){
    NR <- round(c(n/3, 2*n/5, n/2, 3*n/5, 2*n/3),0)
    DS <- lapply(seq_len(nmodels), function(o){
      p <- sample(NR,1)
      s1 <- sample(ind.rare, p, replace = TRUE)
      s2 <- sample(ind.normal, (n-p), replace = TRUE)
      train[c(s1, s2),]
    })
    
  } else if (samp.method == "balanceSMT"){
      # nr of rare cases necessary to generate with SMOTE 
      TgtR <- round((n/2)-LR, 0)
      N <- (TgtR+LR)/LR
      DS <- lapply(seq_len(nmodels), function(o){ 
        SMT <- Smote.exsRegress(train[ind.rare,], ncol(train), N, k=5, dist="HEOM", p=2)
        s2 <- sample(ind.normal, round(n/2,0), replace = TRUE)
        rbind(train[c(ind.rare, s2),], SMT)
      })
      
    } else if (samp.method == "variationSMT"){
      NR <- round(c(n/3, 2*n/5, n/2, 3*n/5, 2*n/3),0)
      DS <- lapply(seq_len(nmodels), function(o){
        p <- sample(NR,1)
        
        if (p<=LR){
          s1 <- sample(ind.rare, p)
          s2 <- sample(ind.normal, (n-p), replace = TRUE)
          train[c(s1, s2),]
        } else if (p>LR){
          #"percentage" of rare cases needed
          N <- p/LR
          SMT <- Smote.exsRegress(train[ind.rare,], ncol(train), N, k=5, dist="HEOM", p=2)
          s2 <- sample(ind.normal, (n-p), replace = TRUE)
          rbind(train[c(ind.rare, s2),], SMT)
        }
      })
    }
    
      BL <- lapply(seq_len(nmodels), function(o){
        do.call(learner, c(list(form, DS[[o]]), learner.pars))
      })

    names(BL) <- paste0("M", seq_along(BL))
    
    if (tgt < ncol(train)) {
      train <- train[, cols]
    }
    
    BagModel(form, train, learner = learner, learner.pars= learner.pars, baseModels = BL,
             aggregation = aggregation, rel = rel, thr.rel = thr.rel, quiet=quiet)
  
  }