
## SMOGN strategy selectes the strategy that is applied for generating new synthetic 
## examples according to the distances between the examples. The two candidate 
## strategies for obtaining new examples are SmoteRegress and GaussNoiseRegress.
## The selection procedure takes into account the distances distribution. If the 
## two examples are too far away, then GaussNoise is selected, if the two examples
## are close then SmoteRegress is used for obtaining a new synthetic example.
## P. Branco Sept 2017


SMOGNRegress <- function(form, dat, rel = "auto", thr.rel = 0.5,
                        C.perc = "balance", k = 5, repl = FALSE,
                        dist = "Euclidean", p = 2, pert=0.01)
  
  # INPUTS:
  # form     a model formula
  # dat      the original training set (with the unbalanced distribution)
  # rel      the relevance function
  # thr.rel  is the relevance threshold above which a case is considered
  #          as belonging to the rare "class"
  # C.perc   is a list containing the percentage of under- or/and 
  #          over-sampling to apply to each "class" obtained with the threshold.
  #          The over-sampling percentage means that the examples above the 
  #          threshold are increased by this percentage. The under sampling
  #          percentage means that the normal cases (cases below the threshold)
  #          are under-sampled by this percentage. Alternatively it may be
  #          "balance" or "extreme", cases where the sampling percentages
  #          are automatically estimated.
  # k        is the number of neighbors to consider as the pool from where
  #          the new synthetic examples are generated when using SmoteR strategy
  # repl     is it allowed to perform sampling with replacement for SmoteR strategy
  # dist     is the distance measure to be used (defaults to "Euclidean") used
  #          for obtaining new examples through SmoteR strategy
  # p        is a parameter used when a p-norm is computed
  # pert     the perturbation to apply when using gaussNoise strategy
{
  
  if (any(is.na(dat))) {
    stop("The data set provided contains NA values!")
  }
  
  # the column where the target variable is
  tgt <- which(names(dat) == as.character(form[[2]]))
  
  if (tgt < ncol(dat)) {
    orig.order <- colnames(dat)
    cols <- 1:ncol(dat)
    cols[c(tgt, ncol(dat))] <- cols[c(ncol(dat), tgt)]
    dat <- dat[, cols]
  }
  if (is.na(thr.rel)) {
    stop("Future work!")
  }
  
  y <- dat[, ncol(dat)]
  attr(y, "names") <- rownames(dat)
  s.y <- sort(y)
  

  if (is.matrix(rel)) {
    pc <- phi.control(y, method = "range", control.pts = rel)
  } else if (is.list(rel)) { 
    pc <- rel
  } else if (rel == "auto") {
    pc <- phi.control(y, method = "extremes")
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
  
  newdata <- data.frame()
  
  if (is.list(C.perc)) {
    if (length(C.perc) != nbump){
      stop("The percentages provided must be the same length as the number
           of bumps!")
    }
  } else if (C.perc == "balance") {
    # estimate the percentages of over/under sampling
    B <- round(nrow(dat)/nbump, 0)
    C.perc <- B/sapply(obs.ind, length)        
  } else if (C.perc == "extreme") {
    B <- round(nrow(dat)/nbump, 0)
    rescale <- nbump * B/sum(B^2/sapply(obs.ind, length))
    obj <- round((B^2/sapply(obs.ind, length)) * rescale, 2)
    C.perc <- round(obj/sapply(obs.ind, length), 1)
  }
  
  for (i in 1:nbump) {
    if (C.perc[[i]] == 1) {
      newdata <- rbind(newdata, dat[names(obs.ind[[i]]), ])
    } else if (C.perc[[i]] > 1) {
      newExs <- SMOGNRegress.exs(dat, names(obs.ind[[i]]), ncol(dat), C.perc[[i]],
                                k, dist, p, pert)
      # add original rare examples and synthetic generated examples
      newdata <- rbind(newdata, newExs, dat[names(obs.ind[[i]]), ])
    } else if (C.perc[[i]] < 1) {
      sel.maj <- sample(1:length(obs.ind[[i]]),
                        as.integer(C.perc[[i]] * length(obs.ind[[i]])),
                        replace = repl)
      newdata <- rbind(newdata, dat[names(obs.ind[[i]][sel.maj]), ])
    }
  }
  if (tgt < ncol(dat)) {
    newdata <- newdata[, cols]
    dat <- dat[, cols]
  }
  newdata
}



SMOGNRegress.exs <- function(orig, ind, tgt, N, k, dist, p, pert)
  # INPUTS:
  # orig the original data set with rare and normal cases
  # ind are character indexes of the rare cases (the minority "class" cases)
  # tgt the column nr of the target variable
  # N is the percentage of over-sampling to carry out;
  # k is the number of nearest neighours
  # dist is the distance function used for the neighours computation
  # p is an integer used when a "p-norm" distance is selected
  # pert is the perturbation introduced when applying GaussNoiseRegress strategy 
  # OUTPUTS:
  # The result of the function is a (N-1)*nrow(dat) set of generate
  # examples with rare values on the target
{
  indpos <- match(ind, rownames(orig))
  dat <- orig[indpos,]
  
  ConstFeat <- which(apply(dat, 2, function(col){length(unique(col)) == 1}))
  
  if(length(ConstFeat)){
    badds <- dat
    ConstRes <- dat[1,ConstFeat]
    dat <- dat[,apply(dat, 2, function(col) { length(unique(col)) > 1 })]
    tgt <- ncol(dat)
  }
  
  nomatr <- c()
  T <- matrix(nrow = dim(dat)[1], ncol = dim(dat)[2])
  for (col in seq.int(dim(T)[2])){
    if (class(dat[, col]) %in% c('factor', 'character')) {
      T[, col] <- as.integer(dat[, col])
      nomatr <- c(nomatr, col)
    } else {
      T[, col] <- dat[, col]
    }
  }
  nC <- dim(T)[2]
  nT <- dim(T)[1]
  
  
  ranges <- rep(1, nC)
  if (length(nomatr)) {
    for (x in (1:nC)[-c(nomatr)]) {
      ranges[x] <- max(T[, x]) - min(T[, x])
    }
  } else {
    for(x in (1:nC)) {
      ranges[x] <- max(T[, x]) - min(T[, x])
    }
  }
  
  # test that k is possible to use!
  # if(nrow(dat)<k+1){ 
  #   warning(paste("Unable to compute", k,"neighbours in this bump. Using",
  #                 nrow(dat)-1, "for kNN computation."), call.=FALSE)
  kNNs <- neighbours(tgt, dat, dist, p, k)
  DM <- distances(tgt, dat, dist, p)
  maxDM <- apply(DM, 1, function(x){ # half the median of the distances in the line
    summary(x)[3]/2
  })
  
  nexs <- as.integer(N - 1) # nr of examples to generate for each rare case
  extra <- as.integer(nT * (N - 1 - nexs)) # the extra examples to generate
  idx <- sample(1:nT, extra)
  newM <- matrix(nrow = nexs * nT + extra, ncol = nC)    # the new cases
  
  if (nexs) {
    for (i in 1:nT) {
      Slist <- which(DM[i,kNNs[i,]]<maxDM[i])
      for (n in 1:nexs) {
        # select randomly one of the k NNs
        neig <- sample(1:k, 1)
        if (neig %in% Slist){  ###### use SmoteR
          # the attribute values of the generated case
          difs <- T[kNNs[i, neig], -tgt] - T[i, -tgt]
          newM[(i - 1) * nexs + n, -tgt] <- T[i, -tgt] + runif(1) * difs
          for (a in nomatr) {
            # nominal attributes are randomly selected among the existing
            # values of seed and the selected neighbour 
            newM[(i - 1) * nexs + n, a] <- c(T[kNNs[i, neig], a],
                                             T[i, a])[1 + round(runif(1), 0)]
          }
          # now the target value (weighted (by inverse distance) average)
          d1 <- d2 <- 0
          for (x in (1:nC)[-c(nomatr, tgt)]) {
            d1 <- abs(T[i, x] - newM[(i - 1) * nexs + n, x])/ranges[x]
            d2 <- abs(T[kNNs[i, neig], x] - newM[(i - 1) * nexs + n, x])/ranges[x]
          }
          if (length(nomatr)) {
            d1 <- d1 + sum(T[i, nomatr] != newM[(i - 1) * nexs + n, nomatr])
            d2 <- d2 + 
              sum(T[kNNs[i, neig], nomatr] != newM[(i - 1) * nexs + n, nomatr])
          }
          # (d2+d1-d1 = d2 and d2+d1-d2 = d1) the more distant the less weight
          if (d1 == d2) {
            newM[(i - 1) * nexs + n, tgt] <- (T[i, tgt] + T[kNNs[i, neig], tgt])/2
          } else {
            newM[(i - 1) * nexs + n, tgt] <- (d2 * T[i, tgt] + 
                                                d1 * T[kNNs[i, neig], tgt])/(d1 + d2)
          }
        } else { ####### use GaussNoise
          if(maxDM[i]>0.02){
            tpert <- 0.02
          } else {
            tpert <- maxDM[i]
          }
          id.ex <- (i - 1) * nexs + n 
          for (num in 1:nC) {
            if (is.na(T[i, num])) {
              newM[id.ex, num] <- NA
            } else {
              newM[id.ex, num] <- T[i, num] + rnorm(1, 0, sd(T[, num], 
                                                             na.rm = TRUE) * tpert)
              if (num %in% nomatr) {
                probs <- c()
                if (length(unique(T[, num])) == 1) {
                  newM[id.ex, num] <- T[1, num]
                } else {
                  for (u in 1:length(unique(T[, num]))) {
                    probs <- c(probs, 
                               length(which(T[, num] == unique(T[, num])[u])))
                  }
                  newM[id.ex, num] <- sample(unique(T[, num]), 1, prob = probs)
                }
              }
            }
          }
          
        }
      }
    }
  }
  
  if (extra) {
    count <- 1
    for (i in idx) {
      Slist <- which(DM[i,kNNs[i,]]<maxDM[i])
      # select randomly one of the k NNs
      neig <- sample(1:k, 1) 
      if (neig %in% Slist){  ###### use SmoteR
        # the attribute values of the generated case
        difs <- T[kNNs[i, neig], -tgt] - T[i, -tgt]
        newM[nexs * nT + count, -tgt] <- T[i, -tgt] + runif(1) * difs
        for (a in nomatr) {
          newM[nexs * nT + count, a] <- c(T[kNNs[i,neig], a], 
                                          T[i, a])[1 + round(runif(1), 0)]
        }
        
        # now the target value (weighted (by inverse distance) average)
        d1 <- d2 <- 0
        for (x in (1:nC)[-c(nomatr,tgt)]) {
          d1 <- abs(T[i, x] - newM[nexs * nT + count, x])/ranges[x]
          d2 <- abs(T[kNNs[i, neig], x] - newM[nexs * nT + count, x])/ranges[x]
        }
        if (length(nomatr)) {
          d1 <- d1 + sum(T[i,nomatr] != newM[nexs *nT + count, nomatr])
          d2 <- d2 + 
            sum(T[kNNs[i, neig], nomatr] != newM[nexs * nT + count, nomatr])
        }
        # (d2+d1-d1 = d2 and d2+d1-d2 = d1) the more distant the less weight
        if (d1 == d2) {
          newM[nexs * nT + count, tgt] <- (T[i, tgt] + T[kNNs[i, neig], tgt])/2
        } else {
          newM[nexs * nT + count, tgt] <- (d2 * T[i, tgt] + 
                                             d1 * T[kNNs[i, neig],tgt])/(d1 + d2)
        }
      } else { ########### use GaussNoise
        if(maxDM[i]>0.02){
          tpert <- 0.02
        } else {
          tpert <- maxDM[i]
        }
        for (num in 1:nC) {
          if (is.na(T[i, num])) {
            newM[nexs * nT + count, num] <- NA
          } else {
            newM[nexs * nT + count, num] <- T[i, num] + rnorm(1, 0, sd(T[, num],
                                                                       na.rm = TRUE) * tpert)
            if (num %in% nomatr) {
              probs <- c()
              if (length(unique(T[, num])) == 1) {
                newM[nexs * nT + count, num] <- T[1, num]
              } else {
                for (u in 1:length(unique(T[, num]))) {
                  probs <- c(probs,
                             length(which(T[, num] == unique(T[, num])[u])))
                }
                newM[nexs * nT + count, num] <- sample(unique(T[, num]),
                                                       1, prob = probs)
              }
            }
          }
        }
      }
      count <- count + 1
    }
  }
  
  newCases <- data.frame(newM)
  for (a in nomatr) {
    newCases[, a] <- factor(newCases[, a],
                            levels = 1:nlevels(dat[, a]),
                            labels = levels(dat[, a]))
  }
  
  if(length(ConstFeat)){ # add constant features that were removed in the beginning
    
    newCases <- cbind(newCases, 
                      as.data.frame(lapply(ConstRes,
                                           function(x){rep(x, nrow(newCases))})))
    colnames(newCases) <- c(colnames(dat), names(ConstFeat))
    newCases <- newCases[colnames(badds)]
    
  } else {
    colnames(newCases) <- colnames(dat)
  }
  newCases
}

