## ===================================================
## Creating a SMOGN training sample for classification problems
## SMOGN strategy selectes the strategy that is applied for generating new synthetic 
## examples according to the distances between the examples. The two candidate 
## strategies for obtaining new examples are SmoteClassif and GaussNoiseClassif.
## The selection procedure takes into account the distances distribution. If the 
## two examples are too far away, then GaussNoise is selected, if the two examples
## are close then SmoteClassif is used for obtaining a new synthetic example.
## ===================================================
## P.Branco, Dec 2019
## ---------------------------------------------------
SMOGNClassif <- function(form, dat, C.perc = "balance",
                         k = 5, repl = FALSE, dist = "Euclidean",
                         p = 2, pert=0.01)
  
  # INPUTS:
  # form     a model formula
  # dat      the original training set (with the unbalanced distribution)
  # C.perc  named list containing each class percentage of under- or 
  #         over-sampling to apply between 0 and 1. The user may provide
  #         only a subset of the existing classes where sampling is to
  #         be applied. Alternatively it may be "balance" (the default) or
  #         "extreme", cases where the sampling percentages are automatically
  #         estimated.
  # k        is the number of neighbors to consider as the pool from where
  #          the new synthetic examples are generated when using SmoteR strategy
  # repl     is it allowed to perform sampling with replacement for 
  #          SmoteClassif Undersampling strategy
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
  names <- sort(unique(dat[, tgt]))
  li <- class.freq(dat, tgt)
  if (tgt < ncol(dat)) {
    orig.order <- colnames(dat)
    cols <- 1:ncol(dat)
    cols[c(tgt, ncol(dat))] <- cols[c(ncol(dat), tgt)]
    dat <- dat[, cols]
  }
  
  if (is.list(C.perc)) {
    names.und <- names(which(C.perc < 1))
    names.ove <- names(which(C.perc > 1))
    names.same <- setdiff(names, union(names.und, names.ove))
    
    # include examples from classes unchanged
    newdata <- dat[which(dat[, ncol(dat)] %in% names.same), ]
    
    if (length(names.und)) {  # perform under-sampling
      for (i in 1:length(names.und)) {
        Exs <- which(dat[, ncol(dat)] == names.und[i])
        sel <- sample(Exs,
                      as.integer(C.perc[[names.und[i]]] * length(Exs)),
                      replace = repl)
        newdata <- rbind(newdata, dat[sel, ])
      }
    }
    if (length(names.ove)) { # perform over-sampling
      for (i in 1:length(names.ove)) {
        if(length(which(dat[, ncol(dat)] == names.ove[i])) == 1){
          warning(paste("SmognClassif :: Unable to use SmognClassif in a bump with 1 example.
                        Introducing replicas of the example."), call.=FALSE)
          newdata <- rbind(newdata, dat[rep(which(dat[, ncol(dat)] == names.ove[i]),C.perc[names.ove[i]]),])
        } else if (length(which(dat[, ncol(dat)] == names.ove[i])) <= k){
          warning(paste("SmognClassif :: Nr of examples is less or equal to k.\n Using k =",
                        length(which(dat[, ncol(dat)] == names.ove[i]))-1, 
                        "in the nearest neighbours computation in this bump."), call.=FALSE)
          Origk <- k
          k <- length(which(dat[, ncol(dat)] == names.ove[i]))-1
          newExs <- Smogn.exsClassif(dat[which(dat[, ncol(dat)] == names.ove[i]), ],
                                     ncol(dat),
                                     li[[3]][ove[i]]/li[[2]][ove[i]] + 1,
                                     k,
                                     dist,
                                     p,
                                     pert)
          # add original rare examples and synthetic generated examples
          newdata <- rbind(newdata, newExs, 
                           dat[which(dat[,ncol(dat)] == names.ove[i]),])
          k <- Origk
        } else {
          newExs <- Smogn.exsClassif(dat[which(dat[, ncol(dat)] == names.ove[i]), ],
                                     ncol(dat),
                                     C.perc[[names.ove[i]]],
                                     k,
                                     dist,
                                     p,
                                     pert)
          # add original rare examples and synthetic generated examples
          newdata <- rbind(newdata, newExs,
                           dat[which(dat[, ncol(dat)] == names.ove[i]), ])
        }
      }
    }
  } else {
    if (C.perc == "balance") {  
      li[[3]] <- round(sum(li[[2]])/length(li[[2]]), 0) - li[[2]]
    } else if (C.perc == "extreme") {
      med <- sum(li[[2]])/length(li[[2]])
      li[[3]] <- round(med^2/li[[2]] * sum(li[[2]])/sum(med^2/li[[2]]), 0) - li[[2]]
    } else {
      stop("Please provide a list with classes to under-/over-sample
           or alternatively indicate 'balance' or 'extreme'.")
    }
    und <- which(li[[3]] < 0) # classes to under-sample
    ove <- which(li[[3]] > 0) #classes to over-sample
    same <- which(li[[3]] == 0) # unchanged classes
    
    # include examples from classes unchanged
    newdata <- dat[which(dat[, ncol(dat)] %in% li[[1]][same]), ]
    
    if (length(und)) { #perform under-sampling
      for (i in 1:length(und)) { 
        Exs <- which(dat[, ncol(dat)] == li[[1]][und[i]])
        sel <- sample(Exs,
                      as.integer(li[[2]][und[i]] + li[[3]][und[i]]),
                      replace = repl)
        newdata <- rbind(newdata, dat[sel, ])
      }
    }
    
    if (length(ove)) { #perform over-sampling
      for (i in 1:length(ove)) {
        if(length(which(dat[, ncol(dat)] == li[[1]][ove[i]])) == 1){
          warning(paste("SmognClassif :: Unable to use SmognClassif in a bump with 1 example.
                        Introducing replicas of the example."), call.=FALSE)
          newdata <- rbind(newdata, dat[rep(which(dat[, ncol(dat)] == li[[1]][ove[i]]), li[[3]][ove[i]]),])
        } else if(length(which(dat[, ncol(dat)] == li[[1]][ove[i]]))<= k){
          warning(paste("SmognClassif :: Nr of examples is less or equal to k.\n Using k =",
                        length(which(dat[, ncol(dat)] == li[[1]][ove[i]]))-1, 
                        "in the nearest neighbours computation in this bump."), call.=FALSE)
          Origk <- k
          k <- length(which(dat[, ncol(dat)] == li[[1]][ove[i]]))-1
          newExs <- Smogn.exsClassif(dat[which(dat[, ncol(dat)] == li[[1]][ove[i]]), ],
                                     ncol(dat),
                                     li[[3]][ove[i]]/li[[2]][ove[i]] + 1,
                                     k,
                                     dist,
                                     p,
                                     pert)
          # add original rare examples and synthetic generated examples
          newdata <- rbind(newdata, newExs, 
                           dat[which(dat[,ncol(dat)] == li[[1]][ove[i]]),])
          k <- Origk
        } else {
          newExs <- Smogn.exsClassif(dat[which(dat[, ncol(dat)] == li[[1]][ove[i]]), ],
                                     ncol(dat),
                                     li[[3]][ove[i]]/li[[2]][ove[i]] + 1,
                                     k,
                                     dist,
                                     p,
                                     pert)
          # add original rare examples and synthetic generated examples
          newdata <- rbind(newdata, newExs, 
                           dat[which(dat[,ncol(dat)] == li[[1]][ove[i]]),])
        }
      } 
    }
    
  }
  
  if (tgt < ncol(dat)) {
    newdata <- newdata[,cols]
    dat <- dat[,cols]
  }
  
  newdata
}


# ===================================================
# Obtain a set of smoted examples for a set of rare cases.
# P.Branco, Dec 2019
# ---------------------------------------------------
Smogn.exsClassif <- function(dat, tgt, N, k, dist, p, pert)
  # INPUTS:
  # dat   are the rare cases (the minority class cases)
  # tgt    is the name of the target variable
  # N      is the percentage of over-sampling to carry out;
  # k      is the number of nearest neighbors to use for the generation
  # dist   is the distance function to use for the neighbors computation
  # p      is an integer used when a "p-norm" distance is selected
  # OUTPUTS:
  # The result of the function is a (N-1)*nrow(dat) set of generated
  # examples with rare class on the target
{
  nomatr <- c()
  T <- matrix(nrow = dim(dat)[1], ncol = dim(dat)[2] - 1)
  for (col in seq.int(dim(T)[2])) { 
    if (class(dat[, col]) %in% c('factor', 'character')) {
      T[, col] <- as.integer(dat[, col])
      nomatr <- c(nomatr, col)
    } else {
      T[, col] <- dat[, col]
    }
  }
  nC <- dim(T)[2]
  nT <- dim(T)[1]
  
  # check if there is enough data to determine the k neighbors
  if (nT <= k) {
    stop("Trying to determine ",k, " neighbors for a subset with only ",
         nT, " examples")
  }
  
  kNNs <- neighbours(tgt, dat, dist, p, k)
  DM <- distances(tgt, dat, dist, p)
  maxDM <- apply(DM, 1, function(x){ # half the median of the distances in the line
    summary(x)[3]/2
  })
  
  
  nexs <-  as.integer(N - 1) # nr of examples to generate for each rare case
  extra <- as.integer(nT * (N - 1 - nexs)) # the extra examples to generate
  idx <- sample(1:nT, extra)
  newM <- matrix(nrow = nexs * nT + extra, ncol = nC)    # the new cases
  if (nexs) {
    for (i in 1:nT) {
      Slist <- which(DM[i,kNNs[i,]]<maxDM[i])
      for (n in 1:nexs) {
        # select randomly one of the k NNs
        neig <- sample(1:k, 1)
        if (neig %in% Slist){  ###### use SmoteClassif
        # the attribute values of the generated case
          difs <- T[kNNs[i, neig], ] - T[i, ]
          newM[(i - 1) * nexs + n, ] <- T[i, ] + runif(1) * difs
          for (a in nomatr) {
            # nominal attributes are randomly selected among the existing values
            # of seed and the selected neighbor 
            newM[(i - 1) * nexs + n, a] <- c(T[kNNs[i, neig], a], 
                                             T[i, a])[1 + round(runif(1), 0)]
          }
        } else { ####### use GaussNoise
          if(maxDM[i]>0.02){
            tpert <- 0.02
          } else {
            tpert <- maxDM[i]
          }
          
          idx.ex <- (i - 1) * nexs + n 
          for (num in 1:(nC)) {
            newM[idx.ex, num] <- T[i, num] + rnorm(1, 0, sd(T[, num]) * tpert)
            if (num %in% nomatr) {
              probs <- c()
              for (u in 1:length(unique(T[, num]))) {
                probs <- c(probs, length(which(T[, num] == unique(T[, num])[u])))
              }
              newM[idx.ex, num] <- sample(unique(T[, num]), 1, prob = probs)
            }
          }
          
      }
    }
    }
  }
  if (extra) {
    count <- 1
    for (i in idx) {    
      # select randomly one of the k NNs
      neig <- sample(1:k, 1)
      # the attribute values of the generated case
      difs <- T[kNNs[i, neig], ] - T[i, ]
      newM[nexs * nT + count, ] <- T[i, ] + runif(1) * difs
      for (a in nomatr) {
        newM[nexs * nT + count, a] <- c(T[kNNs[i, neig], a], 
                                        T[i, a])[1 + round(runif(1), 0)]
      }
      count <- count + 1
    }
  }
  newCases <- data.frame(newM)
  
  for (a in nomatr){
    newCases[, a] <- factor(newCases[, a],
                            levels = 1:nlevels(dat[, a]),
                            labels = levels(dat[, a]))
  }
  newCases[, tgt] <- factor(rep(dat[1, tgt], nrow(newCases)),
                            levels = levels(dat[, tgt]))
  colnames(newCases) <- colnames(dat)
  newCases
}

  